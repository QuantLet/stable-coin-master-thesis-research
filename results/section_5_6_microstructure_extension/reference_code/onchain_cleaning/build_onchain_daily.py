"""Rebuild daily on-chain covariates for the stablecoin thesis.

Dependencies: pandas, numpy, pyarrow. The released Parquet files must first be
extracted from the Curve and Uniswap ZIP assets named in README.md.
"""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path

import numpy as np
import pandas as pd


START = pd.Timestamp("2020-01-01", tz="UTC")
END_EXCLUSIVE = pd.Timestamp("2026-06-01", tz="UTC")
FEE_TIER = 100  # Uniswap v3 0.01%; keep the pool definition constant.


def file_hash(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def date_key(timestamps: pd.Series | pd.DatetimeIndex) -> pd.Series | pd.DatetimeIndex:
    if isinstance(timestamps, pd.Series):
        return pd.to_datetime(timestamps, utc=True).dt.tz_localize(None).dt.normalize()
    return pd.DatetimeIndex(timestamps).tz_convert("UTC").tz_localize(None).normalize()


def daily_last(series: pd.Series, day: pd.Series | pd.DatetimeIndex) -> pd.Series:
    """Last actual (nonmissing) hourly snapshot within each UTC day."""
    return series.groupby(day).last()


def prepare_curve(root: Path, panel: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    curve_path = root / "Curve" / "curve_3pool_hourly.parquet"
    event_path = root / "Curve" / "3CRV_swapevents.parquet"
    curve = pd.read_parquet(curve_path)
    raw_index = pd.DatetimeIndex(curve.index)
    if raw_index.tz is None:
        raw_index = raw_index.tz_localize("UTC")
    else:
        raw_index = raw_index.tz_convert("UTC")
    if raw_index.has_duplicates or not raw_index.is_monotonic_increasing:
        raise ValueError("Curve snapshots must have unique, sorted UTC hours")

    # The upstream code ceils timestamps to the NEXT whole hour. Matching its
    # snapshots to transaction timestamps identifies the represented hour as H-1.
    curve.index = raw_index - pd.Timedelta(hours=1)
    curve = curve[(curve.index >= START) & (curve.index < END_EXCLUSIVE)].copy()
    if not (curve[["w_DAI", "w_USDC", "w_USDT"]].sum(axis=1) - 1).abs().lt(1e-8).all():
        raise ValueError("Curve 3pool weights do not sum to one")
    if curve["totalValueLockedUSD"].le(0).any():
        raise ValueError("Nonpositive Curve TVL")

    event_cols = ["datetime", "transaction_hash", "log_index", "sold_symbol", "tokens_sold"]
    events = pd.read_parquet(event_path, columns=event_cols)
    events["datetime"] = pd.to_datetime(events["datetime"], utc=True)
    events = events[(events.datetime >= START) & (events.datetime < END_EXCLUSIVE)].copy()
    if events.duplicated(["transaction_hash", "log_index"]).any():
        raise ValueError("Duplicate Curve transaction log IDs")
    if events.tokens_sold.isna().any() or events.tokens_sold.lt(0).any():
        raise ValueError("Invalid Curve swap amounts")
    if not set(events.sold_symbol.dropna()).issubset({"DAI", "USDC", "USDT"}):
        raise ValueError("Unexpected Curve 3pool sold token")
    events["day"] = date_key(events.datetime)
    events["hour"] = events.datetime.dt.floor("h")

    price_long = panel.melt(
        id_vars="date",
        value_vars=["dai", "usd_coin", "tether"],
        var_name="price_column", value_name="usd_price_at_daily_close",
    )
    price_long["sold_symbol"] = price_long.price_column.map(
        {"dai": "DAI", "usd_coin": "USDC", "tether": "USDT"}
    )
    events = events.merge(
        price_long[["date", "sold_symbol", "usd_price_at_daily_close"]],
        how="left", left_on=["day", "sold_symbol"], right_on=["date", "sold_symbol"],
        validate="many_to_one",
    )
    events["usd_estimate"] = events.tokens_sold * events.usd_price_at_daily_close
    event_day = events.groupby("day").agg(
        curve_3pool_swap_count=("tokens_sold", "size"),
        curve_3pool_swap_notional_units=("tokens_sold", "sum"),
        curve_3pool_swap_volume_usd_est=("usd_estimate", lambda s: s.sum(min_count=len(s))),
        price_unmatched=("usd_price_at_daily_close", lambda s: int(s.isna().sum())),
    )
    event_hour = events.groupby("hour").size().reindex(curve.index, fill_value=0)
    anomalous_source_volume = curve.hourlyVolumeUSD.gt(0) & event_hour.eq(0)
    nominal_hour = events.groupby("hour").tokens_sold.sum()
    aligned = nominal_hour.reindex(curve.index, fill_value=0)
    unshifted = nominal_hour.reindex(curve.index + pd.Timedelta(hours=1), fill_value=0)
    alignment_corr = np.log1p(curve.hourlyVolumeUSD).corr(np.log1p(aligned))
    unshifted_corr = np.log1p(curve.hourlyVolumeUSD).corr(
        pd.Series(np.log1p(unshifted.to_numpy()), index=curve.index)
    )
    if not np.isfinite(alignment_corr) or alignment_corr < 0.95:
        raise ValueError("Curve H-1 event alignment is not supported by source data")

    curve["day"] = date_key(curve.index)
    grouped = curve.groupby("day", sort=True)
    daily = pd.DataFrame(index=grouped.size().index)
    daily["curve_3pool_hours"] = grouped.size()
    daily["curve_3pool_tvl_usd_eod"] = daily_last(curve.totalValueLockedUSD, curve.day)
    daily["curve_3pool_dai_share_eod"] = daily_last(curve.w_DAI, curve.day)
    imbalance = 0.5 * (
        (curve.w_DAI - 1 / 3).abs()
        + (curve.w_USDC - 1 / 3).abs()
        + (curve.w_USDT - 1 / 3).abs()
    )
    daily["curve_3pool_imbalance_eod"] = daily_last(imbalance, curve.day)
    daily = daily.join(event_day.drop(columns="price_unmatched"))
    full_day = daily.curve_3pool_hours.eq(24)
    # No on-chain swaps during an otherwise fully observed day means zero,
    # unlike no source observations, which remain unavailable.
    for col in ["curve_3pool_swap_count", "curve_3pool_swap_notional_units",
                "curve_3pool_swap_volume_usd_est"]:
        daily.loc[full_day, col] = daily.loc[full_day, col].fillna(0)
    data_cols = [c for c in daily if c != "curve_3pool_hours"]
    daily.loc[~full_day, data_cols] = np.nan

    audit = {
        "curve_original_first_timestamp_utc": str(raw_index.min()),
        "curve_actual_first_hour_utc": str(curve.index.min()),
        "curve_hourly_volume_positive_with_no_swap_after_shift": int(anomalous_source_volume.sum()),
        "curve_shifted_log_volume_correlation": float(alignment_corr),
        "curve_unshifted_log_volume_correlation": float(unshifted_corr),
        "curve_swap_events_missing_price": int(events.usd_price_at_daily_close.isna().sum()),
        "curve_events_used": len(events),
        "curve_full_24h_days": int(full_day.sum()),
        "curve_full_days_with_missing_price": int((event_day.price_unmatched > 0).sum()),
        "curve_parquet_sha256": file_hash(curve_path),
        "curve_events_parquet_sha256": file_hash(event_path),
    }
    return daily, audit


def prepare_uniswap_pool(root: Path, pair: str) -> tuple[pd.DataFrame, dict]:
    path = root / "Uniswap" / f"{pair}_hourly_metrics.parquet"
    raw = pd.read_parquet(path)
    raw["datetime"] = pd.to_datetime(raw.datetime, utc=True)
    pool = raw[
        raw.feeTier.eq(FEE_TIER)
        & raw.datetime.ge(START)
        & raw.datetime.lt(END_EXCLUSIVE)
    ].copy().sort_values("datetime")
    if pool.datetime.duplicated().any() or pool.pool.nunique() != 1:
        raise ValueError(f"Duplicate hours or mixed pools in {pair} 1bp data")
    if pool.net_amountUSD.lt(0).any() or pool.swap_count.lt(0).any():
        raise ValueError(f"Invalid event-based Uniswap swaps in {pair}")
    pool["day"] = date_key(pool.datetime)
    grouped = pool.groupby("day")
    key = pair.lower()
    cols = {
        "hours": f"uni_{key}_hours",
        "tvl": f"uni_{key}_tvl_usd_eod",
        "volume": f"uni_{key}_swap_volume_usd",
        "count": f"uni_{key}_swap_count",
    }
    daily = pd.DataFrame(index=grouped.size().index)
    daily[cols["hours"]] = grouped.size()
    daily[cols["volume"]] = grouped.net_amountUSD.sum(min_count=24)
    daily[cols["count"]] = grouped.swap_count.sum(min_count=24)
    daily[cols["tvl"]] = daily_last(pool.tvlUSD, pool.day)
    # A stock should not be carried from an earlier day or from a stale hour.
    observed = pool.loc[pool.tvlUSD.notna(), ["day", "datetime"]]
    latest = observed.groupby("day").datetime.max()
    stale = (latest.dt.hour < 20).reindex(daily.index, fill_value=True)
    daily.loc[stale, cols["tvl"]] = np.nan
    full_day = daily[cols["hours"]].eq(24)
    daily.loc[~full_day, [cols["tvl"], cols["volume"], cols["count"]]] = np.nan
    audit = {
        f"uni_{key}_pool_id": pool.pool.iloc[0],
        f"uni_{key}_fee_tier": FEE_TIER,
        f"uni_{key}_full_24h_days": int(full_day.sum()),
        f"uni_{key}_days_without_recent_tvl": int(stale.sum()),
        f"uni_{key}_parquet_sha256": file_hash(path),
    }
    return daily, audit


def prepare_uniswap_liquidity(root: Path) -> tuple[pd.DataFrame, dict]:
    path = root / "Uniswap" / "hourly_liquidity_pricecentered_full.parquet"
    bars = pd.read_parquet(
        path, columns=["hour", "poolTick", "tickLower", "usd_total", "active_liquidity_L"]
    )
    bars["hour"] = pd.to_datetime(bars.hour, utc=True)
    bars = bars[bars.hour.ge(START) & bars.hour.lt(END_EXCLUSIVE)].copy()
    if bars.usd_total.lt(-1e-8).any() or bars.active_liquidity_L.lt(0).any():
        raise ValueError("Negative Uniswap liquidity bucket")
    bar_counts = bars.groupby("hour").size()
    if not bar_counts.eq(101).all():
        raise ValueError("Uniswap hourly liquidity snapshot is not 101 tick buckets")
    if bars.duplicated(["hour", "tickLower"]).any():
        raise ValueError("Duplicate Uniswap liquidity tick intervals")
    bars["day"] = date_key(bars.hour)
    tick_distance = (bars.tickLower - bars.poolTick).abs()
    hourly = pd.DataFrame(index=bar_counts.index)
    for n in (10, 25, 50):
        column = f"uni_usdc_usdt_inventory_pm{n}ticks_usd_eod"
        bucket = bars.usd_total.where(tick_distance.le(n), 0)
        hourly[column] = bucket.groupby(bars.hour).sum(min_count=101)
    hourly["day"] = date_key(hourly.index)
    daily = pd.DataFrame(index=hourly.groupby("day").size().index)
    daily["uni_usdc_usdt_liquidity_snapshot_hours"] = hourly.groupby("day").size()
    for col in hourly:
        if col != "day":
            daily[col] = daily_last(hourly[col], hourly.day)
    full_day = daily.uni_usdc_usdt_liquidity_snapshot_hours.eq(24)
    daily.loc[~full_day, [c for c in daily if "inventory_pm" in c]] = np.nan
    audit = {
        "uniswap_liquidity_hourly_snapshots": int(bar_counts.size),
        "uniswap_liquidity_negative_buckets": int(bars.usd_total.lt(0).sum()),
        "uniswap_liquidity_full_24h_days": int(full_day.sum()),
        "uniswap_liquidity_parquet_sha256": file_hash(path),
    }
    return daily, audit


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-root", type=Path, required=True,
                        help="Directory containing extracted Curve/ and Uniswap/ folders")
    parser.add_argument("--price-panel", type=Path, required=True,
                        help="Thesis reference-adjusted 11-coin price CSV")
    parser.add_argument("--output-dir", type=Path, default=Path(__file__).parent)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    panel = pd.read_csv(args.price_panel, parse_dates=["date"])
    required = {"date", "dai", "usd_coin", "tether"}
    if not required.issubset(panel.columns) or panel.date.duplicated().any():
        raise ValueError("Thesis price panel lacks unique dates and USD-coin columns")
    panel = panel[(panel.date >= START.tz_localize(None))
                  & (panel.date < END_EXCLUSIVE.tz_localize(None))].copy()
    panel = panel.sort_values("date")
    expected = pd.date_range(START.tz_localize(None),
                             END_EXCLUSIVE.tz_localize(None) - pd.Timedelta(days=1),
                             freq="D")
    if not pd.DatetimeIndex(panel.date).equals(expected):
        raise ValueError("Thesis price panel does not cover every calendar date")

    curve, curve_audit = prepare_curve(args.data_root, panel)
    usdc_usdt, uu_audit = prepare_uniswap_pool(args.data_root, "USDC_USDT")
    dai_usdc, du_audit = prepare_uniswap_pool(args.data_root, "DAI_USDC")
    liquidity, liq_audit = prepare_uniswap_liquidity(args.data_root)
    daily = pd.DataFrame(index=expected)
    daily.index.name = "date"
    for part in (curve, usdc_usdt, dai_usdc, liquidity):
        daily = daily.join(part, validate="one_to_one")
    if daily.index.has_duplicates or not daily.index.equals(expected):
        raise ValueError("Daily output must match the 2020-01-01 to 2026-05-31 calendar")
    if (daily["curve_3pool_imbalance_eod"].dropna() < 0).any():
        raise ValueError("Negative Curve imbalance")
    if (daily["uni_usdc_usdt_inventory_pm10ticks_usd_eod"].dropna()
            > daily["uni_usdc_usdt_inventory_pm25ticks_usd_eod"].dropna()).any():
        raise ValueError("Near-tick liquidity is not nested")
    if (daily["uni_usdc_usdt_inventory_pm25ticks_usd_eod"].dropna()
            > daily["uni_usdc_usdt_inventory_pm50ticks_usd_eod"].dropna()).any():
        raise ValueError("Wide-tick liquidity is not nested")

    main_path = args.output_dir / "stablecoin_onchain_daily_20200101_20260531.csv"
    daily.to_csv(main_path, index=True, na_rep="", float_format="%.15g")
    audit = {
        "source_release_tag": "data-2026-09-19",
        "thesis_panel_sha256": file_hash(args.price_panel),
        "output_sha256": file_hash(main_path),
        "total_calendar_days": len(daily),
        **curve_audit, **uu_audit, **du_audit, **liq_audit,
    }
    for column in daily.columns:
        audit[f"{column}_nonmissing_days"] = int(daily[column].notna().sum())
        audit[f"{column}_first_valid_day"] = (
            str(daily[column].first_valid_index().date())
            if daily[column].first_valid_index() is not None else ""
        )
    qa_path = args.output_dir / "onchain_data_quality_checks.csv"
    pd.DataFrame(audit.items(), columns=["metric", "value"]).to_csv(qa_path, index=False)
    print(f"Wrote {len(daily)} daily dates to {main_path}")
    print(f"Wrote {len(audit)} quality checks to {qa_path}")


if __name__ == "__main__":
    main()
