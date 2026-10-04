# 日度链上变量（与论文样本日期对齐）

主数据文件：`stablecoin_onchain_daily_20200101_20260531.csv`。每行一个 UTC 自然日；日期范围与论文现有 11 币价格面板相同，缺失的链上历史保留为空值，**不是零**。`onchain_data_quality_checks.csv`记录来源文件 SHA-256、实际覆盖天数与关键核查。`build_onchain_daily.py`可重建两个 CSV。

## 来源及范围

- 来源仓库：[stablecoin-onchain-data](https://github.com/MSCA-DN-Digital-Finance/stablecoin-onchain-data)，固定使用 [data-2026-09-19](https://github.com/MSCA-DN-Digital-Finance/stablecoin-onchain-data/releases/tag/data-2026-09-19) 的 `curve_data.zip` 和 `uniswap_data.zip`。下载的是 Release 附件，而不是只克隆代码仓库。文件哈希见质量核查表。
- 币价来源：论文现有 `Stable_Price_reference_adjusted_11.csv`；这里只用 DAI、USDC、USDT 三列和原有日历，未修改原始面板或 DPI。
- Curve 3pool：2020-09-07 至 2026-05-31，共 2,093 个完整日。Uniswap USDC/USDT 和 DAI/USDC 及流动性曲线：2022-01-01 至 2026-05-31，共 1,612 个完整日。完整三源样本从 2022-01-01 开始，无法回填 2020—2021 年的 Uniswap 变量。

## 可用于论文的字段

| 字段 | 单位及定义 |
| --- | --- |
| `curve_3pool_tvl_usd_eod` | 当日最后一条小时快照的 3pool TVL，美元。 |
| `curve_3pool_dai_share_eod` | DAI 在 3pool 的价值份额，0—1。 |
| `curve_3pool_imbalance_eod` | `0.5 × Σ币种 |份额 - 1/3|`，DAI/USDC/USDT 的整体偏离；0 为等份状态。这是描述性失衡度量，不等于 Curve 协议的最优库存。 |
| `curve_3pool_swap_count` | 当日逐笔 Curve 交换次数。 |
| `curve_3pool_swap_notional_units` | 当日逐笔交换中付出的币单位合计；适合作为主要交易活跃度指标，无币价回归泄漏，但不是严格美元金额。 |
| `curve_3pool_swap_volume_usd_est` | 逐笔售出币数量乘以论文面板该币的**当日收盘美元价格**后加总。仅为日度估算，不能解释为逐笔成交时的精确美元额；若解释同日脱锚，优先使用上面的币单位成交额或交易次数。 |
| `uni_usdc_usdt_*` / `uni_dai_usdc_*` | Uniswap v3 **0.01% 手续费池**，`swap_volume_usd` 为代码中逐笔 `amountUSD` 小时合计的日度加总，`swap_count` 为交易次数，`tvl_usd_eod` 为当日最近有效 TVL。未将不同费率池的 TVL 混合。 |
| `uni_usdc_usdt_inventory_pm10ticks_usd_eod` 等 | 来自 USDC/USDT 价格中心 101 个 tick 桶，取现价 tick 左右 10、25、50 个 tick 的流动性桶美元估值之和。每 tick 在价格上约 1 bp；这是**AMM 库存近价代理**，不是 10/25/50 bp 的精确可执行深度，更不是中心化交易所订单簿。50-tick 量使用了整个已发布的有限窗口，存在边界限制。 |
| `*_hours` | 当日该来源有效的小时条数。其他日度值仅在来源提供完整 24 小时时计算；缺失不插值。 |

## 清洗中解决的问题

Curve 的发布脚本把快照时间向上取整到下一整点。与交易事件对照后，快照 H 对应的实际交易小时为 H−1：对齐后的对数交易量相关系数约为 0.9965，未调整时约为 0.4645。因此先把 Curve 快照的小时标签回拨一小时，再汇总到 UTC 日期。上游脚本还会对缺失小时向前填充，包括小时交易量；对齐后仍有 51 个“有发布量、无对应交易”的小时。为避免重复计量，主 CSV 不直接累加上游小时成交量，而从逐笔交易记录重建日交易次数及币单位成交量。

Uniswap 交易数据按固定 0.01% 费率池聚合，并按 UTC 日期汇总；TVL 取当天最近有效快照，不从上一自然日结转。所有结果截于 2026-05-31。`*_eod` 和当日交易量在当天结束后才已知：若用来预测次日风险，回归中的解释变量必须相对于目标日滞后一天；同日回归只能解释关联。新增变量并未纳入原 90 日滚动 Quantile Lasso，因此不能声称已重估 DPI。

## 复现

将两个 Release ZIP 解压到同一根目录，使其具有 `data/Curve/*.parquet` 与 `data/Uniswap/*.parquet`，安装 Python 的 `pandas`、`numpy`、`pyarrow`，然后运行：

```bash
python build_onchain_daily.py \
  --data-root /path/to/data \
  --price-panel /path/to/Stable_Price_reference_adjusted_11.csv \
  --output-dir /path/to/desired_output
```

本数据只覆盖 Ethereum 主网的指定 Curve 3pool 和两组 Uniswap v3 稳定币池，不能代表全部交易场所或论文中全部 11 种稳定币。仓库没有提供 Binance 买卖价差与订单簿深度，论文中不要用这些链上库存代理替代它们。
