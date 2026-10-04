# FRM_SC — pipeline-ul FRM @ Stable Coins

Copie a tuturor scripturilor R din familia `FRM_SC_*` plus dependența externă
`FRM_Statistics_Algorithm.R`, extrase din `2026 Stable Coins Risk/`.
Originalele nu au fost modificate sau mutate.

## Cum se rulează

```r
setwd("<root-ul proiectului>")   # NU acest folder — scripturile citesc Input/ și scriu Output/, Website/
source("FRM_SC_run_all.R")
```

**Atenție:** `FRM_SC_config.R:3` are `wdir` setat pe calea de Windows
(`D:\G\PROIECTE\2025 FRM Stable Coins`). Pe Mac trebuie decomentată linia 2.
Config-ul fixează și `date_start = 20200102`, `date_end = 20250302`, `tau = 0.05`,
`s = 90`, `stock_main = "tether"`.

Scripturile presupun ca director de lucru rădăcina proiectului, unde există
`Input/Stable/...`, `Output/Stable/...` și `Website/Stable/...`.

## Ordinea din `FRM_SC_run_all.R`

| # | Fișier | Ce face |
|---|--------|---------|
| 1 | `FRM_SC_config.R` | căi, parametri (`tau`, `s`, `date_end`), pachete; sursează `FRM_Statistics_Algorithm.R` |
| 2 | `FRM_SC_load_data.R` | citește prețuri + market cap → `stage_20_loaded.RData` |
| 3 | `FRM_SC_estimation_varying.R` | estimare LASSO-quantile cu set variabil de monede → `stage_30_varying.RData` |
| 4 | `FRM_SC_history_outputs.R` | indicele FRM istoric → `stage_50_index.RData` |
| 5 | `FRM_SC_frm_plot_stable.R` | graficul indicelui FRM@Stable |
| 6 | `FRM_SC_build_fixed_from_csv.R` | reconstruiește matricile de adiacență pe setul fix de monede |
| 7 | `FRM_SC_centrality.R` | măsuri de centralitate în rețea |
| 8 | `FRM_SC_crypto_compare.R` | comparație FRM@Stable vs FRM@Crypto |
| 9 | `FRM_SC_hhi.R` | **HHI** din lambda și din market cap (zilnic + cumulat) → `Output/Stable/Lambda/HHI_*.csv` |
| 10 | `FRM_SC_portfolio_dynamic.R` | portofolii dinamice |
| 11 | `FRM_SC_portfolio_LTEC.R` | portofolii LTEC |
| 12 | `FRM_SC_hhi_vs_frm.R` | **graficul dual-axis HHI vs FRM** (figura din prezentare) |
| 13 | `FRM_SC_centrality_indicators.R` | indicatori derivați din centralitate |
| 14 | `FRM_SC_network_gif.R` | animația rețelei |

`FRM_SC_utils.R` nu apare în `run_all` pentru că e sursat de aproape fiecare
script (temă ggplot `theme_transparent_bottom`, `dualaxis_frm_line_vs_bars()`,
`pretty_coin()`).

## Figura „Normalized HHI for SCs" din slide

`FRM_SC_hhi_vs_frm.R`, apelul `plot_dual_raw()` de la liniile 138–143 →
`Website/Stable/20250302/HHI_vs_FRM_Stable_DAILY_Raw_DualAxis.png`
(albastru `#0A84FF` = FRM brut, axa stângă; roșu `#FF3B30` = HHI market-cap zilnic,
axa dreaptă 0–1; fundal transparent). Titlul, cercurile și adnotarea „2024 Nov
Trump Elected" sunt adăugate ulterior în Keynote.

## Fișiere din familie care NU sunt în `run_all`

Rulabile separat sau rămase din iterații anterioare:

- `FRM_SC_adj_heatmap.R` — heatmap al matricei de adiacență
- `FRM_SC_centrality_models.R` — modele pe seriile de centralitate
- `FRM_SC_compare_crypto.R` — versiune anterioară a lui `FRM_SC_crypto_compare.R`
- `FRM_SC_estimation.R` — versiune anterioară a lui `FRM_SC_estimation_varying.R`
- `FRM_SC_helpers.R` + `FRM_SC_preprocess.R` — pereche de preprocesare a datelor brute
- `FRM_SC_index_and_charts.R` — variantă a pasului index + grafice
- `FRM_SC_macro.R` — variabile macro
- `FRM_SC_network.R` — versiune statică, înlocuită de `FRM_SC_network_gif.R`
- `FRM_SC_portfolios.R` — versiune anterioară a scripturilor de portofoliu

## Ce NU e aici

Din același proiect, dar studii separate — la cerere pot fi extrase la fel:

- `TRI_*.R` (~35 fișiere) — Stablecoin Trilemma: stability / decentralization /
  efficiency, DEA, copule, regresii panel (`TRI_run_all.R`)
- `_00_config.R` … `_100_portfolio_dynamic.R` + `run_all.R` — versiunea anterioară
  a pipeline-ului FRM_SC
- scripturi ad-hoc datate: `20250914 Analysis.r`, `20260103 Peg Analysis.r`,
  `FRM Stable Coins.R`, `bootstrap_*.R` etc.
