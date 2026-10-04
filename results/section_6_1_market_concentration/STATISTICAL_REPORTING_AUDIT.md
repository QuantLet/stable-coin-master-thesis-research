# Section 6.1 statistical reporting audit

- **Question.** How concentrated is market capitalization within the eleven-coin thesis sample, and how does that structure change over time?
- **Analysis unit.** One UTC calendar date. The panel contains 2,343 dates from 1 January 2020 to 31 May 2026.
- **Measurement.** Daily HHI uses decimal market-capitalization shares. NHHI adjusts its lower bound for the number of coins with an observed positive market capitalization on each date. Top-2 share and the effective number of coins are complementary descriptive measures.
- **Coverage.** The number of positive observed market capitalizations ranges from eight to eleven and has a median of eleven. Missing values are not set to zero, interpolated or carried forward.
- **Supplement.** The sole end-of-sample gap identified in the local files, PAXG from 7 April to 31 May 2026, is filled from a frozen CoinGecko public-API response. The raw JSON is retained in `source_data/input/` and listed in the source manifest.
- **Inference.** No p values are reported in Section 6.1. The metrics are deterministic transformations of the observed daily market-capitalization panel; significance tests would not resolve the main measurement boundary or establish causality.
- **Figure.** The plotted lines are 31-day rolling medians for readability. All reported values are calculated from the unsmoothed daily metrics.
- **Interpretation boundary.** Results describe the selected eleven coins, not all stablecoins. Market capitalization is not circulating supply. Concentration is not evidence of causal amplification of depegging pressure; this association belongs in Section 6.2.
- **Data-provider risk.** CoinGecko market capitalization can inherit errors in circulating-supply estimates, token migrations and discontinued series. The source manifest, daily active-coin count and no-fill rule make this boundary visible but cannot eliminate provider measurement error.

