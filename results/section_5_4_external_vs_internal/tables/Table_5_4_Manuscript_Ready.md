**Table 5.4 | Conditional fit and out-of-sample tail performance**

| Model | Conditional GACV | Conditional skill (%) | OOS check loss | OOS skill (%) |
|---|---:|---:|---:|---:|
| Intercept-only benchmark | 0.1808 | 0.0 | 0.2123 | 0.0 |
| Macro-only | 0.1658 | 8.3 | 0.2388 | −12.5 |
| Coin-only | 0.1199 | 33.7 | 0.2460 | −15.9 |
| Joint | 0.1139 | 37.0 | 0.2544 | −19.8 |

*Notes:* Results refer to the same-day conditional specification. Conditional skill is the percentage reduction in minimum-GACV-adjusted loss relative to the rolling intercept-only benchmark. OOS skill is the corresponding reduction in MAD-standardized 5% check loss. Positive values indicate lower loss. The sample contains 2,252 dates and 24,772 target-date equations. Date-level HAC inference uses a Bartlett kernel with lag 90. In the pre-specified three-contrast family, the conditional GACV reductions from adding the coin block to the macro-only model (31.3%) and adding the macro block to the coin-only model (5.0%) both have Holm-adjusted \(p<0.001\).

