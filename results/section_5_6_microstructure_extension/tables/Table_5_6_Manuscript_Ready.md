# Table 5.6. Matched-sample effect of on-chain indicators

| Timing | Outcome | Baseline | With on-chain | Difference [HAC 95% CI] | Holm-adjusted *p* |
|:--|:--|--:|--:|--:|--:|
| Same-day conditional | Selected GACV | 0.1183 | 0.1130 | +0.00534 [0.00428, 0.00640] | <0.001 |
| Same-day conditional | Held-out check loss | 0.2830 | 0.2860 | −0.00302 [−0.01254, 0.00650] | 0.534 |
| All predictors lagged | Selected GACV | 0.1350 | 0.1249 | +0.01017 [0.00448, 0.01585] | 0.0014 |
| All predictors lagged | Held-out check loss | 0.3131 | 0.3064 | +0.00675 [−0.00388, 0.01738] | 0.426 |

*Notes:* The common evaluation sample contains 1,521 dates (2 April 2022–31 May 2026) and 11 target coins per date and timing. Positive differences favour the augmented model. The baseline has 10 contemporaneous or lagged coin predictors plus five lagged macro variables; the extension adds three lagged on-chain indicators. In-window GACV is not a forecasting metric. Mean quantile skill scores relative to the rolling intercept-only benchmark are −24.2% (baseline) and −25.5% (augmented) with same-day coin information, and −37.4% and −34.4% with all predictors lagged. HAC uses a 90-day Bartlett lag on paired daily cross-target means; Holm correction covers the four table comparisons. Full-precision estimates, 90-day block-bootstrap intervals and model-selection frequencies are in the accompanying CSV tables.
