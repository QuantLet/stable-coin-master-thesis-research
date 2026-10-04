# Statistical reporting audit

| Item | Implementation | Status |
|---|---|---|
| Estimation universe | 11 targets, 5 macro variables, 2,252 primary dates | PASS |
| Frozen-model alignment | Maximum coefficient error below \(10^{-12}\) | PASS |
| Target scaling | Training-window median/MAD only | PASS |
| GACV selection | Strict minimum finite GACV with original row mapping | PASS |
| Held-out separation | Parameters and scaling estimated through \(t-1\); same-day coin inputs are labelled conditional rather than predictive | PASS |
| Strict forecast | Stablecoin and macro blocks both lagged one day | PASS |
| Sampling unit | Cross-target losses averaged by calendar date | PASS |
| Serial dependence | Bartlett HAC lag 90 | PASS |
| Multiple comparisons | Holm correction within each three-contrast family | PASS |
| Distributional check | Circular 90-day block bootstrap, 1,999 replications | PASS |
| Missing-data sensitivity | 539 past-filled Chapter 4 windows excluded | PASS |
| Figure source data | 108 displayed cells trace to one CSV | PASS |
