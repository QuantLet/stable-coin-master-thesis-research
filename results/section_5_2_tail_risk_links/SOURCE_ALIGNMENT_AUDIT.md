# 5.2 source and code alignment audit

## PPT logic retained

The presentation defines the Financial Risk Meter as a Quantile-Lasso measure of tail-event co-movement and motivates network topology, systemic-risk factors and the relative contribution of each stablecoin. Section 5.2 operationalises only the coin-to-coin part of that question; portfolio construction remains in Chapter 6.

## Public repository audit

- `QuantLet/Local_Quantile_Regression`, checked at commit `7f9b32eeb0723f88e0f85ee75303d8ee73722108` (26 January 2025), supplies the asymmetric quantile check-loss illustration in `LQRcheck/LQRcheck.R`. It does not implement Lasso selection, GACV, rolling estimation, network construction or inference, and is therefore conceptual provenance for the loss function only.
- `QuantLet/FRM_Stable_Coins`, checked at commit `3b2b032fb1b86390b36d57bbc1e3166c729b0688` (2 October 2025), supplies the original 90-day, 5% Quantile-Lasso adjacency logic. Its public version uses log returns, ends in March 2025 and estimates macro series as target equations as well as predictors. It is therefore methodological provenance, not the numerical basis for Section 5.2.

## Local primary specification used

The empirical analysis uses the frozen coefficient panel underlying the latest manuscript specification: eleven reference-adjusted signed peg deviations, five one-calendar-day-lagged macro changes, 90 calendar-day windows, the 5% quantile, window-local median/MAD scaling and strict minimum finite GACV. It contains 2,252 dates, 11 target equations and 15 candidate predictors per target.

The sample ends on 31 May 2026, matching the manuscript estimator and frozen outputs. The presentation's rounded description of January 2020 to June 2026 is not used to extend the results beyond the available endpoint. The presentation's broader multi-quantile grid motivates possible robustness analysis, but Section 5.2 retains the pre-existing 5% lower-tail object so that it remains consistent with Chapter 4.

## Direction correction

The legacy scripts store target equations in adjacency-matrix rows and predictors in columns (`A[target, predictor] = beta`). Passing this matrix directly to `igraph::graph_from_adjacency_matrix(..., mode = "directed")` reverses the intended regression direction because `igraph` reads rows as sources. The new code constructs every edge explicitly as `predictor -> target`; centrality calculations in Section 5.5 must use the same orientation.

## Statistical additions

- Overall network densities use HAC standard errors with Bartlett lag 90.
- The system-wide aligned-minus-inverse contrast and two taxonomy contrasts form one three-test Holm family.
- Pairwise direction differences use HAC(90) inference and Holm adjustment across the complete family of 55 unordered dyads.
- Reference-asset and design-category contrasts first collapse the cross-section to one difference per date.
- Pointwise persistence intervals use 1,999 circular moving-block bootstrap samples with 90-day blocks.
- Robustness checks compare strict with screened GACV, robust-scaled with unscaled estimation, full with imputation-unexposed windows, and the two chronological sample halves.

These additions quantify dependence in the saved rolling output. They are not post-selection confidence intervals for the underlying Quantile-Lasso coefficients.
