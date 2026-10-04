# Statistical reporting audit for Section 5.2

| Requirement | Implementation | Status |
|---|---|---|
| Analysis unit | 2,252 calendar dates; 110 directed pairs are retained jointly within each date | PASS |
| Primary estimand | Frequency of positive, negative and any non-zero selected coefficient | PASS |
| Dependence | HAC with Bartlett lag 90 and circular moving-block bootstrap with block length 90 | PASS |
| Bootstrap disclosure | 1,999 draws; seed 20260908; pointwise percentile intervals | PASS |
| Multiplicity | One Holm family covers the system-wide sign contrast and two taxonomy contrasts; a separate Holm family covers all 55 dyad-direction tests | PASS |
| Effect sizes | Network densities, percentage-point contrasts, rank correlations and top-ten overlap reported | PASS |
| Uncertainty | 95% intervals accompany overall densities, ranked aligned frequencies and group/state contrasts | PASS |
| Missing-data sensitivity | Excludes 539 rolling outputs whose estimation windows contain past-filled deviation cells | PASS |
| Specification sensitivity | Strict versus screened GACV and robust-scaled versus unscaled estimates compared | PASS |
| Temporal stability | Equal chronological halves compared using Spearman rank correlation and top-ten overlap | PASS |
| Direction semantics | Predictor-to-target direction stated; no causal or temporal interpretation | PASS |
| Selection uncertainty boundary | HAC/bootstrap applies to saved selection sequences and is not presented as coefficient-level post-selection inference | PASS |

The principal inferential result is the system-wide aligned-minus-inverse contrast, accompanied by its effect size, HAC(90) interval and Holm-adjusted p value. The main pair ranking remains explicitly post hoc and its intervals are pointwise, not simultaneous. The taxonomy contrasts are based on Chapter 3 classifications but remain associational because neither reference asset nor design category is randomly assigned and the contrasts are not separately identified from correlated coin characteristics.
