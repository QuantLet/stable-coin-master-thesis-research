# Code and result provenance

| Component | Audited version | Role in Section 5.2 |
|---|---|---|
| `QuantLet/Local_Quantile_Regression` | commit `7f9b32eeb0723f88e0f85ee75303d8ee73722108`, 2025-01-26 | Conceptual implementation of the asymmetric quantile check loss. No rolling Lasso or network estimation. |
| `QuantLet/FRM_Stable_Coins` | commit `3b2b032fb1b86390b36d57bbc1e3166c729b0688`, 2025-10-02 | Methodological ancestor for the 90-day, 5% Quantile-Lasso adjacency construction. Not the source of reported numbers. |
| Latest local primary DPI package | frozen output used by `KEWENFAN20260906.docx` | Numerical basis: reference-adjusted deviations, fixed eleven-coin sample, five lagged macro controls, robust scaling and strict minimum-GACV selection. |
| `section_5_2_tail_risk_links.R` | delivered in this package | Reconstructs directed signed pair-selection sequences, summaries, Table 5.2 inputs and Figure 5.2. |
| `section_5_2_inference_and_robustness.R` | delivered in this package | Adds dependence-aware uncertainty, multiplicity correction, group contrasts and specification/time robustness. |

The public scripts are preserved in `reference_code/` for auditability. The frozen numerical inputs are preserved in `source_data/input/`, accompanied by MD5 manifests. No public-repository output is mixed into the manuscript estimates.
