# Reproduction order

Run commands from the repository root unless a command explicitly changes directory. Section-level frozen inputs are included, so later sections do not require earlier sections to be rerun first.

| Manuscript section | Command | Main output |
|---|---|---|
| 4.4 | `Rscript results/section_4_4_reference_adjusted_event_study/code/02_event_study_three_definitions.R && Rscript results/section_4_4_reference_adjusted_event_study/code/03_plot_reference_adjusted_event_study.R && Rscript results/section_4_4_reference_adjusted_event_study/code/05_plot_figure_4_4_redesign.R && Rscript results/section_4_4_reference_adjusted_event_study/code/04_finalize_bundle_QA.R` | Reference-adjusted DPI event tables and Figure 4.4 |
| 4.5 | `FRM_SKIP_RAW_BUILD=1 FRM_SKIP_ESTIMATION=1 Rscript results/section_4_5_stable_crypto/code/run_all.R` | Stablecoin–crypto comparisons and Figure 4.5 |
| 5.1 | `Rscript results/section_5_1_active_risk_drivers/code/section_5_1_active_sets.R` | Active-set composition and Figure 5.1 |
| 5.2 | `(cd results/section_5_2_tail_risk_links && Rscript code/section_5_2_tail_risk_links.R && Rscript code/section_5_2_inference_and_robustness.R)` | Directional tail-link tables and Figure 5.2 |
| 5.3 | `(cd results/section_5_3_macro_financial && Rscript code/section_5_3_macro_financial.R)` | Macro-financial selection results and Figure 5.3 |
| 5.4 | `Rscript results/section_5_4_external_vs_internal/code/02_analyse_block_models.R && Rscript results/section_5_4_external_vs_internal/code/03_plot_figure_5_4.R && Rscript results/section_5_4_external_vs_internal/code/04_finalize_QA.R` | Macro-only, coin-only, and joint-model comparison |
| 5.5 | `(cd results/section_5_5_centrality && Rscript code/section_5_5_centrality.R && Rscript code/section_5_5_onchain_crosscheck.R)` | Network roles, robustness, and on-chain cross-check |
| 5.6 | `Rscript results/section_5_6_microstructure_extension/code/02_analyse_micro_extension.R` | Matched microstructure extension tables |
| 6.1 | `Rscript results/section_6_1_market_concentration/code/01_build_market_concentration.R` | Daily concentration measures and Figure 6.1 |
| 6.2 | `Rscript results/section_6_2_concentration_dpi/code/01_estimate_concentration_dpi.R` | Concentration–DPI tests and Figure 6.2 |
| 6.3 | `Rscript results/section_6_3_portfolio_protocol/code/01_run_portfolio_protocol.R` | Leakage-free portfolio returns and weights |
| 6.4 | `Rscript results/section_6_4_oos_portfolio_performance/code/01_build_oos_performance.R` | Out-of-sample comparisons and Figure 6.4 |
| 6.5 | `Rscript results/section_6_5_event_resilience/code/01_run_event_resilience.R` | Event resilience tests and Figure 6.5 |

## Full-estimation substitutions

For a full reconstruction rather than a frozen-input reproduction:

- Section 4.4: use `results/section_4_4_reference_adjusted_event_study/code/run_all.R` to rebuild panels and re-estimate all FRM definitions.
- Section 4.5: omit both skip flags and set `COINGECKO_DATA_ROOT`.
- Section 5.4: run `results/section_5_4_external_vs_internal/code/run_all.R` without the skip flag.
- Section 5.6: run `01_estimate_micro_extension.R` before `02_analyse_micro_extension.R`.

The Section 4.4 full run is computationally intensive but self-contained because its exact analysis inputs are preserved in the section package.

## Expected numerical variation

CSV results should match the bundled outputs up to ordinary floating-point and platform-level differences. PDF, SVG, PNG, and TIFF files may differ byte-for-byte across operating systems because of fonts and graphics devices even when the plotted values are identical. Use the source-data CSV files and numerical tables for substantive verification.
