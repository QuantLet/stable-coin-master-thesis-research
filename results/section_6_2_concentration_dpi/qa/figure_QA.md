# Figure 6.2 QA

- R-only rendering: PASS.
- SVG and PDF vector exports: PASS.
- TIFF export at 600 dpi: PASS.
- Helvetica text at a minimum detected size of 7 pt: PASS.
- Cross-backend plotting references: none.
- Final dimensions: 160 x 92 mm.
- Source-data traceability: `figures/Figure_6_2_source_data.csv`.
- Visual inspection: labels, confidence intervals, zero reference and legend are readable at final size.
- Validator warnings reviewed: model-specific complete-case counts are exported in every results row; all logged variables are required to be positive by the analysis QA; the width is set through explicit `width_mm` and `height_mm` variables.
