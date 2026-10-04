# Figure 6.4 contract and QA

- Intended conclusion: GMV reduces both prespecified risk measures relative to EW; LTEC and QTEC do not show Holm-adjusted improvements.
- Evidence geometry: one forest plot of risk ratios with paired 95% moving-block bootstrap intervals and an EW reference line at one.
- Visual hierarchy: strategy comparison first, endpoint distinction second; exact values remain in the source-data file and Table 6.4.
- Backend: R only (`ggplot2`, `svglite`, `ragg`, base R PDF fallback where Cairo/XQuartz is unavailable).
- Export: 140 × 82 mm; SVG and PDF vectors; 600-dpi TIFF; 300-dpi PNG preview.
- Integrity: all six plotted rows are exported to `figures/Figure_6_4_source_data.csv`; no observations are sampled or removed for plotting.
- Visual QA: labels, confidence intervals, benchmark line and legend are legible at final dimensions; no overlap or clipping is present.
- Static preflight: 12 passes, 2 non-substantive warnings, 0 failures. R syntax was separately parsed successfully.

