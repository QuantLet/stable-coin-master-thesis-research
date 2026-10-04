# Figure 6.5 contract and QA

- Core conclusion: portfolio resilience varies by event; GMV usually lowers drawdown, whereas LTEC and QTEC do not provide consistent event protection.
- Evidence geometry: one horizontal dot plot of event-level maximum-drawdown differences relative to EW.
- Archetype: quantitative comparison; no multi-panel assembly.
- Interpretation: negative values indicate a smaller drawdown than EW; the zero line is the common benchmark.
- Statistical boundary: event-level points are descriptive. Formal pooled inference is reported in Table 6.5.
- Backend: R only (`ggplot2`, `svglite`, `ragg`, base R PDF fallback when Cairo/XQuartz is unavailable).
- Export: 150 × 92 mm; SVG and PDF vectors; 600-dpi TIFF; 300-dpi PNG.
- Source traceability: all 18 strategy-event observations are in `figures/Figure_6_5_source_data.csv`.
- Visual QA: all labels and points are legible at final size; no overlap or clipping is present.
- Static preflight: 12 passes, 2 non-substantive warnings, 0 failures. R syntax was separately parsed successfully.

