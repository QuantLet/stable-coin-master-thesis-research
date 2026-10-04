# Figure QA

- Final size: 183 mm by 118 mm.
- Backend: R only (`ggplot2`, `svglite`, base PDF or Cairo PDF when available, and `ragg`).
- The plotting script parses and runs successfully in R 4.4.2.
- All 108 intended cells are present and finite; no target, date or model is selected after viewing the results.
- SVG and PDF retain vector text; TIFF is exported at 600 dpi and PNG at 300 dpi.
- The automated source audit reports no failures. Its width warning is resolved by the explicit 183 mm export setting. The syntax warning is resolved by the successful `Rscript` parse and execution checks.
- The PNG was inspected at full size: labels, group separators, cell values and legend are readable without overlap.
