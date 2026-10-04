# Figure QA

- Backend: R only.
- Final dimensions: 183 mm x 112 mm.
- Data inclusion: all 11 targets, five macro factors and 2,252 dates; no smoothing or post-hoc cell filtering.
- Visual inspection: labels, cell values and colour legend are legible in the 300-dpi R preview; no overlap or clipping remains after moving factor labels to the top.
- Exports: editable SVG, vector PDF, 600-dpi TIFF and 300-dpi PNG.
- Editable-text check: the SVG contains 78 text elements; the PDF uses a Type 1 Helvetica font object rather than outlined raster text.
- Automated-validator note: the validator flags the absence of `cairo_pdf`. Cairo/X11 is unavailable in the local R runtime, so the PDF was produced with R's native vector PDF device and the SVG with `svglite`. This is an environment-specific false positive rather than a rasterisation defect. SVG is the preferred editable master.
- Statistical markings: none are placed on individual heat-map cells. Factor-level inference and multiplicity corrections are reported in the table and source-data files.
