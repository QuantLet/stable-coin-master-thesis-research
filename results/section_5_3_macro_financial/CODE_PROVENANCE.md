# Code and data provenance

- Primary input: frozen robust-scaled strict minimum-GACV coefficient panel used by Sections 5.1 and 5.2.
- Sensitivity inputs: screened-GACV coefficient panel and unscaled strict coefficient panel.
- State input: primary robust-scaled DPI series.
- Missing-data audit input: rolling-window past-fill flags.
- New Section 5.3 code performs only post-estimation aggregation, inference, robustness comparison and R-based figure export.
- Input sizes and MD5 hashes are recorded in `qa/input_manifest_md5.csv`.
- The manuscript and presentation were inspected for definitions and argument order; neither source file was edited.
