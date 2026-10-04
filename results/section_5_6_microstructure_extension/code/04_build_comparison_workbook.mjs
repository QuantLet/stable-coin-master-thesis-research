import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

const codeDir = path.dirname(new URL(import.meta.url).pathname);
const bundleDir = path.dirname(codeDir);
const tableDir = path.join(bundleDir, "tables");
const csvText = await fs.readFile(
  path.join(tableDir, "Table_5_6_Manuscript_Comparison.csv"), "utf8"
);
const workbook = await Workbook.fromCSV(csvText, { sheetName: "Comparison" });
const sheet = workbook.worksheets.getItem("Comparison");
sheet.showGridLines = false;
sheet.tabColor = "#29465B";

sheet.getRange("A1:K1").values = [[
  "Timing", "Outcome", "Dates", "Baseline", "With on-chain",
  "Gain (%)", "HAC CI low", "HAC CI high", "Holm p",
  "Baseline QSS", "On-chain QSS"
]];
const numericRange = sheet.getRange("C2:K5");
numericRange.values = numericRange.values.map(row =>
  row.map(value => value === "" || value == null ? null : Number(value))
);
sheet.getRange("A2:B5").values = [
  ["Same-day conditional", "Selected GACV"],
  ["Same-day conditional", "Held-out check loss"],
  ["All predictors lagged", "Selected GACV"],
  ["All predictors lagged", "Held-out check loss"]
];

sheet.getRange("A1:K9").format.font = {
  name: "Arial", size: 10, color: "#202A35"
};
sheet.getRange("A1:K1").format = {
  fill: "#29465B",
  font: { name: "Arial", size: 10, bold: true, color: "#FFFFFF" },
  rowHeight: 31,
  verticalAlignment: "center",
  horizontalAlignment: "center",
  wrapText: true
};
sheet.getRange("A2:K3").format.fill = "#F2F5F7";
sheet.getRange("A2:K5").format.rowHeight = 25;
sheet.getRange("A1:A9").format.columnWidth = 23;
sheet.getRange("B1:B9").format.columnWidth = 21;
sheet.getRange("C1:C9").format.columnWidth = 11;
sheet.getRange("D1:E9").format.columnWidth = 15;
sheet.getRange("F1:F9").format.columnWidth = 13;
sheet.getRange("G1:H9").format.columnWidth = 15;
sheet.getRange("I1:I9").format.columnWidth = 12;
sheet.getRange("J1:K9").format.columnWidth = 16;
sheet.getRange("C2:C5").setNumberFormat("#,##0");
sheet.getRange("D2:E5").setNumberFormat("0.0000");
sheet.getRange("F2:F5").setNumberFormat("0.00");
sheet.getRange("G2:H5").setNumberFormat("0.0000");
sheet.getRange("I2:I5").setNumberFormat("0.0000");
sheet.getRange("I2").setNumberFormat("0.00E+00");
sheet.getRange("J2:K5").setNumberFormat("0.0%");
sheet.freezePanes.freezeRows(1);

sheet.getRange("A7").values = [[
  "Gain (%) = 100 × (baseline − on-chain model) / baseline. Positive favors the on-chain model."
]];
sheet.getRange("A8").values = [[
  "Same-day conditional includes other coins observed on the evaluation date; all-lagged is a next-day forecast."
]];
sheet.getRange("A9").values = [[
  "HAC intervals use a 90-day Bartlett lag. Holm p values cover the four paired contrasts."
]];
sheet.getRange("A7:K9").format.font = {
  name: "Arial", size: 9, color: "#526271"
};

workbook.recalculate();
const inspect = await workbook.inspect({
  kind: "table", range: "Comparison!A1:K5", include: "values",
  tableMaxRows: 5, tableMaxCols: 11, maxChars: 8000
});
console.log(inspect.ndjson);
const preview = await workbook.render({
  sheetName: "Comparison", range: "A1:K9", scale: 1.5, format: "png"
});
await fs.writeFile(path.join(tableDir, "Comparison_Workbook_Preview.png"),
                   new Uint8Array(await preview.arrayBuffer()));
const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(path.join(tableDir, "Chapter_5_Microstructure_Comparison.xlsx"));
