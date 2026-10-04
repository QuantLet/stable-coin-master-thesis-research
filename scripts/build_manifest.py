#!/usr/bin/env python3
"""Build deterministic repository inventories and SHA-256 checksums."""

from __future__ import annotations

import csv
import hashlib
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "manifest"
MANIFEST.mkdir(exist_ok=True)

EXCLUDED_DIRS = {
    ".git",
    ".Rproj.user",
    "R-library",
    "node_modules",
    "__pycache__",
    "checkpoints",
}
EXCLUDED_SUFFIXES = {".tif", ".tiff", ".pyc"}
EXCLUDED_NAMES = {
    ".DS_Store",
    ".Rhistory",
    ".RData",
    "code_inventory.csv",
    "data_inventory.csv",
    "files_sha256.csv",
    "package_summary.json",
}
CODE_SUFFIXES = {".r", ".py", ".mjs", ".js", ".ipynb", ".sh"}
DATA_SUFFIXES = {".csv", ".csv.gz", ".rds", ".json", ".parquet", ".xlsx"}


def include(path: Path) -> bool:
    rel = path.relative_to(ROOT)
    if any(part in EXCLUDED_DIRS for part in rel.parts):
        return False
    if path.name in EXCLUDED_NAMES:
        return False
    if path.suffix.lower() in EXCLUDED_SUFFIXES:
        return False
    return path.is_file()


def files() -> list[Path]:
    return sorted((p for p in ROOT.rglob("*") if include(p)), key=lambda p: p.as_posix())


def digest(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def compound_suffix(path: Path) -> str:
    name = path.name.lower()
    return ".csv.gz" if name.endswith(".csv.gz") else path.suffix.lower()


all_files = files()

with (MANIFEST / "code_inventory.csv").open("w", newline="", encoding="utf-8") as handle:
    writer = csv.writer(handle)
    writer.writerow(["relative_path", "bytes", "language_or_type"])
    for path in all_files:
        suffix = compound_suffix(path)
        if suffix in CODE_SUFFIXES:
            writer.writerow([path.relative_to(ROOT).as_posix(), path.stat().st_size, suffix.lstrip(".")])

with (MANIFEST / "data_inventory.csv").open("w", newline="", encoding="utf-8") as handle:
    writer = csv.writer(handle)
    writer.writerow(["relative_path", "bytes", "format", "role"])
    for path in all_files:
        suffix = compound_suffix(path)
        if suffix in DATA_SUFFIXES:
            rel = path.relative_to(ROOT).as_posix()
            if "/source_data/input/" in f"/{rel}":
                role = "frozen analysis input"
            elif "/source_data/" in f"/{rel}":
                role = "derived source data"
            elif "/tables/" in f"/{rel}":
                role = "reported table"
            elif "/qa/" in f"/{rel}":
                role = "quality assurance"
            else:
                role = "supporting data"
            writer.writerow([rel, path.stat().st_size, suffix.lstrip("."), role])

# Write the package summary before calculating checksums so the checksum entry
# always describes the final summary content from the same manifest pass.
all_files = files()
summary = {
    "package": "stable-coin-master-thesis-research",
    "public_file_count_excluding_checksum_manifest": len(all_files),
    "public_bytes_excluding_checksum_manifest": sum(path.stat().st_size for path in all_files),
    "largest_public_file_bytes": max((path.stat().st_size for path in all_files), default=0),
    "raw_coingecko_included": False,
    "checksum_algorithm": "SHA-256",
}
with (MANIFEST / "package_summary.json").open("w", encoding="utf-8") as handle:
    json.dump(summary, handle, indent=2, sort_keys=True)
    handle.write("\n")

# Refresh once more so the final summary and inventories are hashed.
all_files = files()
with (MANIFEST / "files_sha256.csv").open("w", newline="", encoding="utf-8") as handle:
    writer = csv.writer(handle)
    writer.writerow(["relative_path", "bytes", "sha256"])
    for path in all_files:
        writer.writerow([path.relative_to(ROOT).as_posix(), path.stat().st_size, digest(path)])

print(json.dumps(summary, indent=2, sort_keys=True))
