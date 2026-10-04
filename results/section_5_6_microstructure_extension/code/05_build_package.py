"""Build the portable Chapter 5.6 replication archive without local dependencies."""

from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile


HERE = Path(__file__).resolve().parent.parent
DESTINATION = HERE.parent / "Chapter_5_Microstructure_Extension.zip"
EXCLUDED_DIRS = {"node_modules", "checkpoints", "__pycache__"}
EXCLUDED_NAMES = {"Comparison_Workbook_Preview.png"}


def included(path: Path) -> bool:
    relative = path.relative_to(HERE)
    return (
        path.is_file()
        and not path.is_symlink()
        and not any(part in EXCLUDED_DIRS for part in relative.parts)
        and path.name not in EXCLUDED_NAMES
        and not path.name.endswith(".inspect.ndjson")
    )


files = sorted(path for path in HERE.rglob("*") if included(path))
with ZipFile(DESTINATION, "w", compression=ZIP_DEFLATED, compresslevel=6) as archive:
    for path in files:
        archive.write(path, arcname=Path(HERE.name) / path.relative_to(HERE))

print(f"Archive: {DESTINATION}")
print(f"Files: {len(files)}")
print(f"Size: {DESTINATION.stat().st_size:,} bytes")
