from __future__ import annotations

import csv
import os
from io import BytesIO
from pathlib import Path

import ee
import numpy as np
import requests
from PIL import Image


ROOT = Path("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation")
SAT_DIR = ROOT / "data" / "roads" / "satellite_images"
OUT_DIR = ROOT / "data" / "roads" / "satellite_images_figure_examples"
MANIFEST_CSV = SAT_DIR / "satellite_images_manifest.csv"
PREDICTIONS_CSV = SAT_DIR / "naip_preds_convnext.csv"
OUT_MANIFEST = OUT_DIR / "figure_examples_manifest.csv"

GEE_PROJECT = "ohioroads"
NAIP_COLLECTION = "USDA/NAIP/DOQQ"

# Use a denser figure-only export than the analysis set.
TILE_SIZE_PX = 640
TILE_SIZE_M = 96
GEE_THUMB_FORMAT = "png"
JPEG_SAVE_QUALITY = 98
JPEG_SUBSAMPLING = 0

EXAMPLES = [
    {"quality": "low", "filename": "3905187024_Main_St_2013_41.573953_-84.002016.jpg"},
    {"quality": "low", "filename": "3904121448_E_Central_Ave_2013_40.298865_-83.051072.jpg"},
    {"quality": "low", "filename": "3902139228_E_Main_St_2013_40.127979_-83.955752.jpg"},
    {"quality": "medium", "filename": "3901364962_Bellaire_Neffs_Rd_2021_40.020333_-80.792185.jpg"},
    {"quality": "medium", "filename": "3911937926_Blackrun_Rd_2021_40.098964_-82.169678.jpg"},
    {"quality": "medium", "filename": "3904118140_Riverside_Dr_2019_40.230895_-83.142134.jpg"},
    {"quality": "high", "filename": "3908174608_Washington_St_2010_40.361322_-80.612949.jpg"},
    {"quality": "high", "filename": "3913775206_Water_St_2010_40.862042_-84.138787.jpg"},
    {"quality": "high", "filename": "3911377504_Salem_Ave_2005_39.823018_-84.289717.jpg"},
]


def load_manifest_lookup(path: Path) -> dict[str, dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return {row["filename"]: row for row in csv.DictReader(f)}


def load_prediction_lookup(path: Path) -> dict[str, dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return {row["filename"]: row for row in csv.DictReader(f)}


def init_ee() -> None:
    try:
        ee.Initialize(project=GEE_PROJECT)
    except Exception:
        ee.Authenticate()
        ee.Initialize(project=GEE_PROJECT)


def get_naip_image_for_year(year: int, lat: float, lon: float) -> ee.Image | None:
    point = ee.Geometry.Point([lon, lat])
    buffered_region = point.buffer(500)
    collection = (
        ee.ImageCollection(NAIP_COLLECTION)
        .filterBounds(buffered_region)
        .filterDate(f"{year}-01-01", f"{year + 1}-01-01")
        .select(["R", "G", "B"])
    )

    if collection.size().getInfo() == 0:
        return None

    return collection.mosaic()


def is_tile_quality_ok(img: Image.Image, max_black_pct: float = 0.10) -> bool:
    arr = np.array(img)
    black_mask = np.all(arr < 5, axis=-1)
    return float(black_mask.mean()) <= max_black_pct


def download_tile(image: ee.Image, lat: float, lon: float) -> Image.Image | None:
    half = TILE_SIZE_M / 2.0
    point = ee.Geometry.Point([lon, lat])
    region = point.buffer(half).bounds()

    try:
        url = image.getThumbUrl({
            "region": region.getInfo(),
            "dimensions": f"{TILE_SIZE_PX}x{TILE_SIZE_PX}",
            "format": GEE_THUMB_FORMAT,
            "bands": ["R", "G", "B"],
            "min": 0,
            "max": 255,
        })
        response = requests.get(url, timeout=60)
        response.raise_for_status()
        img = Image.open(BytesIO(response.content)).convert("RGB")
        if not is_tile_quality_ok(img):
            return None
        return img
    except Exception:
        return None


def save_tile(img: Image.Image, quality_dir: Path, filename: str) -> Path:
    quality_dir.mkdir(parents=True, exist_ok=True)
    out_path = quality_dir / filename
    img.save(
        out_path,
        "JPEG",
        quality=JPEG_SAVE_QUALITY,
        subsampling=JPEG_SUBSAMPLING,
    )
    return out_path


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    manifest_lookup = load_manifest_lookup(MANIFEST_CSV)
    pred_lookup = load_prediction_lookup(PREDICTIONS_CSV)
    init_ee()

    written_rows: list[dict[str, str]] = []

    for example in EXAMPLES:
        filename = example["filename"]
        manifest_row = manifest_lookup.get(filename)
        pred_row = pred_lookup.get(filename)

        if manifest_row is None or pred_row is None:
            print(f"Skipping {filename}: missing manifest or prediction row.")
            continue

        lat = float(manifest_row["lat"])
        lon = float(manifest_row["lon"])
        year = int(float(manifest_row["year"]))
        image = get_naip_image_for_year(year, lat, lon)
        if image is None:
            print(f"Skipping {filename}: no NAIP imagery for {year}.")
            continue

        tile = download_tile(image, lat, lon)
        if tile is None:
            print(f"Skipping {filename}: download failed or black-pixel check failed.")
            continue

        quality_dir = OUT_DIR / example["quality"]
        out_path = save_tile(tile, quality_dir, filename)
        size_kb = out_path.stat().st_size / 1024.0

        written_rows.append({
            "quality": example["quality"],
            "filename": filename,
            "output_path": str(out_path),
            "cosbidfp": manifest_row["cosbidfp"],
            "roadname": manifest_row["roadname"],
            "year": manifest_row["year"],
            "lat": manifest_row["lat"],
            "lon": manifest_row["lon"],
            "pred_id": pred_row["pred_id"],
            "pred_label": pred_row["pred_label"],
            "max_prob": pred_row["max_prob"],
            "size_kb": f"{size_kb:.1f}",
        })
        print(f"Saved {out_path.name} ({size_kb:.1f} KB)")

    with OUT_MANIFEST.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "quality", "filename", "output_path", "cosbidfp", "roadname", "year",
                "lat", "lon", "pred_id", "pred_label", "max_prob", "size_kb"
            ],
        )
        writer.writeheader()
        writer.writerows(written_rows)

    print(f"Wrote {len(written_rows)} figure examples to {OUT_DIR}")
    print(f"Manifest: {OUT_MANIFEST}")


if __name__ == "__main__":
    main()
