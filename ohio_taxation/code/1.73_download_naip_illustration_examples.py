from __future__ import annotations

import argparse
import csv
import math
import time
from io import BytesIO
from pathlib import Path
from typing import Any

import numpy as np
from PIL import Image, ImageDraw, ImageFont


ROOT = Path("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation")
SAT_DIR = ROOT / "data" / "roads" / "satellite_images"
OUT_DIR = ROOT / "data" / "roads" / "satellite_images_illustration_examples"
MANIFEST_CSV = SAT_DIR / "satellite_images_manifest.csv"
CONVNEXT_CSV = SAT_DIR / "naip_preds_convnext.csv"
YOLO_CSV = SAT_DIR / "naip_preds_yolo.csv"

GEE_PROJECT = "ohioroads"
NAIP_COLLECTION = "USDA/NAIP/DOQQ"

# Figure-only defaults. The analysis images remain 512 px over 96 m.
DEFAULT_TILE_SIZE_PX = 1024
DEFAULT_TILE_SIZE_M = 72
JPEG_SAVE_QUALITY = 98
JPEG_SUBSAMPLING = 0
GEE_THUMB_FORMAT = "png"

DEFAULT_CLASSES = ("low_quality", "high_quality")
CLASS_ALIASES = {
    "0": "low_quality",
    "1": "medium_quality",
    "2": "high_quality",
    "low": "low_quality",
    "medium": "medium_quality",
    "high": "high_quality",
    "poor": "low_quality",
    "decent": "medium_quality",
    "good": "high_quality",
}

OFFSET_GRID_M = (
    (0.0, 0.0),
    (8.0, 0.0),
    (-8.0, 0.0),
    (0.0, 8.0),
    (0.0, -8.0),
    (8.0, 8.0),
    (8.0, -8.0),
    (-8.0, 8.0),
    (-8.0, -8.0),
)


def normalize_class(label: str) -> str:
    value = str(label).strip().lower()
    return CLASS_ALIASES.get(value, value)


def short_class(label: str) -> str:
    return label.replace("_quality", "")


def read_csv_lookup(path: Path, key: str = "filename") -> dict[str, dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return {row[key]: row for row in csv.DictReader(f)}


def read_joined_rows(
    classes: tuple[str, ...],
    min_prob: float,
    pre_metric_pool: int,
) -> list[dict[str, Any]]:
    manifest = read_csv_lookup(MANIFEST_CSV)
    convnext = read_csv_lookup(CONVNEXT_CSV)
    yolo = read_csv_lookup(YOLO_CSV) if YOLO_CSV.exists() else {}

    pending: dict[str, list[dict[str, Any]]] = {label: [] for label in classes}
    for filename, pred in convnext.items():
        pred_label = pred.get("pred_label", "")
        max_prob = float(pred.get("max_prob", "nan"))
        if pred_label not in classes or max_prob < min_prob:
            continue

        meta = manifest.get(filename)
        if meta is None:
            continue

        yolo_row = yolo.get(filename, {})
        source_path = SAT_DIR / filename
        if not source_path.exists():
            continue

        row = {
            "filename": filename,
            "source_path": source_path,
            "cosbidfp": meta.get("cosbidfp", ""),
            "namelsad": meta.get("namelsad", ""),
            "roadname": meta.get("roadname", ""),
            "year": int(float(meta["year"])),
            "lat": float(meta["lat"]),
            "lon": float(meta["lon"]),
            "pred_id": pred.get("pred_id", ""),
            "pred_label": pred_label,
            "max_prob": max_prob,
            "p0": float(pred.get("p0", "nan")),
            "p1": float(pred.get("p1", "nan")),
            "p2": float(pred.get("p2", "nan")),
            "yolo_pred_label": yolo_row.get("pred_label", ""),
            "yolo_max_prob": yolo_row.get("max_prob", ""),
        }
        row["yolo_agree"] = int(row["yolo_pred_label"] == row["pred_label"])
        pending[pred_label].append(row)

    rows: list[dict[str, Any]] = []
    for pred_label, candidates in pending.items():
        candidates.sort(
            key=lambda row: (
                int(row["yolo_agree"]),
                float(row["max_prob"]),
                int(row["year"]),
            ),
            reverse=True,
        )
        for row in candidates[:pre_metric_pool]:
            source_path = row["source_path"]
            row.update(existing_image_metrics(source_path))
            row["selection_score"] = selection_score(row)
            rows.append(row)
    return rows


def existing_image_metrics(path: Path) -> dict[str, float]:
    img = Image.open(path).convert("RGB")
    return image_metrics(img)


def image_metrics(img: Image.Image) -> dict[str, float]:
    sample = img.resize((256, 256), Image.Resampling.LANCZOS)
    arr = np.asarray(sample).astype(np.float32)
    gray = (
        0.299 * arr[:, :, 0]
        + 0.587 * arr[:, :, 1]
        + 0.114 * arr[:, :, 2]
    )

    center = gray[1:-1, 1:-1]
    lap = (
        gray[:-2, 1:-1]
        + gray[2:, 1:-1]
        + gray[1:-1, :-2]
        + gray[1:-1, 2:]
        - 4 * center
    )
    sharpness = float(np.var(lap))
    contrast = float(np.std(gray) / 255.0)
    brightness = float(np.mean(gray) / 255.0)
    black_pct = float(np.mean(np.all(arr < 5, axis=2)))
    white_pct = float(np.mean(np.all(arr > 250, axis=2)))
    saturation = float(np.mean((np.max(arr, axis=2) - np.min(arr, axis=2)) / 255.0))

    brightness_penalty = abs(brightness - 0.55)
    visual_score = (
        math.log1p(sharpness)
        + 2.0 * contrast
        + 0.75 * saturation
        - 7.0 * black_pct
        - 4.0 * white_pct
        - 2.0 * brightness_penalty
    )

    return {
        "sharpness": sharpness,
        "contrast": contrast,
        "brightness": brightness,
        "black_pct": black_pct,
        "white_pct": white_pct,
        "saturation": saturation,
        "visual_score": visual_score,
    }


def selection_score(row: dict[str, Any]) -> float:
    year_bonus = max(0.0, min(1.0, (float(row["year"]) - 2003.0) / 18.0))
    agreement_bonus = 0.20 if row.get("yolo_agree") else 0.0
    return (
        float(row["visual_score"])
        + 1.50 * float(row["max_prob"])
        + 0.35 * year_bonus
        + agreement_bonus
    )


def select_candidates(
    rows: list[dict[str, Any]],
    classes: tuple[str, ...],
    min_prob: float,
    candidate_pool: int,
    unique_roads: bool,
) -> dict[str, list[dict[str, Any]]]:
    selected: dict[str, list[dict[str, Any]]] = {}
    for pred_label in classes:
        candidates = [
            row for row in rows
            if row["pred_label"] == pred_label and float(row["max_prob"]) >= min_prob
        ]
        candidates.sort(key=lambda row: row["selection_score"], reverse=True)

        chosen: list[dict[str, Any]] = []
        seen: set[tuple[str, str]] = set()
        for row in candidates:
            road_key = (str(row["cosbidfp"]), str(row["roadname"]).lower())
            if unique_roads and road_key in seen:
                continue
            chosen.append(row)
            seen.add(road_key)
            if len(chosen) >= candidate_pool:
                break
        selected[pred_label] = chosen
    return selected


def init_ee() -> Any:
    try:
        import ee
    except ModuleNotFoundError as exc:
        raise SystemExit(
            "Missing Python package 'earthengine-api' (import name: ee). "
            "Run this script in the same environment used for the existing GEE download scripts, "
            "or install it with: pip install earthengine-api"
        ) from exc

    try:
        ee.Initialize(project=GEE_PROJECT)
    except Exception:
        ee.Authenticate()
        ee.Initialize(project=GEE_PROJECT)
    return ee


def get_naip_image_for_year(ee: Any, year: int, lat: float, lon: float) -> Any | None:
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


def offset_lat_lon(lat: float, lon: float, east_m: float, north_m: float) -> tuple[float, float]:
    lat2 = lat + north_m / 111_320.0
    lon2 = lon + east_m / (111_320.0 * math.cos(math.radians(lat)))
    return lat2, lon2


def download_tile(
    ee: Any,
    image: Any,
    lat: float,
    lon: float,
    tile_size_m: int,
    tile_size_px: int,
    timeout: int,
) -> Image.Image:
    import requests

    half = tile_size_m / 2.0
    point = ee.Geometry.Point([lon, lat])
    region = point.buffer(half).bounds()
    url = image.getThumbUrl({
        "region": region.getInfo(),
        "dimensions": f"{tile_size_px}x{tile_size_px}",
        "format": GEE_THUMB_FORMAT,
        "bands": ["R", "G", "B"],
        "min": 0,
        "max": 255,
    })
    response = requests.get(url, timeout=timeout)
    response.raise_for_status()
    return Image.open(BytesIO(response.content)).convert("RGB")


def acceptable_output(metrics: dict[str, float], max_black_pct: float, max_white_pct: float) -> bool:
    return metrics["black_pct"] <= max_black_pct and metrics["white_pct"] <= max_white_pct


def best_offset_tile(
    ee: Any,
    image: Any,
    row: dict[str, Any],
    tile_size_m: int,
    tile_size_px: int,
    timeout: int,
    max_black_pct: float,
    max_white_pct: float,
) -> tuple[Image.Image, dict[str, float], float, float, float, float] | None:
    best: tuple[Image.Image, dict[str, float], float, float, float, float] | None = None

    for east_m, north_m in OFFSET_GRID_M:
        lat2, lon2 = offset_lat_lon(row["lat"], row["lon"], east_m, north_m)
        try:
            img = download_tile(ee, image, lat2, lon2, tile_size_m, tile_size_px, timeout)
        except Exception as exc:
            print(f"    Offset ({east_m:.0f}, {north_m:.0f}) failed: {exc}")
            continue

        metrics = image_metrics(img)
        if not acceptable_output(metrics, max_black_pct, max_white_pct):
            continue

        candidate = (img, metrics, lat2, lon2, east_m, north_m)
        if best is None or metrics["visual_score"] > best[1]["visual_score"]:
            best = candidate
    return best


def safe_filename(row: dict[str, Any]) -> str:
    stem = Path(row["filename"]).stem
    return f"{stem}_naip_illustration.jpg"


def write_manifest(path: Path, rows: list[dict[str, Any]]) -> None:
    fieldnames = [
        "quality",
        "filename",
        "source_filename",
        "output_path",
        "cosbidfp",
        "namelsad",
        "roadname",
        "year",
        "lat",
        "lon",
        "selected_lat",
        "selected_lon",
        "offset_east_m",
        "offset_north_m",
        "pred_id",
        "pred_label",
        "max_prob",
        "yolo_pred_label",
        "yolo_max_prob",
        "yolo_agree",
        "source_visual_score",
        "output_visual_score",
        "output_sharpness",
        "output_contrast",
        "output_brightness",
        "output_black_pct",
        "output_white_pct",
        "tile_size_m",
        "tile_size_px",
        "size_kb",
    ]
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def write_contact_sheet(rows: list[dict[str, Any]], path: Path) -> None:
    if not rows:
        return

    thumb_w, thumb_h = 300, 300
    label_h = 72
    pad = 18
    cols = min(3, len(rows))
    sheet_rows = math.ceil(len(rows) / cols)
    sheet = Image.new(
        "RGB",
        (cols * thumb_w + (cols + 1) * pad, sheet_rows * (thumb_h + label_h) + (sheet_rows + 1) * pad),
        "white",
    )
    draw = ImageDraw.Draw(sheet)
    try:
        font = ImageFont.truetype("arial.ttf", 15)
    except Exception:
        font = ImageFont.load_default()

    for idx, row in enumerate(rows):
        img = Image.open(row["output_path"]).convert("RGB")
        img.thumbnail((thumb_w, thumb_h), Image.Resampling.LANCZOS)
        col = idx % cols
        sheet_row = idx // cols
        x = pad + col * (thumb_w + pad)
        y = pad + sheet_row * (thumb_h + label_h + pad)
        canvas = Image.new("RGB", (thumb_w, thumb_h), (245, 245, 245))
        canvas.paste(img, ((thumb_w - img.width) // 2, (thumb_h - img.height) // 2))
        sheet.paste(canvas, (x, y))

        label = (
            f"{row['quality'].upper()} p={float(row['max_prob']):.3f}, "
            f"{row['roadname']} {row['year']}"
        )
        location = f"{row['cosbidfp']} | score={float(row['output_visual_score']):.2f}"
        draw.text((x, y + thumb_h + 6), label[:44], fill=(0, 0, 0), font=font)
        draw.text((x, y + thumb_h + 25), location[:44], fill=(0, 0, 0), font=font)
        draw.text((x, y + thumb_h + 44), Path(row["filename"]).name[:44], fill=(0, 0, 0), font=font)

    sheet.save(path, quality=95, subsampling=0)


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Download higher-resolution NAIP road chips for manuscript illustration only. "
            "This script reads the existing NAIP manifest/predictions but writes to a separate output folder."
        )
    )
    parser.add_argument("--out-dir", type=Path, default=OUT_DIR)
    parser.add_argument("--per-class", type=int, default=4)
    parser.add_argument("--classes", nargs="+", default=list(DEFAULT_CLASSES))
    parser.add_argument("--min-prob", type=float, default=0.70)
    parser.add_argument(
        "--pre-metric-pool",
        type=int,
        default=600,
        help="Maximum high-confidence source chips per class to open and visually score before GEE downloads.",
    )
    parser.add_argument("--candidate-pool", type=int, default=35)
    parser.add_argument("--tile-size-px", type=int, default=DEFAULT_TILE_SIZE_PX)
    parser.add_argument("--tile-size-m", type=int, default=DEFAULT_TILE_SIZE_M)
    parser.add_argument("--max-black-pct", type=float, default=0.02)
    parser.add_argument("--max-white-pct", type=float, default=0.12)
    parser.add_argument("--timeout", type=int, default=60)
    parser.add_argument("--sleep", type=float, default=0.30)
    parser.add_argument("--allow-duplicate-roads", action="store_true")
    parser.add_argument("--dry-run", action="store_true", help="Rank candidates without calling Google Earth Engine.")
    return parser


def main() -> None:
    args = build_arg_parser().parse_args()
    classes = tuple(normalize_class(label) for label in args.classes)

    print("Loading existing NAIP manifest and predictions...")
    rows = read_joined_rows(
        classes=classes,
        min_prob=args.min_prob,
        pre_metric_pool=args.pre_metric_pool,
    )
    print(f"  Candidate source images visually scored: {len(rows):,}")

    selected = select_candidates(
        rows,
        classes=classes,
        min_prob=args.min_prob,
        candidate_pool=args.candidate_pool,
        unique_roads=not args.allow_duplicate_roads,
    )

    args.out_dir.mkdir(parents=True, exist_ok=True)
    dry_run_path = args.out_dir / "candidate_rankings.csv"
    ranking_rows: list[dict[str, Any]] = []
    for pred_label, candidates in selected.items():
        print(f"  {pred_label}: {len(candidates)} ranked candidates")
        for rank, row in enumerate(candidates, start=1):
            ranking_rows.append({
                "rank": rank,
                "quality": short_class(pred_label),
                "filename": row["filename"],
                "cosbidfp": row["cosbidfp"],
                "roadname": row["roadname"],
                "year": row["year"],
                "lat": f"{row['lat']:.8f}",
                "lon": f"{row['lon']:.8f}",
                "pred_label": row["pred_label"],
                "max_prob": f"{row['max_prob']:.6f}",
                "yolo_pred_label": row["yolo_pred_label"],
                "yolo_max_prob": row["yolo_max_prob"],
                "source_visual_score": f"{row['visual_score']:.6f}",
                "selection_score": f"{row['selection_score']:.6f}",
            })
    with dry_run_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(ranking_rows[0].keys()) if ranking_rows else ["rank"])
        writer.writeheader()
        writer.writerows(ranking_rows)
    print(f"  Wrote candidate rankings: {dry_run_path}")

    if args.dry_run:
        print("Dry run only; no GEE downloads attempted.")
        return

    ee = init_ee()
    written_rows: list[dict[str, Any]] = []

    for pred_label, candidates in selected.items():
        quality = short_class(pred_label)
        quality_dir = args.out_dir / quality
        quality_dir.mkdir(parents=True, exist_ok=True)
        saved_for_class = 0

        for row in candidates:
            if saved_for_class >= args.per_class:
                break

            print(f"Downloading {quality}: {row['filename']} p={row['max_prob']:.3f}")
            image = get_naip_image_for_year(ee, row["year"], row["lat"], row["lon"])
            if image is None:
                print("    No NAIP image for this year/location.")
                continue

            best = best_offset_tile(
                ee=ee,
                image=image,
                row=row,
                tile_size_m=args.tile_size_m,
                tile_size_px=args.tile_size_px,
                timeout=args.timeout,
                max_black_pct=args.max_black_pct,
                max_white_pct=args.max_white_pct,
            )
            if best is None:
                print("    No acceptable offset tile.")
                continue

            img, metrics, selected_lat, selected_lon, east_m, north_m = best
            out_path = quality_dir / safe_filename(row)
            img.save(out_path, "JPEG", quality=JPEG_SAVE_QUALITY, subsampling=JPEG_SUBSAMPLING)
            size_kb = out_path.stat().st_size / 1024.0

            written_rows.append({
                "quality": quality,
                "filename": out_path.name,
                "source_filename": row["filename"],
                "output_path": str(out_path),
                "cosbidfp": row["cosbidfp"],
                "namelsad": row["namelsad"],
                "roadname": row["roadname"],
                "year": row["year"],
                "lat": f"{row['lat']:.8f}",
                "lon": f"{row['lon']:.8f}",
                "selected_lat": f"{selected_lat:.8f}",
                "selected_lon": f"{selected_lon:.8f}",
                "offset_east_m": f"{east_m:.1f}",
                "offset_north_m": f"{north_m:.1f}",
                "pred_id": row["pred_id"],
                "pred_label": row["pred_label"],
                "max_prob": f"{row['max_prob']:.6f}",
                "yolo_pred_label": row["yolo_pred_label"],
                "yolo_max_prob": row["yolo_max_prob"],
                "yolo_agree": row["yolo_agree"],
                "source_visual_score": f"{row['visual_score']:.6f}",
                "output_visual_score": f"{metrics['visual_score']:.6f}",
                "output_sharpness": f"{metrics['sharpness']:.6f}",
                "output_contrast": f"{metrics['contrast']:.6f}",
                "output_brightness": f"{metrics['brightness']:.6f}",
                "output_black_pct": f"{metrics['black_pct']:.6f}",
                "output_white_pct": f"{metrics['white_pct']:.6f}",
                "tile_size_m": args.tile_size_m,
                "tile_size_px": args.tile_size_px,
                "size_kb": f"{size_kb:.1f}",
            })
            saved_for_class += 1
            print(f"    Saved {out_path} ({size_kb:.1f} KB)")
            time.sleep(args.sleep)

    manifest_path = args.out_dir / "illustration_examples_manifest.csv"
    contact_sheet_path = args.out_dir / "illustration_examples_contact_sheet.jpg"
    write_manifest(manifest_path, written_rows)
    write_contact_sheet(written_rows, contact_sheet_path)

    print(f"Wrote {len(written_rows)} illustration examples to {args.out_dir}")
    print(f"Manifest: {manifest_path}")
    if written_rows:
        print(f"Contact sheet: {contact_sheet_path}")


if __name__ == "__main__":
    main()
