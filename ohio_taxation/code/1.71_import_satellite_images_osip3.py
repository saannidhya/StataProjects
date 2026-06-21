# ========================================================================== #
# File: 1.71_import_satellite_images_osip3.py
# Author: Saani Rawat
# Date: 05 Mar 2026
# Description:
#   Download road-centered Ohio satellite image chips from OSIP III tile
#   downloads (OSIP3Downloads), using the same road-point sampling strategy
#   as 1.7_import_satellite_images_gee.py.
#
#   Pipeline summary:
#   - Sample points along TIGER road geometries by county subdivision.
#   - For each point, query OSIP3Downloads tile layers, preferring the
#     highest-resolution product (1.5-inch, then 3-inch, 6-inch, 1-foot).
#   - Build official ZIPARCHIVES_III download URLs from FOLDER/TILE metadata.
#   - Download/cache ZIPs locally, safely extract TIFF+TFW, and crop a fixed
#     ground-area chip centered on the road point.
#   - Apply visibility screens so heavily shadowed / clouded center patches are
#     rejected and nearby road points are retried before saving.
#   - Save model-ready JPEG chips with manifest + SHA256 hashes.
#
# Dependencies:
#   - geopandas, shapely, requests, Pillow, numpy
#
# Notes:
#   - This script uses only Ohio public services:
#       * OSIP3Downloads (tile index metadata)
#       * ZIPARCHIVES_III (imagery ZIP downloads)
#       * Ohio GeometryServer (point projection)
#   - Output files are stored locally with path/filename sanitization and
#     atomic writes.
#   - Set TEST_MODE = True to validate on one subdivision first.
# ========================================================================== #

import atexit
import csv
import hashlib
import json
import logging
import os
import random
import re
import shutil
import sys
import time
import zipfile
from collections import OrderedDict
from io import BytesIO
from pathlib import Path
from urllib.parse import quote

import geopandas as gpd
import numpy as np
import requests
from PIL import Image
from shapely.geometry import (
    GeometryCollection,
    LineString,
    MultiLineString,
    MultiPoint,
    MultiPolygon,
    Point,
    Polygon,
)

try:
    import rasterio
    from rasterio.windows import Window
except Exception:  # noqa: BLE001
    rasterio = None
    Window = None

try:
    from pyproj import CRS, Transformer
except Exception:  # noqa: BLE001
    CRS = None
    Transformer = None

# ---- Configuration -------------------------------------------------------- #

random.seed(42)
np.random.seed(42)

# Set to False after confirming test images.
TEST_MODE = False

ROOT = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
ROADS_GEOJSON = os.path.join(ROOT, "data/roads/ohio/oh_roads_by_cousub.geojson")

OUT_DIR = os.path.join(ROOT, "data/roads/satellite_images_osip3")
TEST_OUT_DIR = os.path.join(ROOT, "data/roads/satellite_images_osip3_test")
MANIFEST_CSV = os.path.join(OUT_DIR, "satellite_images_osip3_manifest.csv")
LOG_PATH = os.path.join(OUT_DIR, "satellite_download_osip3.log")

# Keep large intermediate ZIP/TIFF files out of OneDrive by default.
DEFAULT_LOCAL_CACHE_ROOT = os.path.join(
    os.environ.get("LOCALAPPDATA", os.path.expanduser("~")),
    "ohio_taxation_cache",
    "satellite_images_osip3_cache",
)
CACHE_DIR = os.environ.get("OSIP3_CACHE_DIR", DEFAULT_LOCAL_CACHE_ROOT)
DELETE_TILE_CACHE_IMMEDIATELY = False
MAX_SESSION_CACHED_TILES = 8
CLEANUP_CACHE_ON_EXIT = True

OSIP3_MAPSERVER = "https://maps.ohio.gov/arcgis/rest/services/OSIP3Downloads/MapServer"
ZIPARCHIVES_ROOT = "https://gis1.oit.ohio.gov/ZIPARCHIVES_III/IMAGERY"
GEOMETRY_SERVER_PROJECT = (
    "https://maps.ohio.gov/arcgis/rest/services/Utilities/Geometry/GeometryServer/project"
)

# Output chip settings
TILE_SIZE_PX = 512
GROUND_SIZE_M = 96.0
MAX_BLACK_PCT = 0.10
US_SURVEY_FOOT_PER_METER = 3.280833333333333

# JPEG output tuning for human inspection quality / ~200KB files
TARGET_JPEG_SIZE_KB = 200
TARGET_JPEG_TOLERANCE_KB = 25
MIN_JPEG_SIZE_PX = 448
MAX_JPEG_QUALITY = 96
MIN_JPEG_QUALITY = 90
JPEG_SUBSAMPLING = 0
JPEG_OPTIMIZE = True
JPEG_PROGRESSIVE = True

# Visibility screening tuned for the road-centered middle of each chip
CENTER_WINDOW_FRAC = 0.25
SHADOW_BRIGHTNESS_CUTOFF = 55.0
MAX_CENTER_SHADOW_PCT = 0.11
COMPOSITE_SHADOW_PCT = 0.08
COMPOSITE_DARK_BRIGHTNESS = 95.0
CLOUD_BRIGHTNESS_CUTOFF = 215.0
CLOUD_SATURATION_CUTOFF = 0.18
MAX_CENTER_CLOUD_PCT = 0.04

# Road sampling / retry settings
SAMPLE_SPACING_M = 25.0
SAMPLE_ENDPOINT_MARGIN_M = 12.0
ROAD_RETRY_OFFSETS_M = (12.0, 24.0)

# Sampling settings
SAMPLES_PER_SUBDIVISION = 50

# Network/retry settings
REQUEST_TIMEOUT = 60
MAX_RETRIES = 4
SLEEP_BETWEEN_REQUESTS = 0.2


# Product priority: highest resolution first
PRODUCTS = [
    {
        "code": "E1_5IN",
        "layer_id": 5,  # E_1_5IN_Tiles
        "prod_folder": "1_5INGEOTIFF",
        "enhanced": True,
    },
    {
        "code": "E3IN",
        "layer_id": 4,  # E_3IN_Tiles
        "prod_folder": "3INGEOTIFF",
        "enhanced": True,
    },
    {
        "code": "E6IN",
        "layer_id": 2,  # E_6IN_Tiles
        "prod_folder": "6INGEOTIFF",
        "enhanced": True,
    },
    {
        "code": "E1FT",
        "layer_id": 3,  # E_1FT_Tiles
        "prod_folder": "1FTGEOTIFF",
        "enhanced": True,
    },
    {
        "code": "S6IN",
        "layer_id": 1,  # S_6IN_Tiles
        "prod_folder": "6INGEOTIFF",
        "enhanced": False,
    },
]


if hasattr(Image, "Resampling"):
    RESAMPLE_LANCZOS = Image.Resampling.LANCZOS
else:
    RESAMPLE_LANCZOS = Image.LANCZOS


SESSION_TILE_CACHE = OrderedDict()


# ---- Utility helpers ------------------------------------------------------ #

def safe_join(base_dir, *paths):
    """
    Join paths and ensure the final path stays inside base_dir.
    Prevents accidental path traversal on local writes.
    """
    base_abs = os.path.abspath(base_dir)
    candidate = os.path.abspath(os.path.join(base_abs, *paths))
    if os.path.commonpath([base_abs, candidate]) != base_abs:
        raise ValueError(f"Unsafe path outside base dir: {candidate}")
    return candidate


def remove_path_quiet(path):
    """
    Remove a file or directory if it exists.
    """
    if not path:
        return
    try:
        if os.path.isdir(path):
            shutil.rmtree(path, ignore_errors=True)
        elif os.path.exists(path):
            os.remove(path)
    except Exception:  # noqa: BLE001
        pass


def sanitize_component(text, fallback="unknown", max_len=80):
    """
    Sanitize a filename component to a safe ASCII subset.
    """
    if text is None:
        text = fallback
    text = str(text)
    text = re.sub(r"[^A-Za-z0-9._-]+", "_", text).strip("._-")
    if not text:
        text = fallback
    return text[:max_len]


def parse_year(value):
    if value is None:
        return None
    m = re.search(r"\d{4}", str(value))
    if not m:
        return None
    try:
        return int(m.group(0))
    except ValueError:
        return None


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def encode_jpeg_bytes(img, quality):
    """
    Encode image to JPEG bytes using tuned settings.
    """
    buf = BytesIO()
    img.save(
        buf,
        format="JPEG",
        quality=int(quality),
        subsampling=JPEG_SUBSAMPLING,
        optimize=JPEG_OPTIMIZE,
        progressive=JPEG_PROGRESSIVE,
    )
    return buf.getvalue()


def tune_image_for_target_size(img, target_kb=TARGET_JPEG_SIZE_KB, tolerance_kb=TARGET_JPEG_TOLERANCE_KB):
    """
    Tune JPEG quality, and only if still necessary, gently downscale toward the
    requested output size range. We do not upscale because the source chip is
    already model-ready and upscaling only inflates file size.

    Returns (final_image, jpeg_bytes).
    """
    target_bytes = int(target_kb * 1024)
    lower_bytes = max(1, int((target_kb - tolerance_kb) * 1024))
    upper_bytes = int((target_kb + tolerance_kb) * 1024)
    working = img
    best = None

    for _ in range(6):
        for quality in range(MAX_JPEG_QUALITY, MIN_JPEG_QUALITY - 1, -1):
            data = encode_jpeg_bytes(working, quality=quality)
            size_bytes = len(data)
            candidate = {
                "img": working,
                "data": data,
                "quality": quality,
                "size_bytes": size_bytes,
                "distance": abs(size_bytes - target_bytes),
            }

            if best is None or candidate["distance"] < best["distance"] or (
                candidate["distance"] == best["distance"] and candidate["quality"] > best["quality"]
            ):
                best = candidate

            if lower_bytes <= size_bytes <= upper_bytes:
                return working, data

        if best is not None and best["size_bytes"] <= upper_bytes:
            break

        if working.size[0] <= MIN_JPEG_SIZE_PX:
            break

        new_dim = max(MIN_JPEG_SIZE_PX, working.size[0] - 32)
        if new_dim >= working.size[0]:
            break
        working = working.resize((new_dim, new_dim), RESAMPLE_LANCZOS)

    return best["img"], best["data"]


def meters_to_lat_degrees(distance_m):
    """
    Approximate meters as latitude degrees. This is sufficient for short
    road-following retry offsets used to dodge shadows or tree cover.
    """
    return float(distance_m) / 111320.0


def dedupe_lat_lon_points(points):
    """
    Deduplicate (lat, lon) points at ~1m precision while preserving order.
    """
    deduped = []
    seen = set()
    for lat, lon in points:
        key = (round(lat, 5), round(lon, 5))
        if key in seen:
            continue
        seen.add(key)
        deduped.append((lat, lon))
    return deduped


def cleanup_tile_cache_entry(entry):
    """
    Delete one cached tile bundle (ZIP + extracted TIFF/TFW).
    """
    if not entry:
        return
    remove_path_quiet(entry.get("zip_path"))
    remove_path_quiet(entry.get("extract_dir"))


def remember_tile_in_session_cache(tile_key, zip_path, extract_dir):
    """
    Keep only a small rolling tile cache on local disk during the current run.
    This avoids repeated downloads for nearby points without accumulating
    hundreds of GB of TIFFs. This path is used only when immediate deletion
    is disabled.
    """
    if tile_key in SESSION_TILE_CACHE:
        SESSION_TILE_CACHE.pop(tile_key)

    SESSION_TILE_CACHE[tile_key] = {
        "zip_path": zip_path,
        "extract_dir": extract_dir,
    }

    while len(SESSION_TILE_CACHE) > MAX_SESSION_CACHED_TILES:
        _, old_entry = SESSION_TILE_CACHE.popitem(last=False)
        cleanup_tile_cache_entry(old_entry)


def cleanup_session_tile_cache():
    """
    Remove all cached ZIP/TIFF files created during this run.
    """
    for _, entry in list(SESSION_TILE_CACHE.items()):
        cleanup_tile_cache_entry(entry)
    SESSION_TILE_CACHE.clear()


def request_json(session, method, url, params=None, data=None, timeout=REQUEST_TIMEOUT):
    """
    HTTP JSON helper with bounded retries and backoff.
    """
    last_err = None
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            if method.upper() == "GET":
                resp = session.get(url, params=params, timeout=timeout)
            else:
                resp = session.post(url, data=data, timeout=timeout)
            resp.raise_for_status()
            payload = resp.json()
            if isinstance(payload, dict) and "error" in payload:
                raise RuntimeError(f"ArcGIS error: {payload['error']}")
            return payload
        except Exception as exc:  # noqa: BLE001
            last_err = exc
            if attempt < MAX_RETRIES:
                time.sleep(0.8 * attempt)
            else:
                raise RuntimeError(f"Request failed after {MAX_RETRIES} attempts: {url}") from last_err


def download_binary_atomic(session, url, out_path):
    """
    Download URL to out_path using a temporary file and atomic rename.
    """
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    if os.path.exists(out_path) and os.path.getsize(out_path) > 0:
        return

    tmp_path = out_path + ".part"
    if os.path.exists(tmp_path):
        os.remove(tmp_path)

    last_err = None
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            with session.get(url, stream=True, timeout=REQUEST_TIMEOUT) as resp:
                resp.raise_for_status()
                with open(tmp_path, "wb") as f:
                    for chunk in resp.iter_content(chunk_size=1024 * 1024):
                        if chunk:
                            f.write(chunk)
            os.replace(tmp_path, out_path)
            return
        except Exception as exc:  # noqa: BLE001
            last_err = exc
            if os.path.exists(tmp_path):
                os.remove(tmp_path)
            if attempt < MAX_RETRIES:
                time.sleep(1.0 * attempt)
            else:
                raise RuntimeError(f"Failed to download {url}") from last_err


def extract_tif_tfw_secure(zip_path, extract_dir, tile_code):
    """
    Extract TIFF + TFW from ZIP with path traversal guard.
    Returns (tif_path, tfw_path).
    """
    os.makedirs(extract_dir, exist_ok=True)
    tif_name = f"{tile_code}.tif"
    tfw_name = f"{tile_code}.tfw"

    tif_path = safe_join(extract_dir, tif_name)
    tfw_path = safe_join(extract_dir, tfw_name)

    if os.path.exists(tif_path) and os.path.exists(tfw_path):
        return tif_path, tfw_path

    with zipfile.ZipFile(zip_path, "r") as zf:
        names = zf.namelist()
        tif_member = None
        tfw_member = None
        for name in names:
            lower = name.lower()
            if lower.endswith(".tif") and Path(name).name.lower() == tif_name.lower():
                tif_member = name
            if lower.endswith(".tfw") and Path(name).name.lower() == tfw_name.lower():
                tfw_member = name

        if tif_member is None:
            for name in names:
                if name.lower().endswith(".tif"):
                    tif_member = name
                    break
        if tfw_member is None:
            for name in names:
                if name.lower().endswith(".tfw"):
                    tfw_member = name
                    break

        if tif_member is None or tfw_member is None:
            raise RuntimeError(f"ZIP missing required TIFF/TFW: {zip_path}")

        for member, out_name in ((tif_member, tif_name), (tfw_member, tfw_name)):
            member_path = Path(member)
            if member_path.is_absolute() or ".." in member_path.parts:
                raise RuntimeError(f"Unsafe ZIP member path: {member}")
            out_path = safe_join(extract_dir, out_name)
            tmp_path = out_path + ".part"
            with zf.open(member, "r") as src, open(tmp_path, "wb") as dst:
                shutil.copyfileobj(src, dst)
            os.replace(tmp_path, out_path)

    return tif_path, tfw_path


def read_world_file(tfw_path):
    """
    Parse .tfw values.
    Returns: (A, D, B, E, C, F)
    """
    vals = []
    with open(tfw_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                vals.append(float(line))
    if len(vals) != 6:
        raise RuntimeError(f"Invalid TFW (expected 6 lines): {tfw_path}")
    return tuple(vals)


def geo_to_pixel(x, y, world):
    """
    Convert map coordinates to pixel coordinates using world-file transform.
    world = (A, D, B, E, C, F)
    """
    a, d, b, e, c, f = world
    mat = np.array([[a, b], [d, e]], dtype=float)
    rhs = np.array([x - c, y - f], dtype=float)
    col, row = np.linalg.solve(mat, rhs)
    return float(col), float(row)


def evaluate_chip_quality(img, max_black_pct=MAX_BLACK_PCT):
    """
    Score visibility in the center of the chip, where the sampled road point
    should appear. Reject chips with too much black fill, cloud cover, or
    heavy shadow over the center patch.
    """
    arr = np.array(img.convert("RGB"), dtype=np.float32)
    gray = arr.mean(axis=2)
    black_mask = np.all(arr < 5, axis=-1)
    black_pct = float(black_mask.mean())

    height, width = gray.shape
    center_side = max(32, int(round(min(height, width) * CENTER_WINDOW_FRAC)))
    center_y0 = max(0, (height - center_side) // 2)
    center_x0 = max(0, (width - center_side) // 2)
    center = arr[center_y0:center_y0 + center_side, center_x0:center_x0 + center_side]
    center_gray = gray[center_y0:center_y0 + center_side, center_x0:center_x0 + center_side]

    center_max = center.max(axis=2)
    center_min = center.min(axis=2)
    center_sat = np.divide(
        center_max - center_min,
        np.maximum(center_max, 1.0),
        out=np.zeros_like(center_max, dtype=np.float32),
        where=center_max > 0,
    )

    center_shadow_pct = float((center_gray < SHADOW_BRIGHTNESS_CUTOFF).mean())
    center_cloud_pct = float(
        ((center_gray > CLOUD_BRIGHTNESS_CUTOFF) & (center_sat < CLOUD_SATURATION_CUTOFF)).mean()
    )
    center_brightness = float(center_gray.mean())

    too_dark_and_shadowed = (
        center_shadow_pct > COMPOSITE_SHADOW_PCT and center_brightness < COMPOSITE_DARK_BRIGHTNESS
    )
    ok = (
        black_pct <= max_black_pct
        and center_shadow_pct <= MAX_CENTER_SHADOW_PCT
        and center_cloud_pct <= MAX_CENTER_CLOUD_PCT
        and not too_dark_and_shadowed
    )

    shadow_score = max(0.0, 1.0 - (center_shadow_pct / max(MAX_CENTER_SHADOW_PCT, 1e-6)))
    cloud_score = max(0.0, 1.0 - (center_cloud_pct / max(MAX_CENTER_CLOUD_PCT, 1e-6)))
    black_score = max(0.0, 1.0 - (black_pct / max(max_black_pct, 1e-6)))
    brightness_score = min(1.0, max(0.0, (center_brightness - 70.0) / 50.0))
    score = 100.0 * (
        0.40 * shadow_score
        + 0.15 * cloud_score
        + 0.15 * black_score
        + 0.30 * brightness_score
    )

    return {
        "ok": ok,
        "score": float(score),
        "black_pct": black_pct,
        "center_shadow_pct": center_shadow_pct,
        "center_cloud_pct": center_cloud_pct,
        "center_brightness": center_brightness,
    }


def units_per_meter_from_epsg(epsg):
    """
    Return horizontal CRS units per meter for an EPSG code.
    """
    if epsg is None:
        return US_SURVEY_FOOT_PER_METER

    if CRS is None:
        # Known Ohio State Plane ft systems used in OSIP.
        if int(epsg) in (3734, 3735, 3753, 3754):
            return US_SURVEY_FOOT_PER_METER
        return 1.0

    try:
        crs = CRS.from_epsg(int(epsg))
        axis = crs.axis_info[0] if crs.axis_info else None
        unit_name = (axis.unit_name or "").lower() if axis else ""
        if "foot" in unit_name:
            if "us survey" in unit_name:
                return US_SURVEY_FOOT_PER_METER
            return 3.280839895013123
        return 1.0
    except Exception:  # noqa: BLE001
        if int(epsg) in (3734, 3735, 3753, 3754):
            return US_SURVEY_FOOT_PER_METER
        return 1.0


def get_tif_epsg(tif_path):
    """
    Read EPSG code from GeoTIFF metadata when available.
    """
    if rasterio is None:
        return None
    try:
        with rasterio.open(tif_path) as ds:
            if ds.crs is None:
                return None
            epsg = ds.crs.to_epsg()
            return int(epsg) if epsg is not None else None
    except Exception:  # noqa: BLE001
        return None


def project_point_pyproj(lon, lat, out_epsg, transformer_cache):
    """
    Project WGS84 lon/lat to target EPSG using pyproj.
    """
    if Transformer is None:
        raise RuntimeError("pyproj is unavailable")

    key = int(out_epsg)
    tr = transformer_cache.get(key)
    if tr is None:
        tr = Transformer.from_crs(4326, key, always_xy=True)
        transformer_cache[key] = tr
    x, y = tr.transform(lon, lat)
    return float(x), float(y)


# ---- Road sampling helpers (same strategy as 1.7) ------------------------ #

def extract_coords_from_geometry(geom):
    coords = []
    if geom is None or geom.is_empty:
        return coords

    if isinstance(geom, Point):
        coords.append((geom.y, geom.x))
    elif isinstance(geom, MultiPoint):
        for pt in geom.geoms:
            coords.append((pt.y, pt.x))
    elif isinstance(geom, LineString):
        for x, y in geom.coords:
            coords.append((y, x))
    elif isinstance(geom, MultiLineString):
        for line in geom.geoms:
            for x, y in line.coords:
                coords.append((y, x))
    elif isinstance(geom, Polygon):
        for x, y in geom.exterior.coords:
            coords.append((y, x))
    elif isinstance(geom, MultiPolygon):
        for poly in geom.geoms:
            for x, y in poly.exterior.coords:
                coords.append((y, x))
    elif isinstance(geom, GeometryCollection):
        for sub_geom in geom.geoms:
            coords.extend(extract_coords_from_geometry(sub_geom))
    return coords


def extract_lines_from_geometry(geom):
    lines = []
    if geom is None or geom.is_empty:
        return lines
    if isinstance(geom, LineString):
        lines.append(geom)
    elif isinstance(geom, MultiLineString):
        lines.extend(geom.geoms)
    elif isinstance(geom, GeometryCollection):
        for sub_geom in geom.geoms:
            lines.extend(extract_lines_from_geometry(sub_geom))
    return lines


def build_retry_points_for_line(line, frac):
    """
    Build nearby points along the same road segment. When the primary chip is
    obscured by trees/shadow, a small shift along the road often produces a
    clearer view while keeping the same road in frame.
    """
    if line.length <= 0:
        return []

    min_frac = 0.0
    endpoint_margin_deg = meters_to_lat_degrees(SAMPLE_ENDPOINT_MARGIN_M)
    if line.length > (2.0 * endpoint_margin_deg):
        min_frac = endpoint_margin_deg / line.length

    retry_points = []
    for offset_m in ROAD_RETRY_OFFSETS_M:
        offset_frac = meters_to_lat_degrees(offset_m) / line.length
        for direction in (-1.0, 1.0):
            alt_frac = frac + (direction * offset_frac)
            if alt_frac <= min_frac or alt_frac >= (1.0 - min_frac):
                continue
            pt = line.interpolate(alt_frac, normalized=True)
            retry_points.append((pt.y, pt.x))

    return dedupe_lat_lon_points(retry_points)


def sample_points_along_roads(gdf_subset, n_points):
    candidate_points = []

    for _, row in gdf_subset.iterrows():
        geom = row.geometry
        fullname = row.get("FULLNAME", "unknown")
        if geom is None or geom.is_empty:
            continue

        line_candidates = []
        for line in extract_lines_from_geometry(geom):
            if line.length == 0:
                continue
            spacing_deg = meters_to_lat_degrees(SAMPLE_SPACING_M)
            endpoint_margin_deg = meters_to_lat_degrees(SAMPLE_ENDPOINT_MARGIN_M)
            n_interp = max(3, int(np.ceil(line.length / max(spacing_deg, 1e-8))))
            min_frac = 0.0
            if line.length > (2.0 * endpoint_margin_deg):
                min_frac = endpoint_margin_deg / line.length

            for i in range(1, n_interp):
                frac = i / n_interp
                if frac <= min_frac or frac >= (1.0 - min_frac):
                    continue
                pt = line.interpolate(frac, normalized=True)
                line_candidates.append(
                    {
                        "lat": pt.y,
                        "lon": pt.x,
                        "roadname": fullname,
                        "alternates": build_retry_points_for_line(line, frac),
                    }
                )

        if line_candidates:
            candidate_points.extend(line_candidates)
            continue

        # Fallback for non-line or tiny geometries.
        for lat, lon in extract_coords_from_geometry(geom):
            candidate_points.append(
                {
                    "lat": lat,
                    "lon": lon,
                    "roadname": fullname,
                    "alternates": [],
                }
            )

    if not candidate_points:
        return []

    seen = set()
    unique_points = []
    for point in candidate_points:
        key = (round(point["lat"], 5), round(point["lon"], 5))
        if key not in seen:
            seen.add(key)
            unique_points.append(point)

    if len(unique_points) <= n_points:
        return unique_points
    return random.sample(unique_points, n_points)


# ---- OSIP logic ----------------------------------------------------------- #

def get_layer_wkid(session, layer_id):
    url = f"{OSIP3_MAPSERVER}/{layer_id}"
    payload = request_json(session, "GET", url, params={"f": "json"})

    source_sr = payload.get("sourceSpatialReference") or {}
    sr = payload.get("extent", {}).get("spatialReference", {})

    wkid = source_sr.get("latestWkid") or source_sr.get("wkid") or sr.get("latestWkid") or sr.get("wkid")
    if wkid is None:
        raise RuntimeError(f"Could not determine spatial reference for layer {layer_id}")
    return int(wkid)


def query_best_tile_for_point(session, lon, lat, product):
    url = f"{OSIP3_MAPSERVER}/{product['layer_id']}/query"
    params = {
        "f": "json",
        "where": "1=1",
        "geometryType": "esriGeometryPoint",
        "geometry": json.dumps({"x": lon, "y": lat}),
        "inSR": "4326",
        "spatialRel": "esriSpatialRelIntersects",
        "outFields": "OBJECTID,TILE,FOLDER,CollYear",
        "returnGeometry": "false",
        "orderByFields": "CollYear DESC",
        "resultRecordCount": "20",
    }
    payload = request_json(session, "GET", url, params=params)
    features = payload.get("features", [])
    if not features:
        return None

    def _year_or_zero(feature):
        attrs = feature.get("attributes", {})
        return parse_year(attrs.get("CollYear")) or 0

    best = max(features, key=_year_or_zero)
    attrs = best.get("attributes", {})
    tile = attrs.get("TILE")
    folder = attrs.get("FOLDER")
    year = parse_year(attrs.get("CollYear"))
    if not tile or not folder:
        return None

    return {
        "tile": str(tile),
        "folder": str(folder),
        "year": year,
        "objectid": attrs.get("OBJECTID"),
    }


def build_zip_url(product, folder, tile):
    prod_folder = product["prod_folder"]
    enh = "_ENHANCED/" if product["enhanced"] else ""
    folder_q = quote(folder, safe="_-().")
    tile_q = quote(tile, safe="_-().")
    return f"{ZIPARCHIVES_ROOT}/{prod_folder}/{enh}{folder_q}/{tile_q}.zip"


def project_point(session, lon, lat, out_wkid, project_cache):
    cache_key = (round(lon, 7), round(lat, 7), out_wkid)
    if cache_key in project_cache:
        return project_cache[cache_key]

    data = {
        "f": "json",
        "inSR": "4326",
        "outSR": str(out_wkid),
        "geometries": json.dumps(
            {
                "geometryType": "esriGeometryPoint",
                "geometries": [{"x": lon, "y": lat}],
            }
        ),
    }
    payload = request_json(session, "POST", GEOMETRY_SERVER_PROJECT, data=data)
    geoms = payload.get("geometries", [])
    if not geoms:
        raise RuntimeError(f"Projection failed for point ({lat}, {lon})")

    x = float(geoms[0]["x"])
    y = float(geoms[0]["y"])
    project_cache[cache_key] = (x, y)
    return x, y


def crop_chip_from_tif(tif_path, tfw_path, center_x, center_y, ground_size_m, out_size_px, tif_epsg=None):
    units_per_meter = units_per_meter_from_epsg(tif_epsg)
    half_size_units = (ground_size_m * units_per_meter) / 2.0
    x_min = center_x - half_size_units
    x_max = center_x + half_size_units
    y_min = center_y - half_size_units
    y_max = center_y + half_size_units

    # Preferred path: use GeoTIFF transform via rasterio (handles mixed OSIP CRSs correctly).
    if rasterio is not None and Window is not None:
        try:
            with rasterio.open(tif_path) as ds:
                b = ds.bounds
                if x_min < b.left or x_max > b.right or y_min < b.bottom or y_max > b.top:
                    return None

                row_tl, col_tl = ds.index(x_min, y_max)
                row_br, col_br = ds.index(x_max, y_min)

                row0 = min(row_tl, row_br)
                row1 = max(row_tl, row_br) + 1
                col0 = min(col_tl, col_br)
                col1 = max(col_tl, col_br) + 1

                if row0 < 0 or col0 < 0 or row1 > ds.height or col1 > ds.width:
                    return None
                if row1 - row0 < 2 or col1 - col0 < 2:
                    return None

                win = Window(col_off=col0, row_off=row0, width=col1 - col0, height=row1 - row0)
                # Keep only RGB for model input even if source has alpha/NIR.
                band_count = min(ds.count, 3)
                if band_count < 3:
                    return None
                arr = ds.read(indexes=list(range(1, band_count + 1)), window=win)
                if arr.size == 0:
                    return None
                arr = np.transpose(arr, (1, 2, 0))
                if arr.dtype != np.uint8:
                    arr = np.clip(arr, 0, 255).astype(np.uint8)
                if arr.shape[2] > 3:
                    arr = arr[:, :, :3]

                img = Image.fromarray(arr, mode="RGB")
                return img.resize((out_size_px, out_size_px), RESAMPLE_LANCZOS)
        except Exception as exc:  # noqa: BLE001
            logging.debug("Rasterio crop failed for %s; fallback to world-file crop (%s)", tif_path, exc)

    # Fallback path: world-file + PIL.
    world = read_world_file(tfw_path)
    corners = [
        geo_to_pixel(x_min, y_min, world),
        geo_to_pixel(x_min, y_max, world),
        geo_to_pixel(x_max, y_min, world),
        geo_to_pixel(x_max, y_max, world),
    ]
    cols = [c[0] for c in corners]
    rows = [c[1] for c in corners]

    left = int(np.floor(min(cols)))
    right = int(np.ceil(max(cols)))
    top = int(np.floor(min(rows)))
    bottom = int(np.ceil(max(rows)))

    with Image.open(tif_path) as tif_img:
        if tif_img.mode not in ("RGB", "RGBA"):
            tif_img = tif_img.convert("RGB")
        width, height = tif_img.size

        if left < 0 or top < 0 or right > width or bottom > height:
            return None
        if right - left < 2 or bottom - top < 2:
            return None

        crop = tif_img.crop((left, top, right, bottom)).convert("RGB")
        chip = crop.resize((out_size_px, out_size_px), RESAMPLE_LANCZOS)
        return chip


def cache_paths_for_tile(product_code, folder, tile):
    safe_folder = sanitize_component(folder, fallback="folder")
    safe_tile = sanitize_component(tile, fallback="tile")
    key = f"{product_code}__{safe_folder}__{safe_tile}"

    zip_path = safe_join(CACHE_DIR, "zips", key + ".zip")
    extract_dir = safe_join(CACHE_DIR, "tiles", key)
    return key, zip_path, extract_dir


def chip_result_sort_key(result):
    quality = result["quality"]
    return (
        1 if quality["ok"] else 0,
        round(quality["score"], 6),
        -result.get("candidate_rank", 0),
        -result.get("product_rank", 0),
    )


def fetch_chip_for_point(session, lat, lon, layer_wkid_cache, project_cache, transformer_cache=None):
    if transformer_cache is None:
        transformer_cache = {}

    best_result = None

    for product_rank, product in enumerate(PRODUCTS):
        tile_key = None
        zip_path = None
        extract_dir = None
        try:
            tile_info = query_best_tile_for_point(session, lon, lat, product)
            if tile_info is None:
                continue

            tile = tile_info["tile"]
            folder = tile_info["folder"]
            year = tile_info["year"]

            zip_url = build_zip_url(product, folder, tile)
            tile_key, zip_path, extract_dir = cache_paths_for_tile(product["code"], folder, tile)
            download_binary_atomic(session, zip_url, zip_path)
            tif_path, tfw_path = extract_tif_tfw_secure(zip_path, extract_dir, tile)

            tif_epsg = get_tif_epsg(tif_path)
            if tif_epsg is not None and Transformer is not None:
                center_x, center_y = project_point_pyproj(lon, lat, tif_epsg, transformer_cache)
            else:
                if product["layer_id"] not in layer_wkid_cache:
                    layer_wkid_cache[product["layer_id"]] = get_layer_wkid(session, product["layer_id"])
                wkid = layer_wkid_cache[product["layer_id"]]
                center_x, center_y = project_point(session, lon, lat, wkid, project_cache)

            chip = crop_chip_from_tif(
                tif_path=tif_path,
                tfw_path=tfw_path,
                center_x=center_x,
                center_y=center_y,
                ground_size_m=GROUND_SIZE_M,
                out_size_px=TILE_SIZE_PX,
                tif_epsg=tif_epsg,
            )
            if chip is None:
                continue
            quality = evaluate_chip_quality(chip)
            result = {
                "chip": chip,
                "quality": quality,
                "product_rank": product_rank,
                "meta": {
                    "product": product["code"],
                    "year": year,
                    "tile": tile,
                    "folder": folder,
                    "source_url": zip_url,
                },
            }

            if best_result is None or chip_result_sort_key(result) > chip_result_sort_key(best_result):
                best_result = result
        except Exception as exc:  # noqa: BLE001
            logging.warning(
                "Point (%0.6f, %0.6f) failed for %s: %s",
                lat,
                lon,
                product["code"],
                exc,
            )
            continue
        finally:
            if zip_path or extract_dir:
                if DELETE_TILE_CACHE_IMMEDIATELY:
                    cleanup_tile_cache_entry(
                        {
                            "zip_path": zip_path,
                            "extract_dir": extract_dir,
                        }
                    )
                elif tile_key is not None:
                    remember_tile_in_session_cache(tile_key, zip_path, extract_dir)

    if best_result is None:
        return None

    return {
        "chip": best_result["chip"],
        "quality": best_result["quality"],
        "product_rank": best_result["product_rank"],
        "meta": best_result["meta"],
    }


def fetch_best_chip_for_sample_point(
    session,
    sample_point,
    layer_wkid_cache,
    project_cache,
    transformer_cache=None,
):
    """
    Try the primary point first. If that chip is too obscured, retry a few
    nearby points on the same road segment and keep the best acceptable chip.
    """
    if transformer_cache is None:
        transformer_cache = {}

    candidate_points = [(sample_point["lat"], sample_point["lon"])]
    candidate_points.extend(sample_point.get("alternates", []))

    best_result = None
    for candidate_rank, (lat, lon) in enumerate(candidate_points):
        result = fetch_chip_for_point(
            session=session,
            lat=lat,
            lon=lon,
            layer_wkid_cache=layer_wkid_cache,
            project_cache=project_cache,
            transformer_cache=transformer_cache,
        )
        if result is None:
            continue

        result["candidate_rank"] = candidate_rank
        result["lat"] = lat
        result["lon"] = lon

        if best_result is None or chip_result_sort_key(result) > chip_result_sort_key(best_result):
            best_result = result

        if candidate_rank == 0 and result["quality"]["ok"]:
            break

    if best_result is None or not best_result["quality"]["ok"]:
        if best_result is not None:
            quality = best_result["quality"]
            logging.info(
                "Rejected road chip near (%0.6f, %0.6f): score=%0.1f shadow=%0.3f cloud=%0.3f brightness=%0.1f",
                sample_point["lat"],
                sample_point["lon"],
                quality["score"],
                quality["center_shadow_pct"],
                quality["center_cloud_pct"],
                quality["center_brightness"],
            )
        return None

    if best_result["candidate_rank"] > 0:
        logging.info(
            "Retry offset accepted for (%0.6f, %0.6f) -> (%0.6f, %0.6f) with score %0.1f",
            sample_point["lat"],
            sample_point["lon"],
            best_result["lat"],
            best_result["lon"],
            best_result["quality"]["score"],
        )

    return best_result


def save_chip_secure(img, out_dir, cosbidfp, roadname, product, year, lat, lon):
    safe_road = sanitize_component(roadname, fallback="road", max_len=40)
    safe_product = sanitize_component(product, fallback="prod", max_len=16)
    year_txt = str(year) if year else "unknown"
    filename = f"{cosbidfp}_{safe_road}_{safe_product}_{year_txt}_{lat:.6f}_{lon:.6f}.jpg"
    filename = sanitize_component(filename, fallback="chip") + ".jpg" if not filename.lower().endswith(".jpg") else filename
    path = safe_join(out_dir, filename)
    os.makedirs(out_dir, exist_ok=True)

    tmp = path + ".part"
    _, data = tune_image_for_target_size(img, TARGET_JPEG_SIZE_KB)
    # Ensure we save exactly what was size-tuned above.
    with open(tmp, "wb") as f:
        f.write(data)
    os.replace(tmp, path)

    return filename, sha256_file(path)


# ---- Main ---------------------------------------------------------------- #

def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    os.makedirs(TEST_OUT_DIR, exist_ok=True)
    os.makedirs(CACHE_DIR, exist_ok=True)
    if CLEANUP_CACHE_ON_EXIT:
        atexit.register(cleanup_session_tile_cache)

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[logging.FileHandler(LOG_PATH), logging.StreamHandler(sys.stdout)],
    )

    logging.info("Loading road geometries (joined to county subdivisions)...")
    gdf_roads = gpd.read_file(ROADS_GEOJSON)
    logging.info("Loaded %d road segments.", len(gdf_roads))
    logging.info("CRS: %s", gdf_roads.crs)

    if gdf_roads.crs and gdf_roads.crs.to_epsg() != 4326:
        gdf_roads = gdf_roads.to_crs(epsg=4326)
        logging.info("Reprojected road geometries to EPSG:4326.")

    unique_cosbidfps = gdf_roads["COSBIDFP00"].unique()
    logging.info("Unique county subdivisions with roads: %d", len(unique_cosbidfps))

    session = requests.Session()
    session.headers.update(
        {
            "User-Agent": "ohio-taxation-osip3-downloader/1.71",
            "Accept": "application/json,text/plain,*/*",
        }
    )

    layer_wkid_cache = {}
    project_cache = {}
    transformer_cache = {}

    # ---- Test mode -------------------------------------------------------- #
    if TEST_MODE:
        logging.info("=" * 70)
        logging.info("TEST MODE: Downloading road-centered OSIP3 chips")
        logging.info("=" * 70)

        shuffled = list(unique_cosbidfps)
        random.shuffle(shuffled)

        test_points = []
        test_cosbidfp = None
        test_namelsad = None

        for candidate in shuffled:
            subset = gdf_roads[gdf_roads["COSBIDFP00"] == candidate]
            if subset.empty:
                continue
            name = subset.iloc[0].get("NAMELSAD00", "unknown")
            pts = sample_points_along_roads(subset, n_points=10)
            logging.info(
                "Trying %s (%s): %d road segments, %d sample points",
                name,
                candidate,
                len(subset),
                len(pts),
            )
            if len(pts) >= 5:
                test_points = pts
                test_cosbidfp = candidate
                test_namelsad = name
                break

        if not test_points:
            logging.error("No test points found. Check input road geometries.")
            sys.exit(1)

        logging.info("Selected: %s (COSBIDFP00=%s)", test_namelsad, test_cosbidfp)
        test_manifest = []

        for point in test_points:
            result = fetch_best_chip_for_sample_point(
                session=session,
                sample_point=point,
                layer_wkid_cache=layer_wkid_cache,
                project_cache=project_cache,
                transformer_cache=transformer_cache,
            )
            if result is None:
                logging.warning("No visible chip for (%0.6f, %0.6f)", point["lat"], point["lon"])
                continue

            chip = result["chip"]
            meta = result["meta"]
            final_lat = result["lat"]
            final_lon = result["lon"]
            roadname = point["roadname"]

            fname, digest = save_chip_secure(
                chip,
                out_dir=TEST_OUT_DIR,
                cosbidfp=test_cosbidfp,
                roadname=roadname,
                product=meta["product"],
                year=meta["year"],
                lat=final_lat,
                lon=final_lon,
            )
            test_manifest.append(
                {
                    "cosbidfp": test_cosbidfp,
                    "namelsad": test_namelsad,
                    "roadname": roadname,
                    "year": meta["year"],
                    "product": meta["product"],
                    "folder": meta["folder"],
                    "tile": meta["tile"],
                    "lat": final_lat,
                    "lon": final_lon,
                    "filename": fname,
                    "sha256": digest,
                    "source_url": meta["source_url"],
                }
            )
            logging.info("Saved test chip: %s", fname)
            time.sleep(SLEEP_BETWEEN_REQUESTS)

        logging.info("TEST complete. %d images saved to %s", len(test_manifest), TEST_OUT_DIR)
        logging.info("Inspect these images, then set TEST_MODE=False for full batch.")
        sys.exit(0)

    # ---- Resume support --------------------------------------------------- #
    existing_cosbidfps = set()
    for f in os.listdir(OUT_DIR):
        if f.lower().endswith(".jpg"):
            existing_cosbidfps.add(f.split("_")[0])

    if existing_cosbidfps:
        logging.info(
            "Found %d previously downloaded subdivisions. They will be skipped.",
            len(existing_cosbidfps),
        )

    # ---- Full batch ------------------------------------------------------- #
    start_time = time.time()
    total_saved = 0
    total_subdivisions = len(unique_cosbidfps)

    with open(MANIFEST_CSV, "a", newline="", encoding="utf-8") as manifest_file:
        fieldnames = [
            "cosbidfp",
            "namelsad",
            "roadname",
            "year",
            "product",
            "folder",
            "tile",
            "lat",
            "lon",
            "filename",
            "sha256",
            "source_url",
        ]
        writer = csv.DictWriter(manifest_file, fieldnames=fieldnames)
        if manifest_file.tell() == 0:
            writer.writeheader()

        for idx, cosbidfp in enumerate(unique_cosbidfps, start=1):
            if cosbidfp in existing_cosbidfps:
                logging.info("[%d/%d] Skipping %s (already downloaded)", idx, total_subdivisions, cosbidfp)
                continue

            subset = gdf_roads[gdf_roads["COSBIDFP00"] == cosbidfp]
            if subset.empty:
                continue

            namelsad = subset.iloc[0].get("NAMELSAD00", subset.iloc[0].get("NAME00", "unknown"))
            logging.info(
                "[%d/%d] Processing %s (COSBIDFP00=%s, %d road segments)",
                idx,
                total_subdivisions,
                namelsad,
                cosbidfp,
                len(subset),
            )

            try:
                points = sample_points_along_roads(subset, n_points=SAMPLES_PER_SUBDIVISION)
                if not points:
                    logging.warning("  No sample points generated for %s", cosbidfp)
                    continue

                logging.info("  Sampled %d road-centered points", len(points))

                for point in points:
                    result = fetch_best_chip_for_sample_point(
                        session=session,
                        sample_point=point,
                        layer_wkid_cache=layer_wkid_cache,
                        project_cache=project_cache,
                        transformer_cache=transformer_cache,
                    )
                    if result is None:
                        continue

                    chip = result["chip"]
                    meta = result["meta"]
                    final_lat = result["lat"]
                    final_lon = result["lon"]
                    roadname = point["roadname"]

                    fname, digest = save_chip_secure(
                        chip,
                        out_dir=OUT_DIR,
                        cosbidfp=cosbidfp,
                        roadname=roadname,
                        product=meta["product"],
                        year=meta["year"],
                        lat=final_lat,
                        lon=final_lon,
                    )

                    writer.writerow(
                        {
                            "cosbidfp": cosbidfp,
                            "namelsad": namelsad,
                            "roadname": roadname,
                            "year": meta["year"],
                            "product": meta["product"],
                            "folder": meta["folder"],
                            "tile": meta["tile"],
                            "lat": final_lat,
                            "lon": final_lon,
                            "filename": fname,
                            "sha256": digest,
                            "source_url": meta["source_url"],
                        }
                    )
                    total_saved += 1
                    if total_saved % 100 == 0:
                        manifest_file.flush()

                    time.sleep(SLEEP_BETWEEN_REQUESTS)

            except Exception as exc:  # noqa: BLE001
                logging.error("  Error processing %s (%s): %s", cosbidfp, namelsad, exc)
                continue

    elapsed = time.time() - start_time
    logging.info("Done. Saved %d OSIP3 image chips in %.1fs", total_saved, elapsed)
    logging.info("Manifest: %s", MANIFEST_CSV)
    logging.info("Images: %s", OUT_DIR)
    logging.info("Cache: %s", CACHE_DIR)
    if CLEANUP_CACHE_ON_EXIT:
        cleanup_session_tile_cache()


if __name__ == "__main__":
    main()
