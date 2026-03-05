#==========================================================================#
# File: 1.7_import_satellite_images_gee.py
# Author: Saani Rawat
# Date: 20 Feb 2026
# Description: Import NAIP satellite images from Google Earth Engine for
#              Ohio roads, mapped to TENDIGIT_FIPS (COSBIDFP00) from
#              Census 2010 county subdivision boundaries.
#
# Dependencies:
#   - Google Earth Engine Python API (earthengine-api)
#   - geopandas, shapely, requests, Pillow
#   - Road geometries: data/roads/ohio/oh_roads_by_cousub.geojson
#     (TIGER road lines already spatially joined to county subdivisions)
#
# Approach:
#   - Instead of random points in subdivision polygons, we sample points
#     ALONG actual TIGER road line geometries. This guarantees every
#     satellite tile is centered on a road — same strategy as
#     1.2_import_streetview_images_new.py.
#   - For each road segment, we interpolate evenly spaced points along
#     the LineString, then randomly sample from those.
#
# Notes:
#   - NAIP imagery is available for Ohio approximately every 2-3 years.
#     Typical Ohio NAIP years: 2004, 2006, 2008, 2010, 2011, 2013, 2015,
#     2017, 2019, 2021.
#   - Resolution: ~1 m/px (native NAIP). Tile size: 256x256 px = ~256m.
#   - Images are saved as JPEG true-color (RGB) for vision model input.
#   - Set TEST_MODE = True to download sample images first.
#     Set TEST_MODE = False to run the full batch after inspection.
#   - Uses C:/Users/rawatsa/AppData/Local/miniforge3/envs/geoai-mf/python.exe
#==========================================================================#

import ee
import geopandas as gpd
import numpy as np
import os
import requests
import time
import logging
import sys
import csv
import random
from PIL import Image
from io import BytesIO
from shapely.geometry import LineString, MultiLineString, Point, MultiPoint, Polygon, MultiPolygon, GeometryCollection, mapping

# ---- Configuration --------------------------------------------------------

random.seed(42)
np.random.seed(42)

# ** Set to False once you have inspected the test images and are ready **
TEST_MODE = False

# Paths
ROOT = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
ROADS_GEOJSON = os.path.join(ROOT, "data/roads/ohio/oh_roads_by_cousub.geojson")
OUT_DIR = os.path.join(ROOT, "data/roads/satellite_images")
TEST_OUT_DIR = os.path.join(ROOT, "data/roads/satellite_images_test")
MANIFEST_CSV = os.path.join(OUT_DIR, "satellite_images_manifest.csv")

# GEE project
GEE_PROJECT = "ohioroads"

# NAIP collection ID
NAIP_COLLECTION = "USDA/NAIP/DOQQ"

# Image settings
TILE_SIZE_PX = 256          # pixels per side
TILE_SIZE_M = 128            # ground area per side in meters (~128m)
                             # At 256px / 128m = ~0.5m/px effective resolution
                             # Roads (~6-10m wide) will be ~12-20 pixels wide

# Sampling settings
SAMPLES_PER_SUBDIVISION = 50   # road-centered points per county subdivision
MIN_YEAR = 2003                # earliest NAIP year to consider
MAX_YEAR = 2021                # latest NAIP year to consider

# Rate limiting (avoid GEE throttling)
SLEEP_BETWEEN_REQUESTS = 0.3   # seconds between image downloads

# ---- Initialize GEE -------------------------------------------------------

ee.Authenticate()  # only needed once; will open browser if not cached
ee.Initialize(project=GEE_PROJECT)

# ---- Load road geometries (already joined to county subdivisions) ----------

print("Loading road geometries (joined to county subdivisions)...")
gdf_roads = gpd.read_file(ROADS_GEOJSON)
print(f"  Loaded {len(gdf_roads)} road segments.")
print(f"  Columns: {list(gdf_roads.columns)}")
print(f"  CRS: {gdf_roads.crs}")

# Ensure CRS is EPSG:4326 (WGS84) for GEE compatibility
if gdf_roads.crs and gdf_roads.crs.to_epsg() != 4326:
    gdf_roads = gdf_roads.to_crs(epsg=4326)
    print("  Reprojected to EPSG:4326.")

# Get unique county subdivisions from the road data
unique_cosbidfps = gdf_roads["COSBIDFP00"].unique()
print(f"  Unique county subdivisions with roads: {len(unique_cosbidfps)}")


# ---- Helper functions ------------------------------------------------------

def extract_coords_from_geometry(geom):
    """
    Recursively extract (lat, lon) coordinate pairs from any geometry type.
    Handles: Point, MultiPoint, LineString, MultiLineString,
             Polygon, MultiPolygon, GeometryCollection.
    Same logic as 1.2_import_streetview_images_new.py.
    Returns a list of (lat, lon) tuples.
    """
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
    else:
        print(f"  Warning: skipping unhandled geometry type: {geom.geom_type}")

    return coords


def extract_lines_from_geometry(geom):
    """
    Recursively extract LineString objects from any geometry type.
    Used for interpolation along roads.
    """
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
    # Polygons/Points don't have lines to interpolate along
    return lines


def sample_points_along_roads(gdf_subset, n_points):
    """
    Sample n random points along road geometries in a GeoDataFrame subset.
    Points are guaranteed to lie ON or very near road geometries.

    Handles ALL geometry types (LineString, MultiLineString, Point,
    Polygon, GeometryCollection) — same as 1.2_import_streetview_images_new.py.

    For LineStrings: extracts vertices + interpolated midpoints (~50m spacing).
    For other types: extracts all vertex coordinates.
    Returns list of (lat, lon, fullname) tuples.
    """
    candidate_points = []

    for _, row in gdf_subset.iterrows():
        geom = row.geometry
        fullname = row.get("FULLNAME", "unknown")

        if geom is None or geom.is_empty:
            continue

        # 1) Extract all vertex coordinates from any geometry type
        vertex_coords = extract_coords_from_geometry(geom)
        for lat, lon in vertex_coords:
            candidate_points.append((lat, lon, fullname))

        # 2) For line geometries, also interpolate along the line
        lines = extract_lines_from_geometry(geom)
        for line in lines:
            if line.length == 0:
                continue
            # Interpolate at roughly every 50m (in degrees, ~0.0005)
            n_interp = max(2, int(line.length / 0.0005))
            for i in range(1, n_interp):
                frac = i / n_interp
                pt = line.interpolate(frac, normalized=True)
                candidate_points.append((pt.y, pt.x, fullname))

    if not candidate_points:
        return []

    # Deduplicate by rounding to ~1m precision (5 decimal places)
    seen = set()
    unique_points = []
    for lat, lon, name in candidate_points:
        key = (round(lat, 5), round(lon, 5))
        if key not in seen:
            seen.add(key)
            unique_points.append((lat, lon, name))

    # Random sample
    if len(unique_points) <= n_points:
        return unique_points
    return random.sample(unique_points, n_points)


def shapely_to_ee_geometry(geom):
    """Convert a Shapely geometry to an Earth Engine geometry."""
    return ee.Geometry(mapping(geom))


def get_naip_years_for_region(geometry_ee, min_year, max_year):
    """
    Query the NAIP collection to find which years have imagery
    for a given Earth Engine geometry.
    Returns a sorted list of unique years.
    """
    collection = (
        ee.ImageCollection(NAIP_COLLECTION)
        .filterBounds(geometry_ee)
        .filterDate(f"{min_year}-01-01", f"{max_year + 1}-01-01")
    )

    dates = collection.aggregate_array("system:time_start").getInfo()
    if not dates:
        return []

    years = sorted(set(
        time.gmtime(d / 1000).tm_year for d in dates
    ))
    return years


def get_naip_image_for_year(year, geometry_ee):
    """
    Get a mosaic NAIP image for a given year and region.
    Uses a buffered region to ensure full NAIP tile coverage at edges.
    Returns an ee.Image with R, G, B bands, or None if no imagery exists.
    """
    # Buffer the query region by 500m so edge tiles get full NAIP coverage
    buffered_region = geometry_ee.buffer(500)
    collection = (
        ee.ImageCollection(NAIP_COLLECTION)
        .filterBounds(buffered_region)
        .filterDate(f"{year}-01-01", f"{year + 1}-01-01")
        .select(["R", "G", "B"])
    )

    count = collection.size().getInfo()
    if count == 0:
        return None

    return collection.mosaic()


def is_tile_quality_ok(img, max_black_pct=0.10):
    """
    Check whether a downloaded tile has too many black (no-data) pixels.
    NAIP mosaic gaps produce black (0,0,0) pixels where no imagery exists.
    Returns True if the tile is usable, False if it should be discarded.
    """
    arr = np.array(img)
    # A pixel is "black" if ALL channels are below a small threshold
    black_mask = np.all(arr < 5, axis=-1)
    black_pct = black_mask.mean()
    return black_pct <= max_black_pct


def download_tile(image, lat, lon, tile_size_m):
    """
    Download a square tile centered at (lat, lon) from an ee.Image.
    Rejects tiles with >10% black pixels (NAIP mosaic edge gaps).
    Returns a PIL Image or None if the download fails or quality is poor.
    """
    half = tile_size_m / 2.0
    point = ee.Geometry.Point([lon, lat])
    region = point.buffer(half).bounds()

    try:
        url = image.getThumbUrl({
            "region": region.getInfo(),
            "dimensions": f"{TILE_SIZE_PX}x{TILE_SIZE_PX}",
            "format": "jpg",
            "bands": ["R", "G", "B"],
            "min": 0,
            "max": 255,
        })

        response = requests.get(url, timeout=30)
        if response.status_code == 200:
            img = Image.open(BytesIO(response.content))
            if not is_tile_quality_ok(img):
                print(f"    Discarded tile at ({lat:.6f}, {lon:.6f}): too many black pixels (NAIP edge gap)")
                return None
            return img
        else:
            print(f"    HTTP {response.status_code} for tile at ({lat:.6f}, {lon:.6f})")
            return None

    except Exception as e:
        print(f"    Error downloading tile at ({lat:.6f}, {lon:.6f}): {e}")
        return None


def save_tile(img, out_dir, cosbidfp, year, lat, lon, roadname=""):
    """
    Save a PIL image tile with a standardized filename.
    Returns the filename if saved, None otherwise.
    """
    safe_roadname = str(roadname).replace(" ", "_").replace("/", "_")
    filename = f"{cosbidfp}_{safe_roadname}_{year}_{lat:.6f}_{lon:.6f}.jpg"
    filepath = os.path.join(out_dir, filename)
    img.save(filepath, "JPEG", quality=95)
    return filename


#==========================================================================#
# TEST SECTION: Download sample images from ONE subdivision
# RUN THIS FIRST (TEST_MODE = True) to verify roads are visible!
#==========================================================================#

if TEST_MODE:
    print("\n" + "=" * 70)
    print("TEST SECTION: Downloading road-centered satellite images")
    print("=" * 70)

    # Create test output directory
    os.makedirs(TEST_OUT_DIR, exist_ok=True)

    # Try subdivisions until we find one with enough road points
    shuffled_cosbidfps = list(unique_cosbidfps)
    random.shuffle(shuffled_cosbidfps)

    test_points = []
    test_cosbidfp = None
    test_namelsad = None

    for candidate_cosbidfp in shuffled_cosbidfps:
        candidate_roads = gdf_roads[gdf_roads["COSBIDFP00"] == candidate_cosbidfp]
        candidate_name = candidate_roads.iloc[0]["NAMELSAD00"]
        candidate_points = sample_points_along_roads(candidate_roads, n_points=10)

        print(f"  Trying {candidate_name} ({candidate_cosbidfp}): "
              f"{len(candidate_roads)} road segments, {len(candidate_points)} sample points")

        if len(candidate_points) >= 5:
            test_cosbidfp = candidate_cosbidfp
            test_namelsad = candidate_name
            test_points = candidate_points
            break

    if not test_points:
        print("ERROR: Could not find any subdivision with extractable road points.")
        print("Check the geometry types in oh_roads_by_cousub.geojson.")
        sys.exit(1)

    print(f"\nSelected: {test_namelsad} (COSBIDFP00={test_cosbidfp})")
    print(f"  Sampled {len(test_points)} road-centered points:")
    for lat, lon, rname in test_points[:5]:
        print(f"    ({lat:.6f}, {lon:.6f}) — {rname}")

    # Find NAIP years using a representative road point
    test_lat, test_lon, _ = test_points[0]
    test_ee_point = ee.Geometry.Point([test_lon, test_lat])
    test_years = get_naip_years_for_region(test_ee_point, MIN_YEAR, MAX_YEAR)
    print(f"  Available NAIP years: {test_years}")

    # Download: 10 points x 2 most recent years = ~20 test images
    test_years_sample = test_years[-2:] if len(test_years) >= 2 else test_years
    test_manifest = []

    for year in test_years_sample:
        print(f"\n  Year {year}:")
        naip_img = get_naip_image_for_year(year, test_ee_point)

        if naip_img is None:
            print(f"    No NAIP imagery for year {year}. Skipping.")
            continue

        for lat, lon, roadname in test_points:
            tile = download_tile(naip_img, lat, lon, TILE_SIZE_M)
            if tile is not None:
                fname = save_tile(tile, TEST_OUT_DIR, test_cosbidfp, year, lat, lon, roadname)
                test_manifest.append({
                    "cosbidfp": test_cosbidfp,
                    "namelsad": test_namelsad,
                    "roadname": roadname,
                    "year": year,
                    "lat": lat,
                    "lon": lon,
                    "filename": fname,
                })
                print(f"    Saved: {fname}")
            time.sleep(SLEEP_BETWEEN_REQUESTS)

    print(f"\nTest complete. {len(test_manifest)} images saved to: {TEST_OUT_DIR}")
    print(">>> INSPECT THESE IMAGES. Roads should be visible in every tile. <<<")
    print(">>> If satisfied, set TEST_MODE = False and re-run for full batch. <<<")
    print("=" * 70)
    # sys.exit(0)


#==========================================================================#
# FULL BATCH: Download satellite images for ALL county subdivisions
#==========================================================================#

# ---- Setup logging --------------------------------------------------------

LOG_PATH = os.path.join(OUT_DIR, "satellite_download.log")
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler(sys.stdout),
    ],
)

start_time = time.time()

# ---- Create output directory -----------------------------------------------

os.makedirs(OUT_DIR, exist_ok=True)

# ---- Build set of already-downloaded cosbidfps for resume capability -------

existing_cosbidfps = set()
if os.path.exists(OUT_DIR):
    for f in os.listdir(OUT_DIR):
        if f.endswith(".jpg"):
            existing_cosbidfps.add(f.split("_")[0])

if existing_cosbidfps:
    logging.info(f"Found {len(existing_cosbidfps)} previously downloaded subdivisions. Will skip them.")

# ---- Main loop: iterate over all county subdivisions -----------------------

total_saved = 0
total_subdivisions = len(unique_cosbidfps)

try:
    with open(MANIFEST_CSV, "a", newline="", encoding="utf-8") as manifest_file:
        manifest_writer = csv.DictWriter(
            manifest_file,
            fieldnames=["cosbidfp", "namelsad", "roadname", "year", "lat", "lon", "filename"],
        )
        if manifest_file.tell() == 0:
            manifest_writer.writeheader()

        for enum_idx, cosbidfp in enumerate(unique_cosbidfps, start=1):
            # Resume: skip already-downloaded subdivisions
            if cosbidfp in existing_cosbidfps:
                logging.info(f"[{enum_idx}/{total_subdivisions}] Skipping {cosbidfp} (already downloaded)")
                continue

            # Get all roads in this subdivision
            cosub_roads = gdf_roads[gdf_roads["COSBIDFP00"] == cosbidfp]
            namelsad = cosub_roads.iloc[0].get("NAMELSAD00", cosub_roads.iloc[0].get("NAME00", "unknown"))

            logging.info(
                f"[{enum_idx}/{total_subdivisions}] Processing {namelsad} "
                f"(COSBIDFP00={cosbidfp}, {len(cosub_roads)} road segments)"
            )

            try:
                # Sample points along actual road geometries
                sample_points = sample_points_along_roads(
                    cosub_roads, n_points=SAMPLES_PER_SUBDIVISION
                )

                if not sample_points:
                    logging.warning(f"  No road points for {cosbidfp}")
                    continue

                # Find NAIP years using a representative road point
                rep_lat, rep_lon, _ = sample_points[0]
                ee_point = ee.Geometry.Point([rep_lon, rep_lat])
                available_years = get_naip_years_for_region(ee_point, MIN_YEAR, MAX_YEAR)

                if not available_years:
                    logging.warning(f"  No NAIP imagery found for {cosbidfp}")
                    continue

                logging.info(f"  NAIP years: {available_years}, points: {len(sample_points)}")

                for year in available_years:
                    naip_img = get_naip_image_for_year(year, ee_point)

                    if naip_img is None:
                        logging.warning(f"  No NAIP imagery for {cosbidfp} in {year}")
                        continue

                    for lat, lon, roadname in sample_points:
                        tile = download_tile(naip_img, lat, lon, TILE_SIZE_M)
                        if tile is not None:
                            fname = save_tile(tile, OUT_DIR, cosbidfp, year, lat, lon, roadname)
                            manifest_writer.writerow({
                                "cosbidfp": cosbidfp,
                                "namelsad": namelsad,
                                "roadname": roadname,
                                "year": year,
                                "lat": lat,
                                "lon": lon,
                                "filename": fname,
                            })
                            total_saved += 1

                            if total_saved % 100 == 0:
                                manifest_file.flush()

                        time.sleep(SLEEP_BETWEEN_REQUESTS)

            except Exception as e:
                logging.error(f"  Error processing {cosbidfp} ({namelsad}): {e}")
                continue

except KeyboardInterrupt:
    logging.info("Interrupted by user. Progress has been saved.")

# ---- Summary ---------------------------------------------------------------

elapsed = time.time() - start_time
logging.info(f"Done. Saved {total_saved} satellite images in {elapsed:.1f}s")
logging.info(f"Manifest: {MANIFEST_CSV}")
logging.info(f"Images: {OUT_DIR}")

#==========================================================================#
# END OF FILE
#==========================================================================#
