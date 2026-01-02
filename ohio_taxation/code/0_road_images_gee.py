"""
Name: road_quality.py
Purpose: Use Google Earth Engine to collect Ohio satellite imagery data on roads.
Date: 2025-09-25
Log:
    - 2025-09-25: Created initial file with header
"""


import os, pandas as pd, geopandas as gpd, numpy as np
import ee # Google Earth Engine
import math
import time
from pathlib import Path
import rasterio
import numpy as np
from PIL import Image
import rasterio

# -----------------------
# 0) AUTH & CONFIG
# -----------------------

# first-run only
# ee.Authenticate()

# initialize project 
ee.Initialize(project='ohioroads')

# get my python working directory
os.getcwd()

# Your Ohio county-subdivision (COUSUB) asset uploaded from TIGER/Line
# OHIO_COUSUBS_ASSET = 'users/saannidhya/ohioroads/tl_2021_39_cousub'  
OHIO_COUSUBS_ASSET = 'projects/ohioroads/assets/tl_2021_39_cousub'
# OHIO_COUSUBS_ASSET = "ohioroads/tl_2021_39_cousub"

# Output Drive folder and export options
DRIVE_FOLDER = 'GEE_Roads_Exports'     # will be created if missing
EXPORT_CRS = None                      # None => native; or e.g. 'EPSG:3857' or 'EPSG:5070'
MAX_PIXELS = 1e13

# Road-mask params (buffer in meters)
BUFFER_M_NAIP = 12    # NAIP is ~1 m; narrower buffer
BUFFER_M_S2   = 18    # 10 m data
BUFFER_M_LS   = 25    # 30 m data

# Growing season window for optical data in Ohio (tweak as needed)
SEASON_START = '-04-01'
SEASON_END   = '-10-31'

# -------- BEST-SCENE SAMPLING --------
SAMPLES_PER_YEAR = 10     # how many random scenes to export per (cousub, year)
TOP_K_PER_YEAR   = 20     # draw from top-K best scenes after ranking
RANDOM_SEED      = 42

# Export look: by default do NOT mask to road centerlines (you want neighborhoods)
USE_ROAD_MASK_DEFAULT = False   # set True if you want the road-only corridor


# If your CSV has a 'years' column, we use that list; else we use min_year..max_year inclusive.
CSV_PATH = 'C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/outputs/tables/hp_rd_fips_within_mean_eff_bw.csv'  


# -----------------------
# 1) DATASETS
# -----------------------
NAIP_ID = 'USDA/NAIP/DOQQ'                                  # 1 m, many years since 2003. :contentReference[oaicite:7]{index=7}
S2_SR   = 'COPERNICUS/S2_SR_HARMONIZED'                      # 10 m SR (2017+). :contentReference[oaicite:8]{index=8}
LS5_SR  = 'LANDSAT/LT05/C02/T1_L2'                           # 30 m SR (1984–2012). QA bits documented. :contentReference[oaicite:9]{index=9}
LS7_SR  = 'LANDSAT/LE07/C02/T1_L2'                           # 30 m SR (1999–present; SLC-off after 2003).
LS8_SR  = 'LANDSAT/LC08/C02/T1_L2'                           # 30 m SR (2013–present). QA bits documented. :contentReference[oaicite:10]{index=10}
ROADS   = 'TIGER/2016/Roads'                                 # U.S. TIGER roads. :contentReference[oaicite:11]{index=11}



# -----------------------
# 2) HELPERS
# -----------------------
def get_cousub_feature(geoid_str):
    fc = ee.FeatureCollection(OHIO_COUSUBS_ASSET).filter(ee.Filter.eq('GEOID', geoid_str))
    feat = fc.first()
    return feat

def get_road_mask(geom, buffer_m=20):
    roads = ee.FeatureCollection(ROADS).filterBounds(geom)
    # Paint buffered roads into a raster mask (1 on roads, 0 elsewhere)
    roads_buf = roads.map(lambda f: f.buffer(buffer_m))
    mask = ee.Image(0).byte().paint(roads_buf, 1)
    return mask.rename('road_mask')

# Sentinel-2 cloud mask using QA60 (harmonized keeps legacy behavior). :contentReference[oaicite:12]{index=12}
def mask_s2_clouds(image):
    qa = image.select('QA60')
    opaque = 1 << 10
    cirrus = 1 << 11
    mask = qa.bitwiseAnd(opaque).eq(0).And(qa.bitwiseAnd(cirrus).eq(0))
    return image.updateMask(mask)

# Landsat C2 SR: mask clouds, cloud shadow, and snow using QA_PIXEL bits. :contentReference[oaicite:13]{index=13}
def mask_ls_c2_sr(image):
    qa = image.select('QA_PIXEL')
    cloud      = qa.bitwiseAnd(1 << 3).eq(0)   # bit 3: clouds==0 keep
    cloudshad  = qa.bitwiseAnd(1 << 4).eq(0)   # bit 4: cloud shadow==0 keep
    snow       = qa.bitwiseAnd(1 << 5).eq(0)   # bit 5: snow==0 keep
    return image.updateMask(cloud.And(cloudshad).And(snow))

# Scale reflectance to 0..1 for Landsat C2 SR (factor ~2.75e-05; offset -0.2). :contentReference[oaicite:14]{index=14}
def ls_scale_reflectance(img, rgb_bands):
    return img.select(rgb_bands).multiply(2.75e-05).add(-0.2).clamp(0, 1)

# Convert a reflectance image (0..1) to 8-bit RGB for ML-ready look
def to_uint8_rgb(img01, min_val=0.02, max_val=0.30):
    return img01.unitScale(min_val, max_val).multiply(255).clamp(0, 255).toUint8()

# Choose the best imagery for a given year and ROI (NAIP -> S2 -> Landsat)
def get_best_images_for_year(year, geom, samples=SAMPLES_PER_YEAR, top_k=TOP_K_PER_YEAR, seed=RANDOM_SEED):
    # 1) NAIP first
    naip = (ee.ImageCollection(NAIP_ID)
            .filterDate(f'{year}-01-01', f'{year}-12-31')
            .filterBounds(geom))
    if naip.size().getInfo() > 0:
        ranked = naip.map(lambda im: add_quality_naip(im, geom)) \
                     .filter(ee.Filter.gt('quality', 0)) \
                     .sort('quality', False)  # best first
        chosen = ee.ImageCollection(ranked.limit(top_k)).toList(top_k).shuffle(seed)
        n = min(samples, top_k)
        # convert to uint8 RGB (NAIP already 8-bit; ensure 3 bands)
        viz_list = ee.ImageCollection(chosen.slice(0, n)).map(
            lambda im: ee.Image(im).select(['R','G','B']).toUint8()
        ).toList(n)
        return {'images': viz_list, 'src': 'NAIP', 'scale': 1}

    # 2) Sentinel-2
    s2 = (ee.ImageCollection(S2_SR)
          .filterDate(f'{year}{SEASON_START}', f'{year}{SEASON_END}')
          .filterBounds(geom)
          .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', 60)))
    if s2.size().getInfo() > 0:
        ranked = s2.map(lambda im: add_quality_s2(im, geom)) \
                   .filter(ee.Filter.gt('valid_frac', 0.05)) \
                   .sort('quality', False)
        chosen = ee.ImageCollection(ranked.limit(top_k)).toList(top_k).shuffle(seed)
        n = min(samples, top_k)
        viz_list = ee.ImageCollection(chosen.slice(0, n)).map(
            lambda im: to_uint8_rgb(ee.Image(im))   # im already 0..1 RGB from add_quality_s2
        ).toList(n)
        return {'images': viz_list, 'src': 'S2', 'scale': 10}

    # 3) Landsat (pick sensor by year)
    if year <= 2011:
        coll_id, bands, scale = LS5_SR, ['SR_B3','SR_B2','SR_B1'], 30
    elif year == 2012:
        coll_id, bands, scale = LS7_SR, ['SR_B3','SR_B2','SR_B1'], 30
    else:
        coll_id, bands, scale = LS8_SR, ['SR_B4','SR_B3','SR_B2'], 30

    ls = (ee.ImageCollection(coll_id)
          .filterDate(f'{year}{SEASON_START}', f'{year}{SEASON_END}')
          .filterBounds(geom))
    if ls.size().getInfo() == 0:
        return None

    ranked = ls.map(lambda im: add_quality_ls(im, geom, bands)) \
               .filter(ee.Filter.gt('valid_frac', 0.05)) \
               .sort('quality', False)
    chosen = ee.ImageCollection(ranked.limit(top_k)).toList(top_k).shuffle(seed)
    n = min(samples, top_k)
    viz_list = ee.ImageCollection(chosen.slice(0, n)).map(
        lambda im: to_uint8_rgb(ee.Image(im))      # im already 0..1 RGB from add_quality_ls
    ).toList(n)
    return {'images': viz_list, 'src': 'LANDSAT', 'scale': scale}


def export_image(img, geom, scale, description, src_label, mask_to_roads=USE_ROAD_MASK_DEFAULT, buffer_override=None):
    if mask_to_roads:
        buffer_m = (buffer_override if buffer_override is not None
                    else (BUFFER_M_NAIP if src_label=='NAIP' else BUFFER_M_S2 if src_label=='S2' else BUFFER_M_LS))
        road_mask = get_road_mask(geom, buffer_m=buffer_m)
        img = img.updateMask(road_mask)
    out = img.clip(geom)
    task = ee.batch.Export.image.toDrive(
        image = out,
        description = description,
        folder = DRIVE_FOLDER,
        fileNamePrefix = description,
        region = geom,
        scale = scale,
        crs = EXPORT_CRS,
        maxPixels = MAX_PIXELS,
        fileFormat = 'GeoTIFF',
        formatOptions = {'cloudOptimized': True}
    )
    task.start()
    return task


# ---------- Image Quality checking ----------
def _luma_from_rgb(img, r, g, b):
    # 0.3R + 0.59G + 0.11B   (band names are passed in)
    return (img.select(r).multiply(0.30)
            .add(img.select(g).multiply(0.59))
            .add(img.select(b).multiply(0.11))).rename('luma')

def _std_over_roi(img_single_band, geom, scale):
    # std-dev as a proxy for detail/texture; downscaled for speed
    key = ee.String(img_single_band.bandNames().get(0))
    red = img_single_band.reduceRegion(
        reducer=ee.Reducer.stdDev(), geometry=geom, scale=scale, maxPixels=1e13
    )
    val = red.get(key)
    return ee.Number(ee.Algorithms.If(val, val, 0)) 

def _valid_fraction(mask_img, geom, scale):
    key = ee.String(mask_img.bandNames().get(0))
    red = mask_img.reduceRegion(
        reducer=ee.Reducer.mean(), geometry=geom, scale=scale, maxPixels=1e13
    )
    val = red.get(key)
    return ee.Number(ee.Algorithms.If(val, val, 0))   # 0 if null

def _num_prop(img, prop, default_val):
    v = img.get(prop)
    return ee.Number(ee.Algorithms.If(v, v, default_val))

def add_quality_naip(img, geom):
    # NAIP: use texture only (cloud-free)
    bands = img.bandNames()
    keep = ee.List(['R','G','B']).filter(ee.Filter.inList('item', bands))
    img_rgb = img.select(keep)
    luma = _luma_from_rgb(img_rgb, 'R','G','B')
    std  = _std_over_roi(luma, geom, 2)          # 2 m
    return img_rgb.set({'quality': std, 'valid_frac': 1})

def add_quality_s2(img, geom):
    masked = mask_s2_clouds(img)
    rgb01  = masked.select(['B4','B3','B2']).multiply(0.0001).clamp(0,1)
    luma   = _luma_from_rgb(rgb01, 'B4','B3','B2')
    std    = _std_over_roi(luma, geom, 20)       # 20 m sampling
    frac   = _valid_fraction(rgb01.select('B2').mask(), geom, 20)
    cld    = _num_prop(img, 'CLOUDY_PIXEL_PERCENTAGE', 100)
    # Higher is better
    quality = ee.Number(100).subtract(cld).multiply(frac).add(std.multiply(100))
    return rgb01.set({'quality': quality, 'valid_frac': frac})

def add_quality_ls(img, geom, rgb_bands):
    masked = mask_ls_c2_sr(img)
    rgb01  = ls_scale_reflectance(masked, rgb_bands)   # 0..1
    luma   = _luma_from_rgb(rgb01, rgb_bands[0], rgb_bands[1], rgb_bands[2])
    std    = _std_over_roi(luma, geom, 60)             # 60 m sampling
    frac   = _valid_fraction(rgb01.select(rgb_bands[0]).mask(), geom, 60)
    cld    = _num_prop(img, 'CLOUD_COVER', 100)
    quality = ee.Number(100).subtract(cld).multiply(frac).add(std.multiply(100))
    return rgb01.set({'quality': quality, 'valid_frac': frac})


# -----------------------
# 3) RUN: read CSV and export
# -----------------------
# Start timing
start_time = time.time()

df = pd.read_csv(CSV_PATH, dtype={'tendigit_fips': str})
df = df.rename(columns={'min_year': 'min_election_year', 'max_year': 'max_election_year'})
# df['min_year'] = 2005
df['min_year'] = 2021
df['max_year'] = 2022

# Ensure 10-digit zero-padded strings
df['tendigit_fips'] = df['tendigit_fips'].str.replace(r'\.0$', '', regex=True).str.zfill(10)

def parse_years(row):
  if 'years' in row and isinstance(row['years'], str):
    return [int(y) for y in row['years'].split(',') if str(y).strip().isdigit()]
  elif 'min_year' in row and 'max_year' in row and not math.isnan(row['min_year']) and not math.isnan(row['max_year']):
    mi, ma = int(row['min_year']), int(row['max_year'])
    return list(range(mi, ma+1))
  else:
    # Fallback: just try min and max if present, else skip
    ys = []
    if 'min_year' in row and not pd.isna(row['min_year']):
      ys.append(int(row['min_year']))
    if 'max_year' in row and not pd.isna(row['max_year']):
      ys.append(int(row['max_year']))
    return list(sorted(set(ys)))


tasks = []
# for idx, r in df.iterrows():
for idx, r in df[0:1].iterrows():
  geoid = r['tendigit_fips']
  # Look up the geographic feature (boundary polygon) for this FIPS code in the Ohio County Subdivisions datasetfrom TIGER/Line
  feat = get_cousub_feature(geoid)
  # Validate that the geographic feature exists in the dataset. getInfo() forces evaluation and returns None if feature not found  
  if feat.getInfo() is None:
    print(f'[WARN] GEOID {geoid} not found in your OHIO COUSUB asset. Skipping.')
    continue # Skip to next row if geography doesn't exist
  # Extract the geometry (polygon boundary) from the feature. This defines the spatial extent for image processing
  geom = feat.geometry()

  # Parse the years to process from the DataFrame row. Could be a comma-separated string or min/max year range
  years = parse_years(r)
  # Validate that we have years to process
  if not years:
    print(f'[WARN] No years found for GEOID {geoid}. Add a years column or min/max. Skipping.')
    continue # Skip to next row if no years found

  # Process each year individually (inner loop)
  for yr in years:
      result = get_best_images_for_year(yr, geom)
      if result is None:
          print(f'[INFO] No imagery available for {geoid} in {yr}.')
          continue

      imgs_list = result['images']     # ee.List of ee.Image
      src      = result['src']
      scale    = result['scale']

      n = ee.Number(imgs_list.size()).getInfo()
      for i in range(n):
          im   = ee.Image(imgs_list.get(i))
          idx  = ee.Algorithms.String(im.get('system:index'))
          t0   = ee.Date(im.get('system:time_start')).format('YYYYMMdd')
          # Build description with index/time so files are unique & traceable
          desc = f'OH_{geoid}_{yr}_{src}_scene{i:02d}_{t0.getInfo()}_{idx.getInfo()}'

          task = export_image(im, geom, scale, desc, src,
                              mask_to_roads=USE_ROAD_MASK_DEFAULT)
          tasks.append(task)
          print(f'[TASK] Started export: {desc}')

# End timing and format output
end_time = time.time()
elapsed_seconds = end_time - start_time

hours = int(elapsed_seconds // 3600)
minutes = int((elapsed_seconds % 3600) // 60)
seconds = int(elapsed_seconds % 60)

print(f'\nStarted {len(tasks)} export task(s). Monitor progress in the Earth Engine Code Editor Tasks tab or via ee.batch.Task.list().')
print(f'Total execution time: {hours:02d}:{minutes:02d}:{seconds:02d}')

# Started 2925 export task(s). Monitor progress in the Earth Engine Code Editor Tasks tab or via ee.batch.Task.list().
# Total execution time: 00:37:19
# ERROR: Too many tasks already in the queue (3000, limit 3000).
#   File "C:\Users\rawatsa\AppData\Local\Programs\Python\Python312\Lib\site-packages\ee\data.py", line 411, in _execute_cloud_call
#     raise _translate_cloud_exception(e)  # pylint: disable=raise-missing-from
#     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# ee.ee_exception.EEException: Too many tasks already in the queue (3000, limit 3000).

# -----------------------
# 4) convert tiffs to PNG 
# -----------------------



# pip install rasterio pillow
src_dir = Path("G:/My Drive/GEE_Roads_Exports")

png_dir = src_dir / "png"

# jpg_dir = src_dir / "jpg"
png_dir.mkdir(exist_ok=True)
# jpg_dir.mkdir(exist_ok=True)

for tif in src_dir.glob("*.tif"):
    with rasterio.open(tif) as ds:
        rgb = ds.read([1,2,3])                  # (3,H,W), uint8
        rgb = np.transpose(rgb, (1,2,0))        # (H,W,3)
        # preserve NoData/mask as transparency in PNG (optional)
        alpha = ds.read_masks(1)                # 0 where masked, 255 otherwise
        rgba = np.dstack([rgb, alpha])          # (H,W,4)

    # PNG with transparency
    Image.fromarray(rgba, mode="RGBA").save(png_dir / f"{tif.stem}.png")

    # JPEG cannot store transparency; fill masked pixels with white (or black=0)
    # rgb_filled = rgb.copy()
    # rgb_filled[alpha==0] = 255
    # Image.fromarray(rgb_filled, mode="RGB").save(jpg_dir / f"{tif.stem}.jpg", quality=95, subsampling=0)

