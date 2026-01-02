"""
Purpose: Import and parse YOLO prediction label files from street view images
Created by: Saani Rawat
Created on: 2025-12-25

Change Log:
- Initial creation: Parse txt files with embedded metadata in filenames
- Added ODOT PCR mapping and weights
- Added threshold optimization via grid search
"""

import os
import numpy as np
import pandas as pd
import re
from pathlib import Path
from PIL import Image

# YOLO class mapping from your fine-tuned model
CLASS_NAMES = {
    0: "D00",
    1: "D10",
    2: "D20",
    3: "D30",
    4: "D40",
    5: "D50",
    6: "D60",
    7: "D70",
    8: "D80",
    9: "D90",
}

# Human-readable defect labels (descriptions) for each class
CLASS_LABELS = {
    "D00": "longitudinal cracks",
    "D10": "transverse cracks",
    "D20": "alligator cracks",
    "D30": "repaired cracks",
    "D40": "potholes",
    "D50": "pedestrian crossing blurs",
    "D60": "lane line blurs",
    "D70": "manhole covers",
    "D80": "patchy road sections",
    "D90": "rutting",
}

# same mapping keyed by numeric class id (matches YOLO outputs)
CLASS_LABELS_BY_ID = {class_id: CLASS_LABELS[name] for class_id, name in CLASS_NAMES.items()}


# Define paths
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/"
labels_path = root + "data/roads/runs_ohio/yolo11n_cpu_ohio_pred_streetview/labels/"

# ORIGINAL images that were fed to the YOLO model
images_path = root + "data/roads/ohio/google streetview photos/"
pcr_path = root + "data/roads/PCR/"

class YOLOPredictionLabelParser:
    """Parse Ultralytics YOLO prediction label files (one .txt per image).

    Expected line format:
      class_id x_center y_center width height [conf]
    where coords are normalized to [0,1] relative to image width/height.
    """

    def __init__(self, class_names: dict[int, str], dedupe: bool = True):
        self.class_names = class_names
        self.dedupe = dedupe

    def parse_text(self, text: str, *, source: str = "") -> list[dict]:
        detections: list[dict] = []
        seen: set[tuple] = set()

        for line_no, raw in enumerate(text.splitlines(), start=1):
            line = raw.strip()
            if not line:
                continue

            parts = line.split()
            if len(parts) not in (5, 6):
                raise ValueError(
                    f"Unexpected YOLO label format in {source} at line {line_no}: "
                    f"expected 5 or 6 columns, got {len(parts)} -> {line!r}"
                )

            class_id = int(float(parts[0]))
            x_center, y_center, width, height = map(float, parts[1:5])
            conf = float(parts[5]) if len(parts) == 6 else float("nan")

            class_name = self.class_names.get(class_id, str(class_id))

            det_key = (class_id, x_center, y_center, width, height, conf)
            if self.dedupe and det_key in seen:
                continue
            seen.add(det_key)

            detections.append(
                {
                    "class_id": class_id,
                    "class_name": class_name,
                    "x_center": x_center,
                    "y_center": y_center,
                    "width": width,
                    "height": height,
                    "conf": conf,
                }
            )

        return detections

    def parse_file(self, path: Path) -> list[dict]:
        return self.parse_text(path.read_text(encoding="utf-8"), source=str(path))

    def summarize_counts(self, detections: list[dict]) -> dict:
        class_counts = {name: 0 for name in self.class_names.values()}
        for d in detections:
            cname = d["class_name"]
            class_counts[cname] = class_counts.get(cname, 0) + 1
        return {
            "n_detections": len(detections),
            "class_counts": class_counts,
        }


def _find_image_for_label(images_dir: Path, label_file: Path) -> Path | None:
    """Best-effort match label file to its corresponding ORIGINAL image."""
    stem = label_file.stem
    for ext in (".jpg", ".jpeg", ".png", ".webp"):
        candidate = images_dir / f"{stem}{ext}"
        if candidate.exists():
            return candidate
    return None


def _get_image_size(path: Path) -> tuple[int, int] | None:
    """Return (width, height) in pixels."""
    if path is None:
        return None
    with Image.open(path) as im:
        w, h = im.size
    return int(w), int(h)

def parse_filename(filename):
    """
    Parse filename to extract metadata.
    Example: 3900558562_Columbus_St_Orange_township_2019-07_40.91941420849277_-82.28154707233851_h359.53069565057126_p-15_f60.txt
    
    Returns dict with: tendigit_fips, street_name, township_name, date, lat, lon, heading, pitch, fov
    """
    # Remove .txt extension
    name = filename.replace('.txt', '')
    
    # Split by underscores
    parts = name.split('_')
    
    # First part is always tendigit_fips
    tendigit_fips = parts[0]
    
    # Find where coordinates start (they contain dots and start with latitude)
    # Coordinates pattern: starts with a number containing a dot (latitude)
    coord_pattern = r'^-?\d+\.\d+$'
    
    # Find the index where coordinates start
    coord_start_idx = None
    for i, part in enumerate(parts):
        if re.match(coord_pattern, part):
            coord_start_idx = i
            break
    
    if coord_start_idx is None:
        return None
    
    # Extract date (should be just before coordinates, format: YYYY-MM)
    date = parts[coord_start_idx - 1]
    
    # Extract coordinates and other params
    lat = float(parts[coord_start_idx])
    lon = float(parts[coord_start_idx + 1])
    
    # Extract heading (h prefix), pitch (p prefix), fov (f prefix)
    heading = float(parts[coord_start_idx + 2].replace('h', ''))
    pitch = float(parts[coord_start_idx + 3].replace('p', ''))
    fov = float(parts[coord_start_idx + 4].replace('f', ''))
    
    # Extract street name and township/city name (between fips and date)
    # Find "township" or "city" keyword to split street and jurisdiction
    middle_parts = parts[1:coord_start_idx - 1]
    
    # Find where 'township' or 'city' appears and include the part before it
    jurisdiction_idx = None
    for i, part in enumerate(middle_parts):
        if part.lower() in ('township', 'city'):
            jurisdiction_idx = i
            break
    
    if jurisdiction_idx is not None and jurisdiction_idx > 0:
        street_name = '_'.join(middle_parts[:jurisdiction_idx - 1])
        township_name = '_'.join(middle_parts[jurisdiction_idx - 1:])
    else:
        # If no keyword found, assume last part is the jurisdiction name
        street_name = '_'.join(middle_parts[:-1])
        township_name = middle_parts[-1]
    
    return {
        'filename': filename,
        'tendigit_fips': tendigit_fips,
        'street_name': street_name,
        'township_name': township_name,
        'date': date,
        'latitude': lat,
        'longitude': lon,
        'heading': heading,
        'pitch': pitch,
        'fov': fov
    }


# Get all txt files
labels_dir = Path(labels_path)
images_dir = Path(images_path)
txt_files = sorted([p.name for p in labels_dir.glob("*.txt")])

yolo_parser = YOLOPredictionLabelParser(CLASS_NAMES, dedupe=True)

# Parse all files
parsed_data = []
for txt_file in txt_files:
    parsed = parse_filename(txt_file)
    if parsed is not None:
        label_file = labels_dir / txt_file
        detections = yolo_parser.parse_file(label_file)
        summary = yolo_parser.summarize_counts(detections)

        # Match the ORIGINAL input image (preferred for area calculations)
        image_file = _find_image_for_label(images_dir, label_file)
        if image_file is not None:
            w_h = _get_image_size(image_file)
        else:
            w_h = None

        if w_h is not None:
            image_width, image_height = w_h

            # Enrich each detection with pixel geometry + area
            for d in detections:
                # normalized values
                x_center = float(d["x_center"])
                y_center = float(d["y_center"])
                width = float(d["width"])
                height = float(d["height"])

                d["area_norm"] = width * height
                d["area_px"] = (width * image_width) * (height * image_height)

                # Optional but handy for downstream work
                d["x1_px"] = (x_center - width / 2.0) * image_width
                d["y1_px"] = (y_center - height / 2.0) * image_height
                d["x2_px"] = (x_center + width / 2.0) * image_width
                d["y2_px"] = (y_center + height / 2.0) * image_height

        # Attach label parsing outputs (this is what you'll use to build PCR metrics)
        parsed["label_path"] = str(label_file)
        parsed["n_detections"] = summary["n_detections"]

        parsed["image_path"] = str(image_file) if image_file is not None else None
        parsed["image_width"] = int(w_h[0]) if w_h is not None else None
        parsed["image_height"] = int(w_h[1]) if w_h is not None else None

        # Store a compact summary plus the raw detections list
        parsed["class_counts"] = summary["class_counts"]
        parsed["detections"] = detections

        # Also flatten counts into scalar columns for easy DataFrame / Stata export
        for cname in CLASS_NAMES.values():
            parsed[f"count_{cname}"] = int(summary["class_counts"].get(cname, 0))

        parsed_data.append(parsed)


# =============================================================================
# LONG dataframe: one row per blemish (detection)
# =============================================================================

long_rows = []
for img in parsed_data:
    detections = img.get("detections", []) or []

    base = {
        "filename": img.get("filename"),
        "label_path": img.get("label_path"),
        "tendigit_fips": img.get("tendigit_fips"),
        "street_name": img.get("street_name"),
        "township_name": img.get("township_name"),
        "date": img.get("date"),
        "latitude": img.get("latitude"),
        "longitude": img.get("longitude"),
        "heading": img.get("heading"),
        "pitch": img.get("pitch"),
        "fov": img.get("fov"),
        "image_path": img.get("image_path"),
        "image_width": img.get("image_width"),
        "image_height": img.get("image_height"),
    }

    for det_idx, d in enumerate(detections):
        row = dict(base)
        row["detection_idx"] = det_idx

        # YOLO detection fields (normalized)
        row["class_id"] = d.get("class_id")
        row["class_name"] = d.get("class_name")
        row["conf"] = d.get("conf")
        row["x_center"] = d.get("x_center")
        row["y_center"] = d.get("y_center")
        row["width"] = d.get("width")
        row["height"] = d.get("height")
        row["area_norm"] = d.get("area_norm")

        # pixel geometry (may be missing if image lookup failed)
        row["area_px"] = d.get("area_px")
        row["x1_px"] = d.get("x1_px")
        row["y1_px"] = d.get("y1_px")
        row["x2_px"] = d.get("x2_px")
        row["y2_px"] = d.get("y2_px")

        long_rows.append(row)

df_long = pd.DataFrame(long_rows)

# Save next to the run folder (parent of labels/)
run_dir = labels_dir.parent
out_long_csv = run_dir / "pcr_detections_long.csv"
df_long.to_csv(out_long_csv, index=False)
print("Saved long detections CSV:", out_long_csv)

# -------------------------------------------------------------------------
# Derive year (from YYYY-MM) and county (from 10-digit FIPS-like code)
# -------------------------------------------------------------------------

# year from "2019-07" -> 2019 (nullable Int64 for safety)
df_long["year"] = (df_long["date"].astype(str).str.slice(0, 4).pipe(pd.to_numeric, errors="coerce").astype("Int64"))

# county from "3900558562" -> "39005" (state+county FIPS, zero-padded to 10 first)
_fips10 = (df_long["tendigit_fips"].astype(str).str.replace(r"\D+", "", regex=True).str.zfill(10))
df_long["county"] = _fips10.str.slice(2, 5)

df_long24 = df_long.loc[df_long["year"].eq(2024)].copy()


# Create DataFrame
# df = pd.DataFrame(parsed_data)

# Display summary
# print(f"Total files parsed: {len(df)}")
# print(f"\nFirst few rows:")
# print(df.head())
# print(f"\nDataFrame shape: {df.shape}")
# print(f"\nColumn names: {df.columns.tolist()}")

# parsed_data[0]
MODEL_TO_ODOT_MAP = {
    "D00": {
        "model_label": "longitudinal cracks",
        "odot_distress": "LONGITUDINAL CRACKING",
        "distress_weight": 5,
        "severity_weight": {"L": 0.4, "M": 0.7, "H": 1.0},
        "extent_weight": {"O": 0.5, "F": 0.7, "E": 1.0}
    },
    "D10": {
        "model_label": "transverse cracks",
        "odot_distress": "BLOCK & TRANSVERSE CRACKING", 
        "distress_weight": 10,
        "severity_weight": {"L": 0.4, "M": 0.7, "H": 1.0},
        "extent_weight": {"O": 0.5, "F": 0.7, "E": 1.0}
    },
    "D20": {
        "model_label": "alligator cracks",
        "odot_distress": "WHEEL TRACK CRACKING", 
        "distress_weight": 15,
        "severity_weight": {"L": 0.4, "M": 0.7, "H": 1.0},
        "extent_weight": {"O": 0.5, "F": 0.7, "E": 1.0}
        # Note: 'Alligator' is explicitly listed as a severity level for Wheel Track Cracking on the form.
    },
    "D30": {
        "model_label": "repaired cracks",
        "odot_distress": "CRACK SEALING DEFIC.",
        "distress_weight": 5,
        "severity_weight": {"L": 1.0, "M": 1.0, "H": 1.0},
        "extent_weight": {"O": 0.5, "F": 0.8, "E": 1.0}
    },
    "D40": {
        "model_label": "potholes",
        "odot_distress": "POTHOLES",
        "distress_weight": 10,
        "severity_weight": {"L": 0.4, "M": 0.8, "H": 1.0},
        "extent_weight": {"O": 0.5, "F": 0.8, "E": 1.0}
    },
    "D50": {
        "model_label": "pedestrian crossing blurs",
        "odot_distress": None,
        "distress_weight": None,
        "severity_weight": None,
        "extent_weight": None
    },
    "D60": {
        "model_label": "lane line blurs",
        "odot_distress": None,
        "distress_weight": None,
        "severity_weight": None,
        "extent_weight": None
    },
    "D70": {
        "model_label": "manhole covers",
        "odot_distress": None, 
        "distress_weight": None,
        "severity_weight": None,
        "extent_weight": None
        # Note: The form tracks "# of Utility Cuts" in the header, but it is not a weighted distress in the rating grid.
    },
    "D80": {
        "model_label": "patchy road sections",
        "odot_distress": "PATCHING",
        "distress_weight": 5,
        "severity_weight": {"L": 0.3, "M": 0.6, "H": 1.0},
        "extent_weight": {"O": 0.6, "F": 0.8, "E": 1.0}
    },
    "D90": {
        "model_label": "rutting",
        "odot_distress": "RUTTING",
        "distress_weight": 10,
        "severity_weight": {"L": 0.3, "M": 0.7, "H": 1.0},
        "extent_weight": {"O": 0.6, "F": 0.8, "E": 1.0}
    }
}

# ODOT distress with no mapping 
# Raveling, Bleeding, Debonding, Setllements, Edge Cracking, Thermal Cracking#

# Add distress weights by model class (keep in sync with MODEL_TO_ODOT_MAP below)
_distress_weight_by_class = {
    "D00": 5,
    "D10": 10,
    "D20": 15,
    "D30": 5,
    "D40": 10,
    "D50": None,
    "D60": None,
    "D70": None,
    "D80": 5,
    "D90": 10,
}
df_long24["distress_weight"] = (df_long24["class_name"].astype(str).map(_distress_weight_by_class).astype("Float64")/100)

# Show first 5 rows with all columns (no truncation)
# with pd.option_context(
#     "display.max_columns", None,
#     "display.width", None,
#     "display.max_colwidth", None,
# ):
#     print(df_long24.head(5))

# Optional: explicitly confirm column count / list
print("n_rows:", len(df_long24), "n_cols:", df_long24.shape[1])
print("columns:", df_long24.columns.tolist())

# Importing ODOT PCR Ratings
odot_pcr = pd.read_csv(Path(pcr_path) / "PCR.csv")
sorted(odot_pcr["PCR_YEAR"].dropna().unique().tolist()) # Only 2024 ODOTs

odot_pcr.columns.tolist()

cols_keep = ["NLFID","CTL_BEGIN","CTL_END","COUNTY_CD","TOWNSHIP_NAME","LEFT_TWP_NAME","PAVE_TYPE","SURFACE_TYPE","CITY_NAME","DIRECTION_CD","JURISDICTION","ROUTE_TYPE","PCR_NBR","TOT_PCR_DEDUCT_NBR"]
odot_pcr = odot_pcr.loc[:, cols_keep].copy()

odot_pcr["JURISDICTION"] = odot_pcr["JURISDICTION"].astype(str).str.strip()
odot_pcr["ROUTE_TYPE"] = odot_pcr["ROUTE_TYPE"].astype(str).str.strip()

odot_pcr = odot_pcr.loc[odot_pcr["JURISDICTION"].isin(["C", "M", "T"]) & odot_pcr["ROUTE_TYPE"].isin(["CR", "MR", "TR"])].copy()

# Main columns of interest: 
# 1. PCR_NBR#

# County codes in ODOT PCR dataset are 3-letter codes. Extracted by Gemini from here: https://highways.dot.gov/sites/fhwa.dot.gov/files/FHWA-HRT-25-114.pdf
# County code (3-letter) -> County name (ODOT)
odot_codes = {"ADA": "Adams","ALL": "Allen","ASD": "Ashland","ATB": "Ashtabula","ATH": "Athens","AUG": "Auglaize","BEL": "Belmont","BRO": "Brown","BUT": "Butler","CAR": "Carroll","CHP": "Champaign","CLA": "Clark","CLE": "Clermont","CLI": "Clinton","COL": "Columbiana","COS": "Coshocton","CRA": "Crawford","CUY": "Cuyahoga","DAR": "Darke","DEF": "Defiance","DEL": "Delaware","ERI": "Erie","FAI": "Fairfield","FAY": "Fayette","FRA": "Franklin","FUL": "Fulton","GAL": "Gallia","GEA": "Geauga","GRE": "Greene","GUE": "Guernsey","HAM": "Hamilton","HAN": "Hancock","HAR": "Hardin","HAS": "Harrison","HEN": "Henry","HIG": "Highland","HOC": "Hocking","HOL": "Holmes","HUR": "Huron","JAC": "Jackson","JEF": "Jefferson","KNO": "Knox","LAK": "Lake","LAW": "Lawrence","LIC": "Licking","LOG": "Logan","LOR": "Lorain","LUC": "Lucas","MAD": "Madison","MAH": "Mahoning","MAR": "Marion","MED": "Medina","MEG": "Meigs","MER": "Mercer","MIA": "Miami","MOE": "Monroe","MOT": "Montgomery","MRG": "Morgan","MRW": "Morrow","MUS": "Muskingum","NOB": "Noble","OTT": "Ottawa","PAU": "Paulding","PER": "Perry","PIC": "Pickaway","PIK": "Pike","POR": "Portage","PRE": "Preble","PUT": "Putnam","RIC": "Richland","ROS": "Ross","SAN": "Sandusky","SCI": "Scioto","SEN": "Seneca","SHE": "Shelby","STA": "Stark","SUM": "Summit","TRU": "Trumbull","TUS": "Tuscarawas","UNI": "Union","VAN": "Van Wert","VIN": "Vinton","WAR": "Warren","WAS": "Washington","WAY": "Wayne","WIL": "Williams","WOO": "Wood","WYA": "Wyandot",
}

# Add county names to odot_pcr (COUNTY_CD is a 3-letter code like "HAM")
odot_pcr["COUNTY_NAME"] = odot_pcr["COUNTY_CD"].astype(str).str.strip().map(odot_codes)

odot_pcr.columns.tolist()

# Aggregate ODOT PCR by county (3-letter code) and county name
odot_pcr_county_avg = (
    odot_pcr.groupby(["COUNTY_CD", "COUNTY_NAME"], as_index=False)
    .agg(n_segments=("PCR_NBR", "size"),
         n_pcr_nonmissing=("PCR_NBR", "count"),
         avg_pcr_nbr=("PCR_NBR", "mean"),)
    .sort_values(["COUNTY_CD", "COUNTY_NAME"])
)

print(odot_pcr_county_avg.head())

# Ideally, I would do ODOT PCR and MODEL PCR comparison at NLFID + CTL_BEGIN + CTL_END level, but I don't have CTL_BEGIN/END info for the images yet.
# For now, we'll just do county + year level comparison.

# Next, I want to set use confidence for severity and area_norm for extent. 
# Confidence and area_norm range from 0 to 1
# 1. I want two thresholds c1 and c2 with c1 < c2 for confidence that segment the variable into L (Low), M (Medium), H (High)
# 2. I want two thresholds a1 and a2 with a1 < a2, and with a2 < 0.6, for area_norm that segment the variable into O (Occasional), F (Frequent), E (Extensive)

# I will use grid search to find optimal thresholds that maximize correlation between MODEL PCR and ODOT PCR at county-year level.

# =============================================================================
# Optimization of thresholds via grid search - DOES NOT WORK YET 
# TODO: Implement grid search to find optimal thresholds
# =============================================================================


# -------------------------
# 0) Ohio county FIPS -> name (3-digit strings)
# -------------------------
OHIO_FIPS3_TO_NAME = {
    "001":"Adams","003":"Allen","005":"Ashland","007":"Ashtabula","009":"Athens","011":"Auglaize",
    "013":"Belmont","015":"Brown","017":"Butler","019":"Carroll","021":"Champaign","023":"Clark",
    "025":"Clermont","027":"Clinton","029":"Columbiana","031":"Coshocton","033":"Crawford","035":"Cuyahoga",
    "037":"Darke","039":"Defiance","041":"Delaware","043":"Erie","045":"Fairfield","047":"Fayette",
    "049":"Franklin","051":"Fulton","053":"Gallia","055":"Geauga","057":"Greene","059":"Guernsey",
    "061":"Hamilton","063":"Hancock","065":"Hardin","067":"Harrison","069":"Henry","071":"Highland",
    "073":"Hocking","075":"Holmes","077":"Huron","079":"Jackson","081":"Jefferson","083":"Knox",
    "085":"Lake","087":"Lawrence","089":"Licking","091":"Logan","093":"Lorain","095":"Lucas","097":"Madison",
    "099":"Mahoning","101":"Marion","103":"Medina","105":"Meigs","107":"Mercer","109":"Miami","111":"Monroe",
    "113":"Montgomery","115":"Morgan","117":"Morrow","119":"Muskingum","121":"Noble","123":"Ottawa",
    "125":"Paulding","127":"Perry","129":"Pickaway","131":"Pike","133":"Portage","135":"Preble","137":"Putnam",
    "139":"Richland","141":"Ross","143":"Sandusky","145":"Scioto","147":"Seneca","149":"Shelby","151":"Stark",
    "153":"Summit","155":"Trumbull","157":"Tuscarawas","159":"Union","161":"Van Wert","163":"Vinton","165":"Warren",
    "167":"Washington","169":"Wayne","171":"Williams","173":"Wood","175":"Wyandot",
}
OHIO_NAME_TO_FIPS3 = {v.strip().lower(): k for k, v in OHIO_FIPS3_TO_NAME.items()}

# -------------------------
# 1) Prepare ODOT target series at county level (FIPS3 key)
# -------------------------
odot_target = odot_pcr_county_avg.copy()
odot_target["county_fips3"] = (
    odot_target["COUNTY_NAME"].astype(str).str.strip().str.lower().map(OHIO_NAME_TO_FIPS3)
)
odot_target = odot_target.dropna(subset=["county_fips3"]).copy()
odot_target["county_fips3"] = odot_target["county_fips3"].astype(str).str.zfill(3)

# target: ODOT avg PCR by county (2024 only here)
odot_y = odot_target.set_index("county_fips3")["avg_pcr_nbr"].astype(float)

# -------------------------
# 2) Prepare MODEL arrays (2024 detections) keyed by county_fips3
# -------------------------
df_det = df_long24.copy()

# ensure county is 3-digit string
df_det["county_fips3"] = df_det["county"].astype(str).str.zfill(3)

# OPTIONAL: keep only rows that map to an ODOT distress (drop nuisance classes)
df_det["distress_weight"] = pd.to_numeric(df_det["distress_weight"], errors="coerce").fillna(0.0)

# conf/area numeric
df_det["conf"] = pd.to_numeric(df_det["conf"], errors="coerce").fillna(0.0)
df_det["area_norm"] = pd.to_numeric(df_det["area_norm"], errors="coerce").fillna(0.0)
df_det["class_id"] = pd.to_numeric(df_det["class_id"], errors="coerce").fillna(-1).astype(int)

# precompute n_images_by_county (does NOT depend on thresholds)
n_images_by_county = df_det.groupby("county_fips3")["filename"].nunique().astype(float)
n_images_by_county = n_images_by_county.replace(0, np.nan)

# restrict to counties we can compare (intersection)
common_counties = sorted(set(n_images_by_county.index).intersection(set(odot_y.index)))
if len(common_counties) < 10:
    raise ValueError(f"Too few overlapping counties for correlation: {len(common_counties)}")

df_det = df_det[df_det["county_fips3"].isin(common_counties)].copy()
n_images_by_county = n_images_by_county.loc[common_counties]
odot_y = odot_y.loc[common_counties]

# categorical county index for fast bincount aggregation
county_cat = pd.Categorical(df_det["county_fips3"], categories=common_counties, ordered=True)
county_idx = county_cat.codes.astype(np.int32)  # 0..(n_counties-1)
n_counties = len(common_counties)

# arrays
conf_arr = df_det["conf"].to_numpy(dtype=np.float32)
area_arr = df_det["area_norm"].to_numpy(dtype=np.float32)
base_w   = df_det["distress_weight"].to_numpy(dtype=np.float32)  # already /100 in your code
cls_id   = df_det["class_id"].to_numpy(dtype=np.int16)

# -------------------------
# 3) Build per-class multiplier lookup tables from MODEL_TO_ODOT_MAP
# -------------------------
# Severity categories: L/M/H => indices 0/1/2
# Extent categories:   O/F/E => indices 0/1/2
sev_levels = ["L", "M", "H"]
ext_levels = ["O", "F", "E"]

max_class_id = max(CLASS_NAMES.keys())
sev_mult = np.zeros((max_class_id + 1, 3), dtype=np.float32)
ext_mult = np.zeros((max_class_id + 1, 3), dtype=np.float32)

for cid, cname in CLASS_NAMES.items():
    m = MODEL_TO_ODOT_MAP.get(cname)
    # ignore unmapped/non-distress classes
    if (m is None) or (m.get("odot_distress") is None):
        continue
    for j, s in enumerate(sev_levels):
        sev_mult[cid, j] = float(m["severity_weight"][s])
    for j, e in enumerate(ext_levels):
        ext_mult[cid, j] = float(m["extent_weight"][e])

# clip class ids outside range to 0 so they contribute 0 via base_w or multipliers
cls_id = np.clip(cls_id, 0, max_class_id)

# -------------------------
# 4) Define MODEL PCR function for given thresholds
# -------------------------
def model_pcr_by_county(c1, c2, a1, a2, *, alpha=1.0, clip_to_0_100=True):
    """
    Returns a pd.Series indexed by county_fips3 with MODEL county-average PCR.
    alpha is an optional scaling of deductions (kept fixed in grid search).
    """
    # severity category per detection
    sev_cat = np.where(conf_arr < c1, 0, np.where(conf_arr < c2, 1, 2)).astype(np.int8)
    # extent category per detection
    ext_cat = np.where(area_arr < a1, 0, np.where(area_arr < a2, 1, 2)).astype(np.int8)

    # per-detection deduction contribution
    ded = base_w * sev_mult[cls_id, sev_cat] * ext_mult[cls_id, ext_cat]
    if alpha != 1.0:
        ded = alpha * ded

    # sum deductions by county (fast)
    sum_ded = np.bincount(county_idx, weights=ded, minlength=n_counties).astype(np.float64)

    # mean deduction per image (equivalent to mean over image-level sums)
    mean_ded = sum_ded / n_images_by_county.to_numpy(dtype=np.float64)

    # map to PCR scale
    pcr = 100.0 * (1.0 - mean_ded)

    if clip_to_0_100:
        pcr = np.clip(pcr, 0.0, 100.0)

    return pd.Series(pcr, index=common_counties, name="model_avg_pcr")

def pearson_corr(x, y):
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    if np.all(np.isfinite(x)) and np.all(np.isfinite(y)) and (x.std() > 0) and (y.std() > 0):
        return float(np.corrcoef(x, y)[0, 1])
    return np.nan

def spearman_corr(x, y):
    # simple Spearman via ranks
    xr = pd.Series(x).rank().to_numpy(dtype=float)
    yr = pd.Series(y).rank().to_numpy(dtype=float)
    return pearson_corr(xr, yr)

def rmse(x, y):
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    return float(np.sqrt(np.nanmean((x - y) ** 2)))

# -------------------------
# 5) Build candidate grids (quantile-based, keeps search size sane)
# -------------------------
# You can tighten/loosen these. Smaller grids = much faster.
conf_vals = conf_arr[np.isfinite(conf_arr)]
area_vals = area_arr[np.isfinite(area_arr)]

# candidate cutpoints from quantiles (dedup + sorted)
conf_cands = np.unique(np.round(np.quantile(conf_vals, np.linspace(0.15, 0.85, 8)), 3))
area_cands = np.unique(np.round(np.quantile(area_vals, np.linspace(0.10, 0.80, 8)), 4))
area_cands = area_cands[area_cands < 0.6]  # enforce your a2 < 0.6

# build ordered pairs c1<c2 and a1<a2
conf_pairs = [(c1, c2) for i, c1 in enumerate(conf_cands) for c2 in conf_cands[i+1:]]
area_pairs = [(a1, a2) for i, a1 in enumerate(area_cands) for a2 in area_cands[i+1:]]

print(f"Grid sizes: conf_pairs={len(conf_pairs)} area_pairs={len(area_pairs)} total={len(conf_pairs)*len(area_pairs)}")

# -------------------------
# 6) Grid search: maximize Pearson correlation with ODOT county avg PCR
# -------------------------
results = []
best = {"corr": -np.inf, "params": None, "spearman": None, "rmse": None}

odot_vec = odot_y.to_numpy(dtype=float)

for (c1, c2) in conf_pairs:
    for (a1, a2) in area_pairs:
        # (optional) add simple guardrails
        if not (0.0 <= c1 < c2 <= 1.0):
            continue
        if not (0.0 <= a1 < a2 < 0.6):
            continue

        model_series = model_pcr_by_county(c1, c2, a1, a2, alpha=1.0, clip_to_0_100=True)
        model_vec = model_series.to_numpy(dtype=float)

        corr = pearson_corr(model_vec, odot_vec)
        sp   = spearman_corr(model_vec, odot_vec)
        e    = rmse(model_vec, odot_vec)

        results.append({"c1": c1, "c2": c2, "a1": a1, "a2": a2,
                        "pearson": corr, "spearman": sp, "rmse": e})

        if np.isfinite(corr) and corr > best["corr"]:
            best.update({"corr": corr, "params": (c1, c2, a1, a2), "spearman": sp, "rmse": e})

res_df = pd.DataFrame(results).sort_values(["pearson", "spearman"], ascending=False)

print("\nTop 10 threshold sets:")
print(res_df.head(10).to_string(index=False))

print("\nBEST:")
print(f"  c1,c2,a1,a2 = {best['params']}")
print(f"  pearson     = {best['corr']:.4f}")
print(f"  spearman    = {best['spearman']:.4f}")
print(f"  rmse        = {best['rmse']:.4f}")

# Save grid results (optional)
out_grid = run_dir / "threshold_grid_search_results.csv"
res_df.to_csv(out_grid, index=False)
print("\nSaved grid search results:", out_grid)

# Compute final model county PCR using best params (optional)
if best["params"] is not None:
    c1, c2, a1, a2 = best["params"]
    model_best = model_pcr_by_county(c1, c2, a1, a2)
    compare_df = (
        pd.DataFrame({"odot_avg_pcr": odot_y, "model_avg_pcr": model_best})
        .assign(diff=lambda d: d["model_avg_pcr"] - d["odot_avg_pcr"])
        .reset_index()
        .rename(columns={"index":"county_fips3"})
    )
    out_compare = run_dir / "county_pcr_compare_best_thresholds.csv"
    compare_df.to_csv(out_compare, index=False)
    print("Saved best-threshold county comparison:", out_compare)
