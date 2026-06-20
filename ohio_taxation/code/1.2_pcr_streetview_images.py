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
import warnings
from pathlib import Path
from PIL import Image
from sklearn.exceptions import ConvergenceWarning
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import LeaveOneOut, cross_val_predict
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

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
root = Path("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/")

label_dir_candidates = [
    root / "data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/predict_ohio_conf10_annotated_no_negs/labels",
    root / "data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/predict_ohio_conf10_annotated/labels",
    root / "data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/predict_ohio_annotated_v2/labels",
    root / "data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/predict_ohio_annotated/labels",
    root / "data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/predict_ohio/labels",
    root / "data/roads/runs_ohio/yolo11n_cpu_ohio_pred_streetview/labels",
]

_existing_label_dirs = [p for p in label_dir_candidates if p.exists()]
if _existing_label_dirs:
    labels_dir = max(_existing_label_dirs, key=lambda p: len(list(p.glob("*.txt"))))
else:
    labels_dir = label_dir_candidates[0]
labels_path = str(labels_dir) + "/"

# ORIGINAL images that were fed to the YOLO model
images_dir = root / "data/roads/ohio/google streetview photos"
images_path = str(images_dir) + "/"
pcr_path = str(root / "data/roads/PCR") + "/"

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
# IMAGE-LEVEL dataframe: one row per label/image, including clean images
# =============================================================================

image_rows = []
for img in parsed_data:
    row = {
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
        "n_detections": img.get("n_detections"),
    }
    for cname in CLASS_NAMES.values():
        row[f"count_{cname}"] = int(img.get(f"count_{cname}", 0))
    image_rows.append(row)

df_images = pd.DataFrame(image_rows)


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

# Build the image-level panel used for county denominators, including clean images.
df_images["year"] = (
    df_images["date"].astype(str).str.slice(0, 4).pipe(pd.to_numeric, errors="coerce").astype("Int64")
)
_fips10_img = (
    df_images["tendigit_fips"].astype(str).str.replace(r"\D+", "", regex=True).str.zfill(10)
)
df_images["county"] = _fips10_img.str.slice(2, 5)
df_images24 = df_images.loc[df_images["year"].eq(2024)].copy()


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
df_img = df_images24.copy()

# ensure county is 3-digit string
df_det["county_fips3"] = df_det["county"].astype(str).str.zfill(3)
df_img["county_fips3"] = df_img["county"].astype(str).str.zfill(3)

# OPTIONAL: keep only rows that map to an ODOT distress (drop nuisance classes)
df_det["distress_weight"] = pd.to_numeric(df_det["distress_weight"], errors="coerce").fillna(0.0)

# conf/area numeric
df_det["conf"] = pd.to_numeric(df_det["conf"], errors="coerce").fillna(0.0)
df_det["area_norm"] = pd.to_numeric(df_det["area_norm"], errors="coerce").fillna(0.0)
df_det["class_id"] = pd.to_numeric(df_det["class_id"], errors="coerce").fillna(-1).astype(int)

# precompute n_images_by_county using all labeled images, including clean roads
n_images_by_county = df_img.groupby("county_fips3")["filename"].nunique().astype(float)
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

def build_county_feature_matrix(df_img, df_det, counties, class_names):
    """
    Build a county-level feature library from image-level counts and detection-
    level confidence/area summaries. Features are normalized per image so that
    county coverage differences do not mechanically drive the score.
    """
    counties_index = pd.Index(counties, name="county_fips3")
    feature_df = pd.DataFrame(index=counties_index)

    img_group = df_img.groupby("county_fips3", sort=False)
    n_images = img_group["filename"].nunique().reindex(counties_index).astype(float)
    n_images_safe = n_images.replace(0.0, np.nan)

    for cname in class_names.values():
        count_col = f"count_{cname}"
        count_per_img = img_group[count_col].mean().reindex(counties_index).fillna(0.0)
        any_rate = (
            img_group[count_col]
            .apply(lambda s: (pd.to_numeric(s, errors="coerce").fillna(0.0) > 0).mean())
            .reindex(counties_index)
            .fillna(0.0)
        )
        feature_df[f"img_any_rate__{cname}"] = any_rate.astype(float)
        feature_df[f"count_per_img__{cname}"] = count_per_img.astype(float)

    if not df_det.empty:
        det = df_det.copy()
        det["conf_area"] = det["conf"] * det["area_norm"]

        det_agg = (
            det.groupby(["county_fips3", "class_name"], as_index=True)
            .agg(
                conf_sum=("conf", "sum"),
                area_sum=("area_norm", "sum"),
                conf_area_sum=("conf_area", "sum"),
            )
        )

        for metric in ("conf_sum", "area_sum", "conf_area_sum"):
            wide = (
                det_agg[metric]
                .unstack("class_name")
                .reindex(index=counties_index, columns=list(class_names.values()))
                .fillna(0.0)
            )
            wide = wide.div(n_images_safe, axis=0).fillna(0.0)
            for cname in class_names.values():
                feature_df[f"{metric}_per_img__{cname}"] = wide[cname].astype(float)
    else:
        for metric in ("conf_sum", "area_sum", "conf_area_sum"):
            for cname in class_names.values():
                feature_df[f"{metric}_per_img__{cname}"] = 0.0

    # Remove degenerate features with no county-level variation.
    non_constant_cols = [
        col for col in feature_df.columns
        if feature_df[col].replace([np.inf, -np.inf], np.nan).fillna(0.0).nunique() > 1
    ]
    return feature_df.loc[:, non_constant_cols].astype(float)

def fit_learned_county_calibration(X, odot_pcr, counties):
    """
    Learn county-level PCR calibration directly from detector features.

    The target is county-level deduction = 100 - ODOT_PCR. Features are all
    non-negative defect burden summaries. A positive Elastic Net therefore
    learns how much each feature should lower PCR. Hyperparameters are chosen
    to maximize in-sample Pearson correlation, and we also report LOOCV fit.
    """
    counties_index = pd.Index(counties, name="county_fips3")
    X = X.loc[counties_index].astype(float)
    y_pcr = odot_pcr.loc[counties_index].astype(float)
    y_ded = (100.0 - y_pcr).clip(lower=0.0, upper=100.0)

    if X.shape[1] == 0:
        raise ValueError("No non-constant county-level features available for learned calibration.")

    alphas = np.logspace(-4, 1, 80)
    l1_ratios = [0.05, 0.10, 0.20, 0.40, 0.60, 0.80, 1.00]

    search_results = []
    best = None

    for l1_ratio in l1_ratios:
        for alpha in alphas:
            pipe = Pipeline(
                steps=[
                    ("scaler", StandardScaler()),
                    (
                        "model",
                        ElasticNet(
                            alpha=float(alpha),
                            l1_ratio=float(l1_ratio),
                            positive=True,
                            fit_intercept=True,
                            selection="cyclic",
                            max_iter=200000,
                        ),
                    ),
                ]
            )
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", category=ConvergenceWarning)
                pipe.fit(X, y_ded)

            ded_fit = pipe.predict(X)
            pcr_fit = np.clip(100.0 - ded_fit, 0.0, 100.0)

            coef = pipe.named_steps["model"].coef_
            n_nonzero = int(np.sum(np.abs(coef) > 1e-10))
            row = {
                "alpha": float(alpha),
                "l1_ratio": float(l1_ratio),
                "pearson": pearson_corr(pcr_fit, y_pcr.to_numpy(dtype=float)),
                "spearman": spearman_corr(pcr_fit, y_pcr.to_numpy(dtype=float)),
                "rmse": rmse(pcr_fit, y_pcr.to_numpy(dtype=float)),
                "n_nonzero": n_nonzero,
                "model_mean": float(np.mean(pcr_fit)),
                "model_std": float(np.std(pcr_fit, ddof=1)) if len(pcr_fit) > 1 else 0.0,
            }
            search_results.append(row)

            if (
                best is None
                or (row["pearson"] > best["pearson"] + 1e-12)
                or (
                    abs(row["pearson"] - best["pearson"]) <= 1e-12
                    and row["spearman"] > best["spearman"] + 1e-12
                )
                or (
                    abs(row["pearson"] - best["pearson"]) <= 1e-12
                    and abs(row["spearman"] - best["spearman"]) <= 1e-12
                    and row["rmse"] < best["rmse"] - 1e-12
                )
            ):
                best = row | {"pipe": pipe}

    if best is None:
        raise RuntimeError("Learned calibration search did not produce a valid model.")

    best_pipe = best["pipe"]
    scaler = best_pipe.named_steps["scaler"]
    model = best_pipe.named_steps["model"]
    fitted_ded = best_pipe.predict(X)
    fitted_pcr = np.clip(100.0 - fitted_ded, 0.0, 100.0)

    loocv_pipe = Pipeline(
        steps=[
            ("scaler", StandardScaler()),
            (
                "model",
                ElasticNet(
                    alpha=float(best["alpha"]),
                    l1_ratio=float(best["l1_ratio"]),
                    positive=True,
                    fit_intercept=True,
                    selection="cyclic",
                    max_iter=200000,
                ),
            ),
        ]
    )
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=ConvergenceWarning)
        loocv_ded = cross_val_predict(loocv_pipe, X, y_ded, cv=LeaveOneOut())
    loocv_pcr = np.clip(100.0 - loocv_ded, 0.0, 100.0)

    raw_coef = model.coef_ / scaler.scale_
    raw_intercept = float(model.intercept_ - np.sum(model.coef_ * scaler.mean_ / scaler.scale_))

    coef_df = pd.DataFrame(
        {
            "feature": X.columns,
            "coef_standardized": model.coef_,
            "coef_raw": raw_coef,
            "abs_coef_raw": np.abs(raw_coef),
        }
    ).sort_values(["abs_coef_raw", "feature"], ascending=[False, True])

    search_df = pd.DataFrame(search_results).sort_values(
        ["pearson", "spearman", "rmse"],
        ascending=[False, False, True],
    )

    metrics_df = pd.DataFrame(
        [
            {
                "model": "learned_in_sample",
                "alpha": float(best["alpha"]),
                "l1_ratio": float(best["l1_ratio"]),
                "pearson": pearson_corr(fitted_pcr, y_pcr.to_numpy(dtype=float)),
                "spearman": spearman_corr(fitted_pcr, y_pcr.to_numpy(dtype=float)),
                "rmse": rmse(fitted_pcr, y_pcr.to_numpy(dtype=float)),
                "model_mean": float(np.mean(fitted_pcr)),
                "model_std": float(np.std(fitted_pcr, ddof=1)) if len(fitted_pcr) > 1 else 0.0,
                "n_nonzero": int(np.sum(np.abs(model.coef_) > 1e-10)),
                "raw_intercept_deduction": raw_intercept,
            },
            {
                "model": "learned_loocv",
                "alpha": float(best["alpha"]),
                "l1_ratio": float(best["l1_ratio"]),
                "pearson": pearson_corr(loocv_pcr, y_pcr.to_numpy(dtype=float)),
                "spearman": spearman_corr(loocv_pcr, y_pcr.to_numpy(dtype=float)),
                "rmse": rmse(loocv_pcr, y_pcr.to_numpy(dtype=float)),
                "model_mean": float(np.mean(loocv_pcr)),
                "model_std": float(np.std(loocv_pcr, ddof=1)) if len(loocv_pcr) > 1 else 0.0,
                "n_nonzero": int(np.sum(np.abs(model.coef_) > 1e-10)),
                "raw_intercept_deduction": raw_intercept,
            },
        ]
    )

    compare_df = pd.DataFrame(
        {
            "county_fips3": counties_index,
            "odot_avg_pcr": y_pcr.to_numpy(dtype=float),
            "model_avg_pcr_learned": fitted_pcr,
            "model_avg_pcr_learned_loocv": loocv_pcr,
        }
    )
    compare_df["diff_learned"] = compare_df["model_avg_pcr_learned"] - compare_df["odot_avg_pcr"]
    compare_df["diff_learned_loocv"] = compare_df["model_avg_pcr_learned_loocv"] - compare_df["odot_avg_pcr"]

    return {
        "feature_matrix": X,
        "search_df": search_df,
        "metrics_df": metrics_df,
        "coef_df": coef_df,
        "compare_df": compare_df,
    }

# -------------------------
# 5) Build candidate grids (quantile-based, keeps search size sane)
# -------------------------
# Use only mapped distress detections when constructing threshold candidates.
# Nuisance detections contribute zero deduction and should not move severity or
# extent cutoffs.
mapped_mask = np.isfinite(base_w) & (base_w > 0)
conf_vals = conf_arr[mapped_mask & np.isfinite(conf_arr)]
area_vals = area_arr[mapped_mask & np.isfinite(area_arr)]

if len(conf_vals) == 0 or len(area_vals) == 0:
    raise ValueError("No mapped distress detections available for threshold search.")

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

# =============================================================================
# Learned county-level calibration
# =============================================================================

county_feature_df = build_county_feature_matrix(df_img, df_det, common_counties, CLASS_NAMES)
learned = fit_learned_county_calibration(county_feature_df, odot_y, common_counties)

out_features = run_dir / "county_feature_matrix_learned_calibration.csv"
learned["feature_matrix"].reset_index().to_csv(out_features, index=False)

out_search = run_dir / "learned_calibration_search_results.csv"
learned["search_df"].to_csv(out_search, index=False)

out_metrics = run_dir / "learned_calibration_metrics.csv"
learned["metrics_df"].to_csv(out_metrics, index=False)

out_coef = run_dir / "learned_calibration_coefficients.csv"
learned["coef_df"].to_csv(out_coef, index=False)

learned_compare = learned["compare_df"].copy()
if best["params"] is not None:
    learned_compare = learned_compare.merge(
        compare_df[["county_fips3", "model_avg_pcr"]].rename(
            columns={"model_avg_pcr": "model_avg_pcr_threshold"}
        ),
        on="county_fips3",
        how="left",
    )

out_learned_compare = run_dir / "county_pcr_compare_learned_calibration.csv"
learned_compare.to_csv(out_learned_compare, index=False)

print("\nLearned calibration metrics:")
print(learned["metrics_df"].to_string(index=False))
print("\nTop learned-calibration features:")
print(learned["coef_df"].head(10).to_string(index=False))
print("\nSaved learned feature matrix:", out_features)
print("Saved learned calibration search results:", out_search)
print("Saved learned calibration metrics:", out_metrics)
print("Saved learned calibration coefficients:", out_coef)
print("Saved learned calibration county comparison:", out_learned_compare)
