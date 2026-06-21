# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a research project by Saani Rawat (University of Cincinnati) studying the causal effect of road infrastructure spending on local economic outcomes in Ohio. The primary analysis uses a **Regression Discontinuity Design (RDD)** exploiting close levy elections (cutoff at 50% vote share). A secondary **Difference-in-Differences (DiD)** analysis is also used.

The codebase spans **two linked repositories** that share data:
- `ohio_employment/` — this repo: ODJFS employment data extraction, cleaning, and geocoding
- `ohio_taxation/` — the main analysis repo: all regression analysis, satellite image ML, DGE modeling

**Active branch**: `roads` (the main working branch; `master` is the stable base)

## Data Pipeline

### ohio_employment (this repo)
The employment data originates from Ohio Department of Job & Family Services (ODJFS) via quarterly ES-202/QCEW microdata files stored at `C:\QCEW Data - Ohio\ES202\`.

**Processing order:**
1. `ohio_employment_data_extracts.sas` — Imports quarterly CSV/Stata files (2006–2024Q4), stacks into `masterfile_2006q1_2024q4`, geocodes addresses using `unique_addresses_spatial_join.csv` from ArcGIS, outputs `odjfs_employment_df.dta`
2. `ohio_data_checks.sas` — Validates the master file (NAICS completeness, duplicate detection, year 2020 merge issue)
3. `ohio_data_quality_check.sas` — Benchmarks against published QCEW reports from ohiolmi.com
4. `ohio_employment_summary_tbls.sas` — Creates summary tables aggregated by NAICS 2-digit code, quarter, and year
5. `odjfs_data_geocoding.R` — Geocodes unique addresses via Census geocoding API (chunked, handles UTF-8 issues); outputs `unique_addresses_census_geocoded.csv`
6. `read_data.R` — Quick exploratory reads of the master Stata file

**Key geocoding note**: A known issue exists with 4 addresses containing "9988 merrill" that cause UTF-8 encoding errors in R geocoding — these are filtered out explicitly in `odjfs_data_geocoding.R`.

**MEEI flag**: `MEEI == 2` records are excluded throughout (per ODJFS guidance from Dr. Michael Jones).

### ohio_taxation (main analysis repo)
Employment data from this repo feeds directly into `ohio_taxation` analysis:

```
ohio_employment/data/odjfs_employment_df.dta
        → ohio_taxation/code/2.4_odjfs_data_setup.sas  (creates monthly panel, job creation/destruction vars)
        → ohio_taxation/data/employment/  (SAS and Stata outputs by event-time: t-3 to t+10)
        → ohio_taxation/code/2.1_employment_data_setup.R  (loads into R for RDD/DiD)
```

## ohio_taxation Code Structure

Files are numbered by pipeline stage:

| Stage | Files | Purpose |
|-------|-------|---------|
| 0.x | `0_utility_functions.R`, `0_biasi_utils.r`, `0_data_checks.R` | Shared functions (RD helpers, covariate search) |
| 1.x | `1.x_*.R/py` | Data acquisition: road geometry, CoreLogic geocoding, streetview/satellite images |
| 2.x | `2.x_*.R/.sas/.do` | Data setup: housing, employment, FARS, roads, DiD panel construction |
| 3.x | `3.x_*.R/.do` | Main analyses: RDD (aggregate, by industry, with covariates, placebo, QTE), DiD |
| 4.x | `4.x_*.py` | ML: YOLO/ConvNeXt v2 satellite image road quality classification |
| 5.x | `5.x_*.jl/.py` | DGE model: Julia/Python Dynamic General Equilibrium simulations |
| 6.x | `6.x_*.R/.do` | Robustness: covariate balance, bandwidth sensitivity, misc tests |

**Key RDD parameter**: `cutoff <- 50` (percent of votes against a levy; treatment = levy passes when opposition < 50%)

## SAS Utilities

All SAS code loads a shared macro library:
```sas
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions, subfolder=1);
```
Key macros used: `%util_dat_aggregate`, `%util_dat_drop_empty_cols`, `%util_aux_nobs`, `%util_aux_nvars`, `%util_fmt_std_graphics`.

## R Package Ecosystem

Key packages used across analysis files:
- **Econometrics**: `rdrobust`, `rddensity`, `rdd` (RDD), `MatchIt` (DiD matching)
- **Data**: `haven`, `data.table`, `tidyverse`, `janitor`, `lubridate`
- **Custom**: `Rbearcat` (UC-specific utilities)
- **Geocoding**: `tidygeocoder`

## ML / Python Environment

Satellite image prediction (`4.5_predict_naip_satellite_images.py`) uses the `geoai-mf` conda environment:
```bash
conda activate geoai-mf
python code/4.5_predict_naip_satellite_images.py
```
Models: fine-tuned ConvNeXt v2 (`data/roads/hf_finetuned_convnextv2/`) and YOLOv11 (`data/roads/runs_ohio/.../weights/best.pt`). Output classes: `low_quality` (0), `medium_quality` (1), `high_quality` (2).

## Key File Paths

```
C:\QCEW Data - Ohio\ES202\                     # Raw ODJFS quarterly microdata (external drive)
C:\QCEW Data - Ohio\ES202\extracts\            # Processed extracts (SAS output)
ohio_employment\data\odjfs_employment_df.dta   # Master geocoded employment dataset
ohio_taxation\data\roads_and_census.dta        # Core RDD analysis dataset
ohio_taxation\data\employment\                 # Event-time employment outcomes (t-3 to t+10)
ohio_taxation\data\housing\                    # CoreLogic property data
//cobshares.uccob.uc.edu/economics$/Julia/roads/ # Shared network drive (large files)
```

## ArcGIS Workflow

The `arcgis_data_replication/` folder contains the ArcGIS Pro project (`MyProject.aprx`) used for:
1. Geocoding `unique_addresses.sas7bdat` → spatial join with `tl_2020_39_cousub.shp` (Ohio county subdivisions)
2. Producing `unique_addresses_spatial_join.csv` with `TENDIGIT_FIPS` codes
3. This FIPS assignment is then merged back in `ohio_employment_data_extracts.sas`

## Data Update Workflow (adding new ODJFS quarters)

1. Place new quarterly files in `C:\QCEW Data - Ohio\ES202\<year>\`
2. Add `%import_df(...)` calls for new quarters in `ohio_employment_data_extracts.sas`
3. Run `ohio_employment_data_extracts.sas` to rebuild `odjfs_employment_df.dta`
4. Re-run `ohio_taxation/code/2.4_odjfs_data_setup.sas` to update event-time employment files
5. Re-run downstream analysis scripts (3.x, 6.x) as needed
