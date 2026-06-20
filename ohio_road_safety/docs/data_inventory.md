# Data inventory

## Core sources

| Dataset | Path | Unit | Time | Geography |
| --- | --- | --- | --- | --- |
| Geocoded ODJFS masterfile | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_employment/data/masterfile_2006q1_2025q3_geocoded.csv` | establishment-quarter | 2006Q1-2025Q3 | `FIPS_ID` |
| Legacy employment panel | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/employment/emp_df_agg_fips_yr.dta` | local government-year | 2006-2020 | `tendigit_fips` |
| Legacy firm dynamics | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/employment/df_firm_vars_fips_yr.sas7bdat` | local government-year | 2006-2018 | `tendigit_fips` |
| CoreLogic cleaned house sales | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/housing/CoreLogic/housesales_9524_slim.dta` | sale | 1995-2024 | `FIPS_ID` |
| FARS Ohio crash points | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/fars/oh_accident_2001-2021.csv` | fatal crash | 2001-2021 | lat-long |
| FARS ArcGIS join | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/fars/fars_arcgis/oh_accident_2001_ExportTable.csv` | fatal crash | 2001-2021 | `GEOID` |
| Roads and census | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads_and_census.dta` | election-year | existing panel window | `TENDIGIT_FIPS` |
| Cosub/place panel | `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/cosub_place_panel_9023.dta` | local government-year | 1990-2023 | `FIPS_ID` |

## Variables already verified from the existing pipelines

### Employment

The geocoded ODJFS masterfile includes:

- administrative identifiers: `pad`, `uin`, `rep_unit`, `ein`
- timing: `year`, `quarter`
- outcomes: `m1`, `m2`, `m3`, `wage`
- industry: `naics`
- spatial fields: `lat`, `long`, `cousub_fips`, `place_fips`, `FIPS_ID`

### Housing

The cleaned house-sales file includes:

- `FIPS_ID`
- `SALE_AMOUNT`
- `year`
- `acres`
- `universal_built`
- `year_built`
- `total_rooms`
- `total_baths_calculated`
- `agehouse`
- `ac`
- `basement`
- condition dummies
- `lat`, `lon`

### Crashes

The FARS point file includes:

- `year`
- `fatals`
- `persons`
- `ve_forms`
- `latitude`
- `longitud`

The ArcGIS join adds:

- `GEOID`
- `NAME`
- `NAMELSAD`
- `LSAD`

## Overlap that matters for the paper

### Full joint overlap

- housing: available
- employment: available
- crashes: available
- safe common window: 2006-2021

### Best panel unit

The best common panel is `FIPS_ID x year`.

This keeps:

- the housing side on the same local-government definition already used in the CoreLogic cleaning pipeline
- the employment side on the same logic used in the new geocoding workflow
- the crash side aligned once the spatial join is rebuilt from point data

## Main design issue to fix

The legacy FARS aggregation uses a county-subdivision `GEOID`, but the housing and employment pipelines rely on a harmonized `FIPS_ID` that uses place GEOIDs for cities and villages. The crash panel must therefore be rebuilt from points before final estimation.
