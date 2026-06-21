*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Merge CoreLogic housesales (1995–2024 slim) with cosub/place Census panel (1990–2023)
* Loading housesales first to ensure its columns appear first in the merged file.
* Created by: Saani Rawat
* Log:
* 1. 19Feb2026: Reversed merge order (housesales as master, census as using)
*-----------------------------------------------------------------------------------------------------------------------;

clear all
set more off

*----------------------------------------------------------------------------------;
* 0) Globals: project paths
*----------------------------------------------------------------------------------;

global root   "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global data   "${root}/data"
global hous   "${data}/housing/CoreLogic"

*----------------------------------------------------------------------------------;
* 1) Prepare Census panel (Using) and save as tempfile
*----------------------------------------------------------------------------------;

use "${data}/cosub_place_panel_9023.dta", clear

* Standardize merge keys to match housesales data
rename Census_FIPS FIPS_ID

* Format ID as 10-character string
gen str10 FIPS_ID2 = FIPS_ID
drop FIPS_ID
rename FIPS_ID2 FIPS_ID

* Format year as double
destring year, replace
recast double year

* Save prepared census data to a temporary file
tempfile census_prepped
save `census_prepped'

*----------------------------------------------------------------------------------;
* 2) Load Housesales (Master) and Merge
*----------------------------------------------------------------------------------;

use "${hous}/housesales_9524_slim.dta", clear

* Merge m:1 because housesales has multiple records per FIPS_ID-year, while census is a panel (1 record per FIPS_ID-year)
merge m:1 FIPS_ID year using `census_prepped'

*----------------------------------------------------------------------------------;
* 3) Save outputs
*----------------------------------------------------------------------------------;

keep if _merge==3 // keep inner join only
drop _merge       // clean up merge variable before saving

* (A) Merged output i.e. inner join
save "${hous}/housesales_census_9023.dta", replace