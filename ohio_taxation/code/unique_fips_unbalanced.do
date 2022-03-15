* Purpose: data cleaning - identify townships with balanced data (26 years: 1990 to 2015)
* Created by: Saani Rawat
* Last modified: 21 sep 2021
* Summary: 

ssc inst unique


*********
* Specifying libraries
*********

global root "/Users/saannidhyarawat/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

global code "${root}/code"
global data "${root}/data"


* loading data
use "${data}/cosub_place_county_votes_property2" , clear

* dropping values with missing subdivision_name. These seem to be problematic obs
drop if subdivision_name == "" 

* keeping only row identifiers and one col
keep TENDIGIT_FIPS year poverty

* sort by unique identifiers
sort TENDIGIT_FIPS year

* generating dup column to identify duplicates
* dup = 0 : obs has no duplicates
* dup = 1 : obs has duplicates, but this is the first occurrence (i.e. not a duplicate)
* dup = 2 : obs has duplicates, this is the second occurrence etc. (i.e. duplicate, so remove it) and so on
quietly by TENDIGIT_FIPS year:  gen dup = cond(_N==1,0,_n)

* keep unique values of TENDIGIT_FIPS
drop if dup > 1

* checking number of unique FIPS codes
unique TENDIGIT_FIPS
// 2358. Very close to 2356 townships in "ohio-only-all-geocodes-2016.xlsx"

* checking number of unique years
unique year
// 26. So we need 26 years per county for a balanced panel data

*identifying townships which do not have 26 years worth of data
collapse (count) year, by(TENDIGIT_FIPS)
drop if year == 26
// So 43 counties which do not have data from 1990 to 2015 (one or more year missing)
// Can a subdivision_name have two different FIPS? 
// Can a FIPS have two different subdivisions?

* Saving these counties for a later use
save "${root}/data/unique_fips_unbalanced.dta", replace
