* Purpose: identify balanced panel data, remove duplicates and prepare data for analysis;

global root "/Users/saannidhyarawat/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

global code "${root}/code"
global data "${root}/data"


* loading data
// use "${data}/unique_fips_unbalanced" , clear
// levelsof TENDIGIT_FIPS
// global mac_fips_list = r(levels)
// display "$mac_fips_list"
// display "$root"

use "${data}/cosub_place_county_votes_property2" , clear

// global mac_fips_list : subinstr global mac_fips_list " " ",", all 
// drop if inlist(TENDIGIT_FIPS, $mac_fips_list)

* removing leads and lags and saving this dataset
drop *_lag*
drop *_lead*

* drop anything before 1990 or after 2015
drop if year < 1990
drop if year > 2015

* dropping values with missing subdivision_name. These seem to be problematic obs. Need to double check this step
// drop if subdivision_name == "" 

* sort by unique identifiers (i = TENDIGIT_FIPS (county), t = year (time), j = purpose2 and votes_for and votes_against (specific tax renewal voting))
sort TENDIGIT_FIPS year purpose2 votes_for votes_against

* generating dup column to identify duplicates as per TENDIGIT_FIPS and year
* dup = 0 : obs has no duplicates
* dup = 1 : obs has duplicates, but this is the first occurrence (i.e. not a duplicate)
* dup = 2 : obs has duplicates, this is the second occurrence etc. (i.e. duplicate, so remove it) and so on
quietly by TENDIGIT_FIPS year purpose2 votes_for votes_against:  gen dup = cond(_N==1,0,_n)

* keep unique values of TENDIGIT_FIPS
drop if dup > 1

* saving only when purpose2 is road
// drop if purpose2 != "road"

* saving the dataset
save "${root}/data/cosub_df_cleaned.dta", replace

// keep if county == "Hamilton"
// keep subdivision_name TENDIGIT_FIPS year unemprate county subdivision_type tax_type id purpose2 millage_percent votes_for votes_against newpct

