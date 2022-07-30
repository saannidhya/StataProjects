*-------------------------------------------------------------------------------------------------;
* Purpose: Merge cleaned housing data with census & road voting data to perform RDD
* Created by: Saani Rawat
* Log: 
* 		1. 05July2022: finished cleaning roads_and_census dataset							
*		2. 06July2022: Added merge based on t+1,...,t+10 variables
*		3. 08July2022: updated the code to include t-1, t-2 variables and merge
*		4. 11July2022: updated code by generating new dataset for _merge==3 only
*		5. 18July2022: added code to generate aggregate housing datasets with median sale amount
*		6. 30July2022: added code to generate SALE_AMOUNT_per_sq_feet variable
*-------------------------------------------------------------------------------------------------;


* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"

* start log;
log using "${root}/code/ohio_road_housing_census_merge.txt", text replace

* defining lists/scalars;
scalar cutoff = 50

*importing tax levies dataset (also contains census info);
use "${data}/roads_levies2_census_9118.dta", clear
sort TENDIGIT_FIPS year

*storing a snapshot (to restore later, if needed);
preserve

* keeping only the required variables (using vars in Dr. Brasington's email: see email on 07/08/2022, 8:13am)
keep year pop TENDIGIT_FIPS TENDIGIT_FIPS_year childpov poverty pctwithkids pctsinparhhld pctnokids pctlesshs pcthsgrad pctsomecoll pctbachelors pctgraddeg unemprate medfamy pctrent pctown pctlt5 pct5to17 pct18to64 pct65pls pctwhite pctblack pctamerind pctapi pctotherrace pctmin raceherfindahl pcthisp pctmarried pctnevermarr pctseparated pctdivorced lforcepartrate incherfindahl inctaxrate tax_type purpose2 description millage_percent duration votes_for votes_against 

*calculating votes_pct_for and votes_pct_for_cntr
generate votes_pct_for = (votes_for / (votes_for + votes_against))*100
generate votes_pct_for_cntr = abs(votes_pct_for - cutoff)

*---------------------------------------------------------------------------------------------;
*	filtering to remove dups																  ;
*---------------------------------------------------------------------------------------------;

*1st type of filter: keep observations closest to cutoff;
sort TENDIGIT_FIPS year votes_pct_for_cntr
quietly by TENDIGIT_FIPS year: gen dup = cond(_N==1, 0, _n)
drop if dup > 1
drop dup

* creating new year variables: t+1, ...., t+10. Merge will be done based on these variables
foreach t of numlist -2/-1 1/10 {
	if `t' < 0 {
		local t_abs = abs(`t')
		generate yr_t_minus_`t_abs' = year + `t'
	}
	else {
		generate yr_t_plus_`t' = year + `t'	
	}
}

order TENDIGIT_FIPS year yr_t_minus_1 yr_t_minus_2 yr_t_plus_1-yr_t_plus_10, first
// keep TENDIGIT_FIPS year yr_t_plus_1-yr_t_plus_10

* saving after filtering
save "${data}/roads_and_census.dta", replace


*---------------------------------------------------------------------------------------------------------;
*	Merging housing and roads (using TENDIGIT_FIPS and year variable : t periods ahead and behind)		  ;
*---------------------------------------------------------------------------------------------------------;

*importing housing sales data;
foreach t of numlist -2/-1 1/10 {
	clear all 
	use "${shared}\housesales_9521_slim.dta", clear
	order TENDIGIT_FIPS year, first
	sort TENDIGIT_FIPS year
	scalar cutoff = 50	
	
	*creating sale_amount per square feet variable;
	generate SALE_AMOUNT_per_sq_feet = SALE_AMOUNT/universal_building_square_feet
	
	if `t' < 0 {
		local t_abs = abs(`t')
		rename year yr_t_minus_`t_abs'

		*destring TENDIGIT_FIPS and year (converting into numeric) before merging;
		destring TENDIGIT_FIPS yr_t_minus_`t_abs', replace
		recast float yr_t_minus_`t_abs'

		* merging with roads and census data;
		merge m:1 TENDIGIT_FIPS yr_t_minus_`t_abs' using "${data}/roads_and_census.dta"
		
		* re-generating centered variable to avoid absolute value sign
		drop votes_pct_for_cntr
		generate votes_pct_for_cntr = votes_pct_for - cutoff		
		
		* saving dataset with matches and non-matches;
		save "${shared}\housing_roads_census_t_minus_`t_abs'.dta", replace 
		keep if _merge == 3
		save "${shared}\housing_roads_census_t_minus_`t_abs'_matches.dta", replace 		
	}
	else {
		rename year yr_t_plus_`t'

		*destring TENDIGIT_FIPS and year (converting into numeric) before merging;
		destring TENDIGIT_FIPS yr_t_plus_`t', replace
		recast float yr_t_plus_`t'
		format %10.0f TENDIGIT_FIPS

		* merging with roads and census data;
		merge m:1 TENDIGIT_FIPS yr_t_plus_`t' using "${data}/roads_and_census.dta"
		
		* re-generating centered variable to avoid absolute value sign
		drop votes_pct_for_cntr
		generate votes_pct_for_cntr = votes_pct_for - cutoff
		
		* saving dataset with matches and non-matches;
		save "${shared}\housing_roads_census_t_plus_`t'.dta", replace 	
		keep if _merge == 3
		save "${shared}\housing_roads_census_t_plus_`t'_matches.dta", replace 		
	}	
	
}


*---------------------------------------------------------------------------------------------------------;
*	Aggregating dataset using median sale amount
*---------------------------------------------------------------------------------------------------------;
foreach t of numlist -2/-1 1/10 {
  clear all 
  use "${shared}\housesales_9521_slim.dta", clear
  scalar cutoff = 50  

  *removing missing SALE_AMOUNT;
  drop if SALE_AMOUNT == .

  *aggregating and using median SALE_AMOUNT;
  bysort TENDIGIT_FIPS year: egen median_sale_amount = median(SALE_AMOUNT)
  bysort TENDIGIT_FIPS year: gen dup = cond(_N==1, 0, _n)

  * keeping only up till first obs;
  keep if dup <= 1
  keep TENDIGIT_FIPS year median_sale_amount
  
	*creating sale_amount per square feet variable;
	generate SALE_AMOUNT_per_sq_feet = SALE_AMOUNT/universal_building_square_feet  

  if `t' < 0 {
    local t_abs = abs(`t')

    * renaming and re-formatting before merge;
    rename year yr_t_minus_`t_abs'
    destring TENDIGIT_FIPS yr_t_minus_`t_abs', replace
    recast float yr_t_minus_`t_abs'

    * merging with roads and census data;
    merge 1:1 TENDIGIT_FIPS yr_t_minus_`t_abs' using "${data}/roads_and_census.dta"
    
	* keeping only renewals, _merge == 3 and duration != 100;
	keep if _merge == 3
	keep if description == "R"
	drop if duration == 1000
	format %12.0g TENDIGIT_FIPS

    * re-generating centered variable to avoid absolute value sign
    drop votes_pct_for_cntr
    generate votes_pct_for_cntr = votes_pct_for - cutoff    
    
    * saving dataset;
    save "${shared}\housing_agg_roads_census_t_minus_`t_abs'.dta", replace 
  }
  else {

    * renaming and re-formatting before merge;
    rename year yr_t_plus_`t'
    destring TENDIGIT_FIPS yr_t_plus_`t', replace
    recast float yr_t_plus_`t'

    * merging with roads and census data;
    merge 1:1 TENDIGIT_FIPS yr_t_plus_`t' using "${data}/roads_and_census.dta"

	* keeping only renewals, _merge == 3 and duration != 100;
	keep if _merge == 3
	keep if description == "R"
	drop if duration == 1000
	format %12.0g TENDIGIT_FIPS
    
    * re-generating centered variable to avoid absolute value sign
    drop votes_pct_for_cntr
    generate votes_pct_for_cntr = votes_pct_for - cutoff
    
    * saving dataset;
    save "${shared}\housing_agg_roads_census_t_plus_`t'.dta", replace   
  } 
  
}


*end log;
log close 
