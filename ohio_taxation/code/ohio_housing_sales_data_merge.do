*-------------------------------------------------------------------------------------------;
* Purpose: Merge old (ohiohousesales9516_cleaned.dta) and new (ohiohousesales_1621_cleaned.dta) Ohio Housing data
* Created by: Saani Rawat
* Log: 
* 		1. 09may2022: finished writing script based on matched variable names				;
*		2. 22may2022: corrected for missing values generated by variable type mismatch		;
*-------------------------------------------------------------------------------------------;


* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"

* importing variable mapping table;
// import excel "${root}/docs/oh_housing_sales_variable_mapping.xlsx", sheet("final_mapping_table") firstrow

* keeping only variables which matched in excel (intptlat_1, intptlon_1, intptlon_2, intptlon10, subdivision_tract_number were left out due to data type issues);
// levelsof ho
global var_list fid_1 ac acres adjusted_gross_square_feet agehouse aland10 aland10_1 aland10_12 apn_sequence_number attgarage awater10 awater10_1 awater10_2 basement basement_square_feet block_le_1 block_leve block_level_latitude block_level_longitude building_square_feet classfp10 classfp10_ cnectafp10 cond_exc cond_fair cond_good cond_poor cond_vgood condo count_ countyfp10 cousubfp10 cousubns10 depth_footage effective_year_built fid_2 fid_tl_2_1 fid_tl_2_2 fid_tl_2_3 fid_tl_201 finbase fireplace front_footage funcstat_1 funcstat_2 funcstat10 geoid10 geoid10_1 geoid10_12 ground_floor_square_feet higrade10 id intptlat_2 intptlat10 land_square_footage lograde10 lsad10 lsad10_1 lsad10_12 mailing_direction mailing_house_number mailing_house_number_suffix mailing_mode mailing_quadrant mailing_street_name mtfcc10 mtfcc10_1 mtfcc10_12 municipality_code municipality_name name10 name10_1 name10_12 namelsad_1 namelsad10 nctadvfp10 nectafp10 number_of_units objectid onestory original_apn owner_ownership_rights_code ownership_transfer_percentage partial_interest_indicator pcicbsa10 pcinecta10 placefp10 placens10 pool pool_code privwater publicsewer quarter_section ranch range residential_model_indicator sale_amount sale_code sale_date sdtyp10 section shape_area shape_leng situs_city situs_direction situs_house_number situs_house_number_suffix situs_mode situs_quadrant situs_state situs_street_name situs_zip_code statefp1_1 statefp10 statefp10_ stories_code stories_number subdivision_name subdivision_plat_book subdivision_plat_page sum_housin sum_pop1_1 sum_pop10 tendigit_fips title_company_code title_company_name township universal_building_square_feet unsdlea10 wellwater year year_built yearre yearsa
display "$var_list"

*------------------------;
* using old data;
*------------------------;
use "${data}/ohiohousesales9516_cleaned" , clear

*remove PROPERTY_INDICATOR;
drop PROPERTY_INDICATOR

*change variable names to lowercase;
rename *, lower

* DROPPING 2016;
drop if year == 2016

* keeping only matched variables
keep $var_list

* changing variable data types. 3 missing values were missing originally from tendigit_fips;
destring tendigit_fips, replace float
format %12.0g tendigit_fips

* creating a flag to identify observation coming from old dataset;
gen new_flag = 0

* saving old dataset;
save "${root}/data/outputs/oh_house_sales_old.dta", replace


*------------------------;
* using new data;
*------------------------;
use "${data}/ohiohousesales_1621_cleaned" , clear

*change variable names to lowercase;
rename *, lower

* keeping only matched variables
keep $var_list

* convert the following variables from numeric to string;
tostring countyfp10 cousubfp10 cousubns10 intptlat_2 intptlat10 lsad10 lsad10_1 lsad10_12 placefp10 placens10 situs_house_number situs_zip_code statefp1_1 statefp10 statefp10_ title_company_code unsdlea10 geoid10 geoid10_1 geoid10_12 higrade10 , replace


* creating a flag to identify observation coming from new dataset;
gen new_flag = 1
// keep if _n <= 50


* saving old dataset;
save "${root}/data/outputs/oh_house_sales_new.dta", replace

*---------------------------;
* appending old and new data;
*---------------------------;
clear all
append using "${root}/data/outputs/oh_house_sales_old.dta" "${root}/data/outputs/oh_house_sales_new.dta", force

* saving appended dataset;
save "${root}/data/outputs/oh_house_sales_df.dta", replace

