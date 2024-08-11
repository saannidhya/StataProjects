*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Quantilte-level Regression discontinuity analysis on Outcome of interest (Median Sale amount 
* 		   and Median Sale Amount per sq feet)
* Created by: Saani Rawat
* Log: 
*		1. 26July24 : added code to run rdqte (Quantile RD estimates) 
*		2. 3Aug24	: using rddqte package instead for all regressions. Note: experiencing error while specifying quantiles. Sent authors an email.
*-----------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global output "${data}/outputs"
global tables "${output}/tables"
global plots "${output}/plots"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"

* assigning global macros for variables
global Y median_sale_amount // median_sale_amount_per_sq_feet
// global ln_Y ln_median_sale_amount
// global Y avg_persons // tot_wages
// global ln_Y ln_avg_persons // ln_wages
global X votes_pct_against
global R votes_pct_against_cntr
global prior_yrs_flg = 1
global cutoff = 50
scalar cutoff = 50
global qn = 4
global ql = 0.25
global qh = 1


*------------------------------------------------------------------------------------------;
*	Median Housing Price
*------------------------------------------------------------------------------------------;

// use "${shared}/housing_agg_roads_census_t_plus_1.dta", clear
// gen treated = 1 if votes_pct_against > $cutoff
// replace treated = 0 if votes_pct_against <= $cutoff
// rddqte $Y treated $X , discontinuity($cutoff) bandwidth(5.0)  

foreach t of numlist -3/10 {
	local t_abs = abs(`t')		
	if `t' < 0 {
		local t_type = "t_minus"		
	}
	else {
		local t_type = "t_plus"		
	}
	if "$Y" == "median_sale_amount" {
		local h_prefix = "housing_agg_roads_census"		
	}
	if "$Y" == "median_sale_amount_per_sq_feet" {
		local h_prefix = "housing_agg_roads_census_per"				
	}
	* housing dataset in a local macro
	local housing_df = "`h_prefix'_`t_type'_" + "`t_abs'"
	*scalar to store length of the prefix
	scalar com = length("`h_prefix'")
	
	* extracting years. E.g. t_plus_1, t_plus_2 etc.
	local yr = substr("`housing_df'", com+2, length("`housing_df'")) 
	* replacing "_" with " "
	local year = subinstr("`yr'","_", " ",.)
	local Y = subinstr("${Y}","_", " ",.)
	local covariates = "${covs_list_`yr'}"
	
	* importing median_sale_amount dataset ;
	use "${shared}/`housing_df'.dta", clear	
	display "`housing_df'"
	display "`yr'"
	display "`year'"
	display "$Y"
	display "`Y'"	
	* treated variable
	gen treated = 1 if votes_pct_against > $cutoff
	replace treated = 0 if votes_pct_against <= $cutoff	
	
// 	rdqte $Y $X , c($cutoff) qn($qn) ql($ql) qh($qh)
// 	rddqte $Y treated $X , discontinuity($cutoff) bandwidth(5.0)  
	rddqte $Y treated $X , discontinuity($cutoff) bandwidth(10.0)  	
}
	
// rdqte median_sale_amount votes_pct_against, c($cutoff) qn($qn) ql($ql) qh($qh) 

*------------------------------------------------------------------------------------------;
*	Employment (avg_persons on the payroll)
*------------------------------------------------------------------------------------------;


global Y wages_per_cap
// global Y ln_avg_persons

foreach t of numlist -3/10 {
	local t_abs = abs(`t')		
	if `t' < 0 {
		local t_type = "t_minus"		
	}
	else {
		local t_type = "t_plus"		
	}
	if "$Y" == "avg_persons" | "$Y" == "tot_wages" | "$Y" == "wages_per_cap" {
		local e_prefix = "dfs_emp_agg_yr"
	}
	if "$Y" == "ln_avg_persons" | "$Y" == "ln_tot_wages" {
		local e_prefix = "dfs_emp_ln_agg_yr"
	}
	* housing dataset in a local macro
	local employment_df = "`e_prefix'_`t_type'_" + "`t_abs'"
	*scalar to store length of the prefix
	scalar com = length("`e_prefix'")
	
	* extracting years. E.g. t_plus_1, t_plus_2 etc.
	local yr = substr("`employment_df'", com+2, length("`employment_df'")) 
	* replacing "_" with " "
	local year = subinstr("`yr'","_", " ",.)
	local Y = subinstr("${Y}","_", " ",.)
	local covariates = "${covs_list_`yr'}"
	
	* importing median_sale_amount dataset ;
	use "${data}/employment/`employment_df'.dta", clear	
	display "`employment_df'"
	display "`yr'"
	display "`year'"
	display "$Y"
	display "`Y'"	
	
	* if wages_per_cap is the y variable
	if "$Y" == "wages_per_cap" {
		gen wages_per_cap  = tot_wages/avg_persons
	}
	
// 	rdqte $Y $X , c($cutoff) qn($qn) ql($ql) qh($qh)
// 	rddqte $Y treated $X , discontinuity($cutoff) bandwidth(5.0)  quantiles(0.2 0.4 0.6 0.8)
// 	rddqte $Y treated $X , discontinuity($cutoff) bandwidth(10.0) kernel(triangle) control(poverty)
	rddqte $Y treated $X , discontinuity($cutoff) bandwidth(10.0) kernel(triangle)
}

