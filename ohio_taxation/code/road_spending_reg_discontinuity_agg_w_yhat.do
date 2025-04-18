*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Indirect involvement of covariates in RDD i.e. predict outcome variable using covariates and then use predicted outcome variable as y in the RD equation
* Created by: Saani Rawat
* Log: 
*		1. 12may2023 : Started code
*		2. 14may2023 : Finished for loop for y_hat outcome variable
*-----------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global output "${data}/outputs"
global tables "${output}/tables/predicted_outcome"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"

* assigning global macros for variables
global Y median_sale_amount // median_sale_amount_per_sq_feet
global Y_hat median_sale_amount_hat // median_sale_amount_per_sq_feet_hat
global ln_Y ln_median_sale_amount
global X votes_pct_for

*------------------------------------------------------------------------;
*		USER SPECIFIED
*------------------------------------------------------------------------;
* specifying regression parameters (see "help rdrobust" for more options)
global kernel = "tri"
global p = "1"
global q = "2"
global bwselect = "mserd"
global cutoff = 50
* covariates list (change covariate list when you change Y)
global covs_list_t_minus_2 = "pop unemprate pctlt5 pctwhite pctblack pctamerind pctapi pctotherrace raceherfindahl pcthisp pctnevermarr"
global covs_list_t_minus_1 = "pctwithkids pctlesshs pctsomecoll pct18to64 pctwhite pctblack pctamerind pctapi pcthisp pctnevermarr incherfindahl inctaxrate"
global covs_list_t_plus_1 = "pctnokids pctwithkids pct5to17 raceherfindahl"
global covs_list_t_plus_2 = "pctwithkids pctnokids pctlesshs pct18to64 pctwhite pctblack pctamerind pctapi raceherfindahl incherfindahl"
global covs_list_t_plus_3 = "pctlt5 pctwhite pctblack pctapi pctotherrace raceherfindahl pctnevermarr incherfindahl"
global covs_list_t_plus_4 = "pctwithkids pctsinparhhld pctnokids pctlesshs pctrent pct5to17 pct18to64 pctamerind pctotherrace incherfindahl"
global covs_list_t_plus_5 = "pop pctsinparhhld pctlesshs pctsomecoll unemprate pctrent pct18to64 pctwhite pctblack pctamerind raceherfindahl incherfindahl"
global covs_list_t_plus_6 = "childpov poverty pctsinparhhld unemprate pct18to64 pctmarried pctseparated"
global covs_list_t_plus_7 = "poverty pctsinparhhld unemprate pctrent pct18to64 pctamerind pctotherrace pctseparated" 
global covs_list_t_plus_8 = "pop pctsinparhhld pctlesshs pctrent pct18to64 pctwhite pctblack pctamerind raceherfindahl pctseparated"
global covs_list_t_plus_9 = "pop poverty pctsinparhhld unemprate pctrent pct18to64 pctwithkids"
global covs_list_t_plus_10 = "unemprate pctrent pctlt5 pct18to64 pct65pls pctblack pctamerind pctotherrace pcthisp pctseparated"

// use "${shared}/housing_agg_roads_census_t_plus_4.dta", clear	
// regress $Y $covs_list_t_plus_4
// predict $Y_hat
* RD regression;
// rdrobust $Y_hat $X, c($cutoff) covs($covariates) all kernel($kernel) p($p) q($q) bwselect($bwselect)
// eststo model
// esttab using "${tables}/est_results_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}_`cutoff'.csv", replace p
// esttab using "${tables}/est_results_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}_`cutoff'.csv", replace p


*for loop: runs RD for each year (t-2 to t+10) using Y_hat as the outcome variable

foreach t of numlist -2/-1 1/10 {
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
	display "Y : $Y"
	display "Y = `Y'"	
	display "covariates = `covariates'"
	
	* computing y_hat;
	regress $Y `covariates'
	predict $Y_hat
	
	* RD regression;
	* generating an exportable table
	rdrobust $Y_hat $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect)
	eststo model
	esttab using "${tables}/est_results_${Y_hat}_`yr'_${kernel}_${bwselect}_${p}_${q}.csv", replace p
	
}

