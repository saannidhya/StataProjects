*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Regression discontinuity analysis on Outcome of interest (Median Sale amount 
* 		   and Median Sale Amount per sq feet), running variable AND covariates
* Created by: Saani Rawat
* Log: 
*		1. 23Aug2022 : added code to run a regression WITH covariates for median_sale_amount. Need to do median_sale_amount_per_sq_feet next 
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
global ln_Y ln_median_sale_amount
global X votes_pct_for
global R votes_pct_for_cntr
global prior_yrs_flg = 1
global cutoff = 50
scalar cutoff = 50


*------------------------------------------------------------------------;
*		USER SPECIFIED
*------------------------------------------------------------------------;
* specifying regression parameters (see "help rdrobust" for more options)
global kernel = "tri"
global p = "1"
global q = "2"
global bwselect = "mserd"
* covariates list
global covs_list_t_minus_2 = "pop unemprate pctlt5 pctwhite pctblack pctamerind pctapi pctotherrace raceherfindahl pcthisp pctnevermarr"
global covs_list_t_minus_1 = "pctwithkids pctlesshs pctsomecoll pct18to64 pctwhite pctblack pctamerind pctapi pcthisp pctnevermarr incherfindahl inctaxrate"
global covs_list_t_plus_1 = "childpov poverty pctwithkids pctsinparhhld pctnokids pcthsgrad pctsomecoll pctrent pct5to17 raceherfindahl pcthisp inctaxrate"
global covs_list_t_plus_2 = "pctwithkids pctnokids pctlesshs pct18to64 pctwhite pctblack pctamerind pctapi raceherfindahl incherfindahl"
global covs_list_t_plus_3 = "pctlt5 pctwhite pctblack pctapi pctotherrace raceherfindahl pctnevermarr incherfindahl inctaxrate"
global covs_list_t_plus_4 = "pctwithkids pctsinparhhld pctnokids pctlesshs pctrent pct5to17 pct18to64 pctamerind pctotherrace incherfindahl"
global covs_list_t_plus_5 = "pop pctsinparhhld pctlesshs pctsomecoll unemprate pctrent pct18to64 pctwhite pctblack pctamerind raceherfindahl incherfindahl"
global covs_list_t_plus_6 = "childpov poverty pctsinparhhld unemprate pct18to64 pctmarried pctseparated"
global covs_list_t_plus_7 = "poverty pctsinparhhld unemprate pctrent pct18to64 pctamerind pctotherrace pctseparated" 
global covs_list_t_plus_8 = "pop pctsinparhhld pctlesshs pctrent pct18to64 pctwhite pctblack pctamerind raceherfindahl pctseparated"
global covs_list_t_plus_9 = "pop childpov poverty pctsinparhhld unemprate medfamy pct65pls pctwhite pctblack raceherfindahl pctseparated inctaxrate"
global covs_list_t_plus_10 = "unemprate pctrent pctlt5 pct18to64 pct65pls pctblack pctamerind pctotherrace pcthisp pctseparated"
// global covs = "childpov poverty pctwithkids pctsinparhhld pctnokids pcthsgrad pctsomecoll pctrent pct5to17 raceherfindahl pcthisp inctaxrate"


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
	display "$Y"
	display "`Y'"	
	display "covariates = `covariates'"

	* RD regression;
	* generating an exportable table
	table () ( result ) (), command(rdrobust $Y $X, c($cutoff) covs(`covariates') all kernel($kernel) p($p) q($q) bwselect($bwselect) )
	collect layout (result[_r_b] result[_r_se] result[_r_z] result[_r_p] result[_r_lb] result[_r_ub] result[N] result[N_l] result[N_r] result[N_h_l] result[N_h_r] result[bwselect] result[c] result[h_l] result[h_r] result[cmd] result[kernel] result[level] result[outcomevar] result[runningvar] result[p] result[q]) (colname[Conventional Bias-corrected Robust] colname[_hide]) (), name(Table)
	collect label levels result level "Significance Level" _r_lb "95% lower" _r_ub "95% upper" N_l "Tot. obs. to left" N_r "Tot. obs. to right" N_h_l "Tot. obs. to left within bw" N_h_r "Tot. obs. to right within bw", name(Table) modify
	collect style cell result[_r_b]#result[_r_se]#result[_r_lb]#result[_r_ub], name(Table) warn nformat(%9.0f)
	collect style cell result[h_l]#result[h_r], name(Table) warn nformat(%9.2f)
	collect export "${tables}/est_results_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}.xlsx", name(Table) as(xlsx) sheet(with_covs) cell(A1) modify	
	putexcel set "${tables}/est_results_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}.xlsx", sheet(with_covs) modify
	putexcel A24 = "covariates: " B24 = " `covariates'" ///
			 A25 = "dataset:" B25 = "`housing_df'"
}