*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Regression discontinuity analysis on Outcome of interest (Sale amount) and running variable
* Created by: Saani Rawat
* Log: 
*		1. 18July2022: added code to run a regression. need to update to include all datasets
*		2. 26July2022: created loop to generate t-2, ... t+10 variables, created RD plots and pei 2021 test
*		3. 27July2022: finished the loop
*		4. 02Aug2022 : generalized the code so that it can handle median_sale_amount and median_sale_amount_per_sq_feet
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

* specifying regression parameters (see "help rdrobust" for more options)
global kernel = "tri"
global p = "1"
global q = "2"
global bwselect = "mserd"

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

	* importing median_sale_amount dataset ;
	use "${shared}/`housing_df'.dta", clear	
	display "`housing_df'"
	display "`yr'"
	display "`year'"
	display "$Y"
	display "`Y'"	
	
	* generating treatment/control flag, log of SALE_AMOUNT and interaction term: treated x votes_pct_for;
	generate treated = 1 if votes_pct_for >= cutoff
	replace treated = 0 if votes_pct_for < cutoff
	generate t_times_votes_pct_for = treated*votes_pct_for
	
	* density/manipulation test;
	rddensity $X, c(50)
	twoway (histogram $X if $X < cutoff, freq width(2) bcolor(red)) ///
		   (histogram $X if $X >= cutoff, freq width(2) bcolor(blue) xline(50)), ///
		   leg(off) xtitle("Percent of Votes for Tax Levy") title("Density plot: (`year')")
	graph export "$plots/density_plot_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}.png", replace
	   
	display "ran till checkpoint 0"   
	* regression run
	rdrobust $Y $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect)
	
	* storing optimal bandwidths
	local h_l = round(`e(h_l)', 0.1)
	local h_r = round(`e(h_r)', 0.1)
// 	local b_l = round(`e(b_l)'
// 	local b_r = round(`e(b_r)'
	
	* Polynomial order test	   
	*rdmse $Y $X, c(50) h(`e(h_l)') b(`e(b_l)')	
	
	*RD plots
	*rdplot $Y $X, c(50) binselect(esmv) 	 
	* full plot
	binscatter $Y $X, rd($cutoff) linetype(lfit) ///
	xtitle("Percent of Votes for Tax Levy") ytitle("`Y' (`year')") title("Regression Discontinuity plot (Full)") ///
	savegraph("$plots/rd_plot_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}_full.png") replace
	* plot within the bandwidth selected by rdrobust
	binscatter $Y $X if votes_pct_for >= cutoff-`h_l' & votes_pct_for <= cutoff+`h_r', rd($cutoff) linetype(lfit) ///
	xtitle("Percent of Votes for Tax Levy") ytitle("`Y' (`year')") title("Regression Discontinuity plot (bw: `h_l', `h_r')") ///
	savegraph("$plots/rd_plot_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}_within.png") replace

	* generating an exportable table
	table () ( result ) (), command(rdrobust $Y $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect) )
	collect layout (result[_r_b] result[_r_se] result[_r_z] result[_r_p] result[_r_lb] result[_r_ub] result[N] result[N_l] result[N_r] result[N_h_l] result[N_h_r] result[bwselect] result[c] result[h_l] result[h_r] result[cmd] result[kernel] result[level] result[outcomevar] result[runningvar] result[p] result[q]) (colname[Conventional Bias-corrected Robust] colname[_hide]) (), name(Table)
	collect label levels result level "Significance Level" _r_lb "95% lower" _r_ub "95% upper" N_l "Tot. obs. to left" N_r "Tot. obs. to right" N_h_l "Tot. obs. to left within bw" N_h_r "Tot. obs. to right within bw", name(Table) modify
	collect style cell result[_r_b]#result[_r_se]#result[_r_lb]#result[_r_ub], name(Table) warn nformat(%9.0f)
	collect style cell result[h_l]#result[h_r], name(Table) warn nformat(%9.2f)
	collect export "${tables}/est_results_${Y}_`yr'_${kernel}_${bwselect}_${p}_${q}.xlsx", name(Table) as(xlsx) sheet(Sheet1) cell(A1) replace	
	
}

