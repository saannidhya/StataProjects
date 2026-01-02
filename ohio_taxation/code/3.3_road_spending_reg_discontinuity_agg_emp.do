*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: 
* Created by: 
* Log: 
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
global Y ln_avg_persons // tot_wages, avg_persons, ln_wages , ln_avg_persons
// global ln_Y ln_$Y
// global y_var $ln_Y
global X votes_pct_for
global R votes_pct_for_cntr
global prior_yrs_flg = 1
global cutoff = 50
scalar cutoff = 50
global h_l 10
global h_r 10

// if ($h_l == 0) & ($h_r == 0) {
//     display "It works!"
// }
// else {
//     display "It does not work.."
// }


*------------------------------------------;
*	RD Plots
*------------------------------------------;

foreach t of numlist -2/-1 1/10 {
	local t_abs = abs(`t')		
	if `t' < 0 {
		local t_type = "t_minus"		
	}
	else {
		local t_type = "t_plus"		
	}
	
	local e_prefix = "dfs_emp_agg_yr"
	local t_type_ = subinstr("`t_type'","_", " ",.)
	
	* storing employment dataset name in a local macro
	local employment_df = "`e_prefix'_`t_type'_" + "`t_abs'"
	
	* importing dataset
	use "${data}/employment/`employment_df'.dta", clear	
	
	display "Dataset name: `employment_df'.dta"
	display "Outcome variable: ${Y}"

	* full plot or bandwidth plot
	if ($h_l == 0) & ($h_r == 0) {
		display "displaying full plot"
		
		binscatter $Y $X , rd($cutoff) linetype(lfit) ///
		xtitle("Percent of Votes for Tax Levy") ytitle("$Y (`t_type_' `t_abs')") title("Regression Discontinuity plot (full)") ///
		savegraph("$plots/employment/rd_plot_${Y}_full_`t_type'_`t_abs'.png") replace
		
		
		
	}
	else {
		display "plot within bandwidth: $h_l, $h_r"
		binscatter $Y $X if votes_pct_for >= cutoff- $h_l & votes_pct_for <= cutoff+ $h_r, rd($cutoff) linetype(lfit) ///
		xtitle("Percent of Votes for Tax Levy") ytitle("${Y} (`t_type_' `t_abs')") title("Regression Discontinuity plot (within b.w + ${h_l} and - ${h_r})") ///
		savegraph("$plots/employment/rd_plot_${Y}_within_${h_r}_${h_l}_`t_type'_`t_abs'.png") replace
		
	}
		


}



