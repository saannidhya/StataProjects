*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Estimator from Biasi, Lafortune and Schonholzer (2025) on additional road tax levy and employment data
*	       Data cleaning done by odjfs_data_setup_for_biasi.R
* Created by: Saani Rawat
* Log: 
*		1. 23Apr25: started the code
*		2. 07may25: added the firm creation and destruction variables
*-----------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global output "${data}/outputs"
global tables "${output}/tables"
global plots "${output}/plots"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"


*-----------------------------------------------------------------------------------------------------------------------;
*-----------------------------------------------------------------------------------------------------------------------;
*								Employment and Wages
*-----------------------------------------------------------------------------------------------------------------------;
*-----------------------------------------------------------------------------------------------------------------------;


* import agg;
import delimited "${data}/employment/roads_emp_stacked.csv", clear
// import delimited "${data}/employment/roads_emp_stacked_all.csv", clear

gen zero = 0
label var zero "0"

gen ln_wage = log(wage)
gen ln_num_employed = log(num_employed)
gen ln_wage_per_emp = log(wage_per_emp)
gen ln_jobs_created = log(jobs_created)
gen ln_jobs_destroyed = log(jobs_destroyed)

egen baseline_emp = max(cond(year == 2006, num_employed, .)), by(tendigit_fips)

format tendigit_fips %10.0f

gen county_code = floor(tendigit_fips/100000)

* levy election history and future
global D_lag = "dl_5 dl_4 dl_3 dl_2 dl_1"
global D_lead = "dl1 dl2 dl3 dl4 dl5 dl6 dl7 dl8 dl9 dl10"
global E_lag = "el_5 el_4 el_3 el_2 el_1"
global E_lead = "el1 el2 el3 el4 el5 el6 el7 el8 el9 el10"
global M_lag = "ml_1 ml_2 ml_3 ml_4 ml_5 ml_6 ml_7 ml_8 ml_9 ml_10"
global M_lead = "ml1 ml2 ml3 ml4 ml5 ml6 ml7 ml8 ml9 ml10"

global model = "$D_lag $D_lead $E_lead  $E_lag  $M_lag $M_lead"
global FE = "tendigit_fips#cohort county_code#cohort#year"
// global FE = "tendigit_fips year"
/* I want TENDIGIT_FIPS x cohort AND county x cohort x year F.Es  */

// reghdfe jobs_created $model [aw = baseline_emp], a($FE_test) cluster(tendigit_fips)
// reghdfe jobs_destroyed $model, a($FE_test) cluster(tendigit_fips)
// reghdfe ln_wage $model, a($FE_test) cluster(tendigit_fips) // wages go down and up year 5 and 6
// reghdfe job_destruction_rate $model, a($FE_test) cluster(tendigit_fips)
// reghdfe ln_wage_per_emp $model, a($FE_test) cluster(tendigit_fips)


* trying Biasi code;
capture drop coef2 up2 down2 coef3 up3 down3 x x2
gen coef2 = .
gen up2 = .
gen down2 = .
gen coef3 = .
gen up3 = .
gen down3 = .

gen x = _n-6 // subtracts 6 from _n (where _n is the obs counter)
replace x = . if x > 10
eststo nume_linear: reghdfe ln_wage $model, a($FE) cl(tendigit_fips) // Notice no controls in Biasi equation

local t = invttail(`e(df_r)', .025) // Find the critical value for a t-distribution with e(df_r) degrees of freedom at the 2.5% tail, and save it into local macro t

/* global t = invttail(`e(df_r)', .025) // Find the critical value for a t-distribution with e(df_r) degrees of freedom at the 2.5% tail, and save it into local macro t */


// This code takes coefs from dl1 to dl10 (leads) and "additively" adds the coef, and coinfidence interval [down3,up3] to the data table so that they can be used later 
replace coef3 = _b[dl1] if x == 1
replace up3 = _b[dl1] + ((`t')*_se[dl1]) if x == 1
replace down3 = _b[dl1] - ((`t')*_se[dl1]) if x == 1
global COEF = "dl1"
forvalues n = 2/10 {
	global COEF = "$COEF + dl`n'"
	lincom ($COEF)
	replace coef3 = r(estimate) if x == `n'
	replace up3 = r(estimate) + ((`t')*r(se)) if x == `n'
	replace down3 = r(estimate) - ((`t')*r(se)) if x == `n'

}

// This code takes coefs from D_5 to D_1 (lags) and stores the coef, and confidence interval to the data table so that they can be used later
local k = 1
forvalues n = 5(-1)1 {
	replace coef2 = _b[dl_`n'] if x == -`n'
	replace up2 = _b[dl_`n'] + (($t)*_se[dl_`n'])	if x == -`n'
	replace down2 = _b[dl_`n'] - (($t)*_se[dl_`n'])	if x == -`n'
	replace x = -`n' if x == -`n'
	local k = `k' + 1
}
replace coef2 = 0 if x == 0
replace up2 = 0 if x == 0
replace down2 = 0 if x == 0
replace coef3 = 0 if x == 0
replace up3 = 0 if x == 0
replace down3 = 0 if x == 0

local k = `k' + 1

// This code takes coefs from D1 to D10 (leads) and stores the coef, and confidence interval to the data table so that they can be used later
forvalues n = 1(1)10 {
	replace coef2 = _b[dl`n']	if x == `n'
	replace up2 = _b[dl`n'] + (($t)*_se[dl`n'])	if x == `n'
	replace down2 = _b[dl`n'] - (($t)*_se[dl`n']) if x == `n'
	local k = `k' + 1
}
gen x2 = x - 0.2
sort x

twoway 	(connected coef2 x, mcolor("$ylb") lcolor("$ylb") lw(thick) lp(solid))  (rcap up2 down2 x, lcolor("$ylb % 30")) ///
		(line coef3 x, lcolor("$yo") lw(thick) lp(solid) yaxis(2)) (rarea up3 down3 x, color("$yo % 20") yaxis(2)) ///
		, xtitle("Time since levy passage") ytitle("Effect on Num Employed") ytitle("Cumulative effect on Num employed", axis(2)) ///
		legend(ring(0) pos(11) order(1 "Effect per year" 3 "Cumulative effect")) xlabel(-5(1)10) xline(0.3, lcolor(red)) ylabel(-0.8(0.1)0.8, axis(1)) ylabel(-0.8(0.1)0.8, axis(2)) xsize(7) yline(0, lcolor(gs10) lw(vthin)) yscale(off axis(2))

		

* Specifications that have worked so far:
* FULL SAMPLE (A + R) - 
* 1. outcome: ln_jobs_created, FE = "tendigit_fips cohort year"


* A only - no luck yet


*-----------------------------------------------------------------------------------------------------------------------;
*-----------------------------------------------------------------------------------------------------------------------;
*								Firms Created and Destroyed
*-----------------------------------------------------------------------------------------------------------------------;
*-----------------------------------------------------------------------------------------------------------------------;

// import delimited "${data}/employment/roads_firm_cr_stacked_all.csv", clear
import delimited "${data}/employment/roads_firm_dr_stacked_all.csv", clear

gen zero = 0
label var zero "0"

// gen ln_firms_created = log(firms_created)
gen ln_firms_destroyed = log(firms_destroyed)

// egen baseline_emp = max(cond(year == 2006, num_employed, .)), by(tendigit_fips)

* levy election history and future
global D_lag = "dl_5 dl_4 dl_3 dl_2 dl_1"
global D_lead = "dl1 dl2 dl3 dl4 dl5 dl6 dl7 dl8 dl9 dl10"
global E_lag = "el_5 el_4 el_3 el_2 el_1"
global E_lead = "el1 el2 el3 el4 el5 el6 el7 el8 el9 el10"
global M_lag = "ml_1 ml_2 ml_3 ml_4 ml_5 ml_6 ml_7 ml_8 ml_9 ml_10"
global M_lead = "ml1 ml2 ml3 ml4 ml5 ml6 ml7 ml8 ml9 ml10"

global model = "$D_lag $D_lead $E_lead  $E_lag  $M_lag $M_lead"
global FE = "tendigit_fips cohort year"


* trying Biasi code;
capture drop coef2 up2 down2 coef3 up3 down3 x x2
gen coef2 = .
gen up2 = .
gen down2 = .
gen coef3 = .
gen up3 = .
gen down3 = .

gen x = _n-6
replace x = . if x > 10
// eststo nume_linear: reghdfe ln_firms_created $model, a($FE) cl(tendigit_fips) 
eststo nume_linear: reghdfe ln_firms_destroyed $model, a($FE) cl(tendigit_fips) 

local t = invttail(`e(df_r)', .025) // Find the critical value for a t-distribution with e(df_r) degrees of freedom at the 2.5% tail, and save it into local macro t

// This code takes coefs from dl1 to dl10 (leads) and "additively" adds the coef, and coinfidence interval [down3,up3] to the data table so that they can be used later 
replace coef3 = _b[dl1] if x == 1
replace up3 = _b[dl1] + ((`t')*_se[dl1]) if x == 1
replace down3 = _b[dl1] - ((`t')*_se[dl1]) if x == 1
global COEF = "dl1"
forvalues n = 2/10 {
	global COEF = "$COEF + dl`n'"
	lincom ($COEF)
	replace coef3 = r(estimate) if x == `n'
	replace up3 = r(estimate) + ((`t')*r(se)) if x == `n'
	replace down3 = r(estimate) - ((`t')*r(se)) if x == `n'

}

// This code takes coefs from D_5 to D_1 (lags) and stores the coef, and confidence interval to the data table so that they can be used later
local k = 1
forvalues n = 5(-1)1 {
	replace coef2 = _b[dl_`n'] if x == -`n'
	replace up2 = _b[dl_`n'] + ((`t')*_se[dl_`n'])	if x == -`n'
	replace down2 = _b[dl_`n'] - ((`t')*_se[dl_`n'])	if x == -`n'
	replace x = -`n' if x == -`n'
	local k = `k' + 1
}
	replace coef2 = 0 if x == 0
	replace up2 = 0 if x == 0
	replace down2 = 0 if x == 0
	replace coef3 = 0 if x == 0
	replace up3 = 0 if x == 0
	replace down3 = 0 if x == 0
	
	local k = `k' + 1

// This code takes coefs from D1 to D10 (leads) and stores the coef, and confidence interval to the data table so that they can be used later
forvalues n = 1(1)10 {
	replace coef2 = _b[dl`n']	if x == `n'
	replace up2 = _b[dl`n'] + ((`t')*_se[dl`n'])	if x == `n'
	replace down2 = _b[dl`n'] - ((`t')*_se[dl`n']) if x == `n'
	local k = `k' + 1
}
gen x2 = x - 0.2
sort x

twoway 	(connected coef2 x, mcolor("$ylb") lcolor("$ylb") lw(thick) lp(solid))  (rcap up2 down2 x, lcolor("$ylb % 30")) ///
		(line coef3 x, lcolor("$yo") lw(thick) lp(solid) yaxis(2)) (rarea up3 down3 x, color("$yo % 20") yaxis(2)) ///
		, xtitle("Time since levy passage") ytitle("Effect on Firms Created") ytitle("Cumulative effect on Firms Created", axis(2)) ///
		legend(ring(0) pos(11) order(1 "Effect per year" 3 "Cumulative effect")) xlabel(-5(1)10) xline(0.3, lcolor(red)) ylabel(-1(0.1)1, axis(1)) ylabel(-1(0.1)1, axis(2)) xsize(7) // yline(0, lcolor(gs10) lw(vthin)) // yscale(off axis(2))

