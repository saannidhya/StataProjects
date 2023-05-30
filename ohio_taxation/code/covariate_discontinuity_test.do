*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Regression discontinuity analysis on Placebo variables i.e. covariates
* Created by: Saani Rawat
* Log: 
*		1. 02nov2022: 
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

* generating treatment/control flag, log of SALE_AMOUNT and interaction term: treated x votes_pct_for;
generate treated = 1 if votes_pct_for >= cutoff
replace treated = 0 if votes_pct_for < cutoff
generate t_times_votes_pct_for = treated*votes_pct_for

* Note: covs_list_all taken from road_spending_reg_discontinuity_agg_w_covariates.do;
global covs_list_all = "pop unemprate pctlt5 pctwhite pctblack pctamerind pctapi pctotherrace raceherfindahl pcthisp pctnevermarr pctwithkids pctlesshs pctsomecoll pct18to64 incherfindahl inctaxrate pctnokids pct5to17 pctrent childpov poverty pctmarried pctseparated pct65pls"

foreach v in $covs_list_all  {
	display "`v'"
	rdrobust `v' $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect)
}
