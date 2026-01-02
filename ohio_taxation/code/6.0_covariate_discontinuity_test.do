*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Regression discontinuity analysis on Placebo variables i.e. covariates
* Created by: Saani Rawat
* Log: 
*		1. 26June2024: Added Covariate Smoothness Test plots which will be added to Appendix B
*-----------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global output "${data}/outputs"
global tables "${output}/tables"
global plots "${output}/plots"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"

* importing roads_and_census.dta
use "${data}/roads_and_census.dta", clear	

* assigning global macros for variables
global Y median_sale_amount // median_sale_amount_per_sq_feet
global ln_Y ln_median_sale_amount
global X votes_pct_against
global R votes_pct_against_cntr
global prior_yrs_flg = 1
global cutoff = 50
scalar cutoff = 50

* specifying regression parameters (see "help rdrobust" for more options)
global kernel = "tri"
global p = "1"
global q = "2"
global bwselect = "mserd"

* dropping year cols as they are irrelevant here
drop yr_t_*

* Filter rows where description is "R" and duration is not 1000
keep if description == "R" & duration != "1000"

* Create treated variable based on votes_pct_against and cutoff. Treatment here means the city stopped getting funding.
gen treated = (votes_pct_against >= cutoff)

* interaction term: treated x votes_pct_for;
generate t_times_votes_pct_for = treated*votes_pct_against

* Note: covs_list_all taken from road_spending_reg_discontinuity_agg_w_covariates.do;
// global covs_list_all = "pop unemprate pctlt5 pctwhite pctblack pctamerind pctapi pctotherrace raceherfindahl pcthisp pctnevermarr pctwithkids pctlesshs pctsomecoll pct18to64 incherfindahl inctaxrate pctnokids pct5to17 pctrent childpov poverty pctmarried pctseparated pct65pls"
global covs_list_all = "pop childpov poverty pctwithkids pctsinparhhld pctnokids pctlesshs pcthsgrad pctsomecoll pctbachelors pctgraddeg unemprate medfamy pctrent pctown pctlt5 pct5to17 pct18to64 pct65pls pctwhite pctblack pctamerind pctapi pctotherrace pctmin raceherfindahl pcthisp pctmarried pctnevermarr pctseparated pctdivorced lforcepartrate incherfindahl"

global covs_list_desc = "Population Child_Poverty_Rate Poverty_Rate %_with_Kids %_Households_with_Children_under_18 %_with_no_Kids %_Less_Than_High_School_Education %_Graduated_From_High_School %_with_Some_College_Education %_with_Bachelors_Degree %_with_Graduate_Degree Unemployment_Rate Median_Family_Income %_Renters %_Owners %_Under_5_Years_Old %_Aged_5_to_17 %_Aged_18_to_64 %_Aged_Above_65 %_White %_Black %_American_Indian %_Asian_Pacific_Islander %_of_Other_Race %_Minority Racial_Heterogeneity_Index %_Hispanic %_Married %_Never_Married %_Separated %_Divored Labor_Force_Participation_Rate Income_Heterogeneity_Index"

global covs_len = wordcount("$covs_list_all")
// display "`count'"
foreach i of numlist 1/$covs_len {
    local vari = word("$covs_list_all", `i')
    local desci = word("$covs_list_desc", `i')
	local descii = subinstr("`desci'","_", " ",.)
	display "var is `vari'" 
	display "desc is `descii'" 
	
	rdplot `vari' votes_pct_against, p(1) c($cutoff) ci(95) shade graph_options(title("`descii'") legend(off) xtitle("% Votes Against") ytitle("`descii'"))
	graph export "${plots}/cov_smoothness_`vari'.png", as(png) replace	
}

foreach v in $covs_list_all  {
	display "`v'"
	rdrobust `v' $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect)
}
