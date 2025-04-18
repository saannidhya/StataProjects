*-----------------------------------------------------------------------------------------------------------------------;
* Purpose: Covariate smoothness plots
* Created by: Saani Rawat
* Log: 
*		1. 23Aug2022: finished writing plot creation code. Based on one df only
*-----------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global output "${data}/outputs"
global tables "${output}/tables"
global plots "${output}/plots"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"
global X votes_pct_for
global cutoff = 50
global df = "roads_and_census"

* complete covariates list
global covariates_list = "pop childpov poverty pctwithkids pctsinparhhld pctnokids pctlesshs pcthsgrad pctsomecoll pctbachelors pctgraddeg unemprate medfamy pctrent pctown pctlt5 pct5to17 pct18to64 pct65pls pctwhite pctblack pctamerind pctapi pctotherrace pctmin raceherfindahl pcthisp pctmarried pctnevermarr pctseparated pctdivorced lforcepartrate incherfindahl inctaxrate"
global Y median_sale_amount // median_sale_amount_per_sq_feet
global kernel = "tri"
global p = "1"
global q = "2"
global bwselect = "mserd"

* importing median_sale_amount dataset ;
use "${data}/${df}.dta", clear

* clean
keep if description == "R"
drop if duration == 1000

foreach covariate of varlist $covariates_list  {
   display "variable = `covariate'"
   binscatter `covariate' $X, rd($cutoff) linetype(lfit) ///
	xtitle("Percent of Votes for Tax Levy") ytitle("`covariate'") title("Covariate smoothness at cutoff: `covariate'") ///
	savegraph("$plots/covariate_smoothness_plot_`covariate'.png") replace
}

* covariate rdrobust

foreach covariate of varlist $covariates_list  {
   display "variable = `covariate'"
   rdrobust `covariate' $X, c($cutoff) all kernel($kernel) p($p) q($q) bwselect($bwselect)
//    collect export "${tables}/est_results_`covariate'_${kernel}_${bwselect}_${p}_${q}.xlsx", name(Table) as(xlsx) sheet(endogeneity_test) cell(A1) modify
}

* pcthsgrad was less than 0.05;