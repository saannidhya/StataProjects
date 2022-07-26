


* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"
global housing_df "housing_roads_census_t_plus_10_matches"

*importing tax levies dataset (also contains census info);
use "${shared}/${housing_df}.dta", clear

* keeping only matches and renewal taxes
keep if description == "R"
drop if duration == 1000

* assigning global macros
global Y SALE_AMOUNT
global ln_Y ln_SALE_AMOUNT
global X votes_pct_for
global R votes_pct_for_cntr
scalar cutoff = 50

drop if SALE_AMOUNT 
* dropping all missing SALE_AMOUNT (could also impute them)

* taking log of SALE_AMOUNT
generate ln_SALE_AMOUNT = log(SALE_AMOUNT)
* generating treatment/control flag 
generate treated = 1 if votes_pct_for >= cutoff
replace treated = 0 if votes_pct_for < cutoff

preserve
drop if votes_pct_for < 40


* summary statistics (before and after winsorizing - full sample)
winsor $Y, gen(winsor_SALE_AMOUNT) p(0.01)
summarize winsor_SALE_AMOUNT $Y ln_SALE_AMOUNT $X
bysort treated: summarize winsor_SALE_AMOUNT $Y ln_SALE_AMOUNT $X
summarize millage_percent, detail

* density/manipulation test of X (FAILED)
twoway (histogram $X if $X < 50, freq width(2) bcolor(red)) ///
       (histogram $X if $X >= 50, freq width(2) bcolor(blue) xline(50)), ///
	   name("Histogram_of_X") leg(off)

rddensity $X, c(50)
rddensity $R
	   
*RD plots
rdplot $Y $X, c(50) binselect(esmv) graph_options(name("Default"))
rdplot $Y $X, c(50) p(1) graph_options(name("RDPLOT_p__")) nbins(13 13) h(10 10)
rdplot $Y $X, c(50) p(2) graph_options(name("RDPLOT_p2"))

rdplot winsor_SALE_AMOUNT $X, c(50) binselect(esmv) graph_options(name("Default_w_bins")) h(13 13)

rdplot winsor_SALE_AMOUNT $X, c(50) p(1) graph_options(name("RDPLOT_p1_w"))
rdplot winsor_SALE_AMOUNT $X, c(50) p(2) graph_options(name("RDPLOT_p2_w"))

// rdplot $ln_Y $X, c(50) binselect(esmv) graph_options(name("ln_Default"))
// rdplot $ln_Y $X, c(50) p(1) graph_options(name("RDPLOT_p1"))


* full regression
rdrobust $Y $R
rdrobust winsor_SALE_AMOUNT $R
rdrobust $ln_Y $R
rdrobust $Y $R, h(10) kernel(uni) vce(hc0)
rdrobust $ln_Y $R, h(10) kernel(uni) vce(hc0)
rdrobust winsor_SALE_AMOUNT $R, h(10) kernel(uni) vce(hc0)

* bandwidth selection
rdbwselect $Y $X, c(50) kernel(uniform)
rdbwselect $Y $X, c(50) kernel(triangular)
rdbwselect $Y $X, c(50) kernel(epanechnikov)
rdbwselect $Y $X, c(50) kernel(uni) all


* restricted regression (within a bandwidth)
rdrobust $Y $X, c(50) bwselect(msetwo)
rdrobust $Y $X, c(50) bwselect(mserd) all
rdrobust winsor_SALE_AMOUNT $X, c(50) bwselect(msetwo)
rdrobust winsor_SALE_AMOUNT $X, c(50) bwselect(mserd)

** PLOTS -- LOCAL
rdrobust $Y $X, c(50) h(9) kernel(uni) vce(hc0)

rdrobust $Y $R, h(9) kernel(uni) vce(hc0)
rdplot $Y $R if -e(h_l) <= $R & $R <= e(h_r), ///
       binselect(esmv) kernel(uniform) h(`e(h_l)' `e(h_r)') p(1) nbins(1000) ///
       graph_options(title("RD Plot") ///
                     ytitle(Sale Amount) ///
                     xtitle(Percent of Votes for Road Tax Levy) ///
                     graphregion(color(white)) ///
					 name("RDPlotLocal1000"))


*manipulation tests;
// hist votes_pct_for_cntr, addplot(pci 0 0 0.2 0)
// rddensity votes_pct_for_cntr
//
// by TENDIGIT_FIPS: rdrobust SALE_AMOUNT votes_pct_for
//
// rdplot SALE_AMOUNT votes_pct_for_cntr
//
// *select bandwidth
// rdbwselect SALE_AMOUNT votes_pct_for_cntr
//
//
// local hlfbw = e(b_mserd)/2
// rdplot SALE_AMOUNT votes_pct_for_cntr, graph_options(xline(`hlfbw') xline(`hlfbw'))


*------------------------------------------------------------;
*		Aggregation
*------------------------------------------------------------;
preserve
bysort 


