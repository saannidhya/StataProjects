*------------------------------------------------------------------------------------------------------------------;
* Purpose: Run Regression discontinuity analysis on Outcome of interest (Sale amount) and running variable
* Created by: Saani Rawat
* Log: 
*		1. 18July2022: added code to run a regression. need to update to include all datasets
*		2. 26July2022: created loop to generate t-2, ... t+10 variables, created RD plots and pei 2021 test
*------------------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"
global shared "\\cobshares.uccob.uc.edu\economics$\Julia\roads"
scalar com = length("housing_agg_roads_census_")


global housing_df "housing_agg_roads_census_t_plus_6"
global yr = substr("$housing_df", com+1, length("$housing_df"))
// global var = "housing_" + "$yr"

* importing median_sale_amount dataset ;
use "${shared}/${housing_df}.dta", clear

* assigning global macros
global Y median_sale_amount
global ln_Y ln_median_sale_amount
global X votes_pct_for
global R votes_pct_for_cntr
scalar cutoff = 50

* generating treatment/control flag 
generate treated = 1 if votes_pct_for >= cutoff
replace treated = 0 if votes_pct_for < cutoff
* taking log of SALE_AMOUNT
generate ln_median_sale_amount = log(median_sale_amount)
* creating interaction term: treated x votes_pct_for;
generate t_times_votes_pct_for = treated*votes_pct_for

* density/manipulation test;
rddensity $X, c(50)
twoway (histogram $X if $X < cutoff, freq width(2) bcolor(red)) ///
       (histogram $X if $X >= cutoff, freq width(2) bcolor(blue) xline(50)), ///
	   name("Hist") leg(off)

* Polynomial order test	   
rdmse median_sale_amount votes_pct_for, c(50) h(5) b(5)
	   
*RD plots
rdplot $Y $X, c(50) binselect(esmv) graph_options(name("Defaultt"))	   
rdplot $ln_Y $X, c(50) binselect(esmv) graph_options(name("Defaulttt"))	 

binscatter median_sale_amount votes_pct_for, rd(50) linetype(lfit) ///
xtitle("Percent of Votes for Tax Levy") ytitle("Median House Sale Amount")


	   
* restricted regression (within a bandwidth)
rdrobust $Y $X, c(50)
rdrobust $Y $X, c(50) h(10.642)

rdbwselect $Y $X, c(50) all

* rdrobust and reg commands giving the same output/treatment effect
rdrobust $Y $X, h(5) kernel(uni) c(50) vce(hc0) all
reg $Y treated $X t_times_votes_pct_for if abs($X) <= 55 & abs($X) >= 45, robust

* using regular regression
reg $Y treated##c.$R i
