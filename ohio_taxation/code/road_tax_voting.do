* Purpose: identify road taxes due renewal only

// global root "/Users/saannidhyarawat/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

global root "C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation"

global code "${root}/code"
global data "${root}/data"

* read in data
use "${data}/cosub_df_cleaned.dta" , clear

* keeping voting variables only
// keep subdivision_name TENDIGIT_FIPS year tax_type purpose2 description millage_percent duration votes_for votes_against 

* keep voting variables and county's covariates
keep subdivision_name TENDIGIT_FIPS year tax_type purpose2 description millage_percent duration votes_for votes_against pop pctblack pctapi pcthisp pctmarried pctseparated pctwhite poverty unemprate pctlesshs pcthsgrad pctbachelors

* identify only road taxes which were due renewal (and voting on renewal instead of additional taxes)
// keep if purpose2 == "road" && description == "R" 

* identify only road taxes
keep if purpose2 == "road" && description == "R" 


* identify obs with duration greater than 0 and less than 100 (reasonable values)
keep if duration > 0 
keep if duration < 100

* create new variable: total votes
gen total_votes = votes_for + votes_against

* create new variable: proportion of votes for 
gen prop_votes_for = votes_for/ total_votes
gen pct_votes_for = (votes_for/ total_votes)*100

* create a dummy variable for if a tax levy was passed or failed (50% cutoff)
recode pct_votes_for 50/100 = 1 0/49.999 = 0, generate(vote_result)

* taking 2-4 year lags of population variable
by vote_result, sort : summarize unemprate
by vote_result, sort : summarize pop

* saving the dataset
save "${root}/data/road_tax_voting.dta", replace


