

* reading in data;
use "C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation\data\cosub_df_cleaned.dta", clear

* checking dups by fips and year;
by TENDIGIT_FIPS year: generate dup_fips_yr = cond(_N == 1, 0, _n)

* removing multiple entries;
keep if dup_fips_yr >= 1

* create percent votes variable;
generate pct_votes = votes_for/(votes_for + votes_against) * 100

* create two way scatter plot;
twoway scatter pct_votes year

* histogram;
histogram pct_votes, xline(60)

by TENDIGIT_FIPS: tab result