/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source	   : 
|Purpose   : To merge houses_census_outcomes_9518.dta and roads_levies2_9118.dta
|History   : Date 		By Description
			 20may22	SR Started joining road tax, housing and census data
\*=================================================================================================*/


%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;

%let data = &root.\data;
%let extracts = C:\QCEW Data - Ohio\ES202\extracts;
libname input "&root.\data";
libname extr "&extracts.";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*----------------------------------------------------------------------------------------
*	Importing housing data	(subset only)
*----------------------------------------------------------------------------------------;
proc import datafile="&root.\data\houses_census_outcomes_subset.dta" dbms=dta replace out=houses_census_subset;
/*proc import datafile="&root.\data\houses_census_outcomes_9518.dta" dbms=dta replace out=houses_census_subset;*/
run;
* importing full df but only first few obs;
/*options obs= 10000;*/
/*proc import datafile="&root.\data\houses_census_outcomes_9518.dta" dbms=dta replace out=houses_census;*/
/*run;*/
/*options obs= max;*/
/*run;*/

*----------------------------------------------------------------------------------------
*	Importing Tax levies data (keeping renewals only)
*----------------------------------------------------------------------------------------;
proc import datafile="&root.\data\roads_levies2_9118.dta" dbms=dta replace out=roads_levies ;*(where=(strip(upcase(description)) = "R"));
run;

*----------------------------------------------------------------------------------------
*	Identifying unique observations by ten-digit fips code and year
*----------------------------------------------------------------------------------------;
* remove all entirely duplicated rows in roads_levies;
proc sort data=roads_levies out= roads_levies noduprecs dupout=dups;
	by _all_;
run;

* sorting by millage_percent (in descending order). We keep the highest millage_percent for each fips per year;
proc sort data=roads_levies out=roads_levies_df;
	by TENDIGIT_FIPS year descending millage_percent;
run;

* keeping only the first observation for each fips, for each year;
proc sort data=roads_levies_df out=roads_levies_sorted nodupkey dupout=roads_levies_dups;
	by TENDIGIT_FIPS year;
run;


*----------------------------------------------------------------------------------------
*	Joining housing dataset with tax levy data
*----------------------------------------------------------------------------------------;
proc sql;
	create table roads_houses_census_left as
		select *
			from houses_census_subset (rename = (TENDIGIT_FIPS=TENDIGIT_FIPS_a year = year_a)) as a
				left join roads_levies_sorted (rename = (TENDIGIT_FIPS=TENDIGIT_FIPS_b year = year_b)) as b
					on a.TENDIGIT_FIPS_a = b.TENDIGIT_FIPS_b and a.year_a=b.year_b;
quit;

* keeping only matched observations;
proc sql;
	create table roads_houses_census_inner as
		select *
			from houses_census_subset (rename = (TENDIGIT_FIPS=TENDIGIT_FIPS_a year = year_a)) as a
				inner join roads_levies_sorted (rename = (TENDIGIT_FIPS=TENDIGIT_FIPS_b year = year_b)) as b
					on a.TENDIGIT_FIPS_a = b.TENDIGIT_FIPS_b and a.year_a=b.year_b;
quit;
*256,523 matches when not removing Additional/new levies.
138,540 after removing additiona/new levies;

*----------------------------------------------------------------------------------------
*	Identify unique fips and year code for join analysis
*----------------------------------------------------------------------------------------;
proc sql;
	create table fips_housing as
		select distinct TENDIGIT_FIPS, count(*) as count
			from houses_census_subset
				group by TENDIGIT_FIPS
;
quit;
proc sql;
	create table fips_census as
		select distinct TENDIGIT_FIPS, count(*) as count
			from roads_levies_sorted
				group by TENDIGIT_FIPS
;
quit;
*by year only;
proc sql;
	create table year_housing as
		select distinct year, count(*) as count
			from houses_census_subset
				group by year
;
quit;
proc sql;
	create table year_census as
		select distinct year, count(*) as count
			from roads_levies_sorted
				group by year
;
quit;
* by fips code and year;
proc sql;
	create table fips_year_housing as
		select distinct TENDIGIT_FIPS, year, count(*) as count
			from houses_census_subset
				group by TENDIGIT_FIPS, year
;
quit;
proc sql;
	create table fips_year_census as
		select distinct TENDIGIT_FIPS, year, count(*) as count
			from roads_levies_sorted
				group by TENDIGIT_FIPS, year
;
quit;