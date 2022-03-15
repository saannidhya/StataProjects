/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Road Tax Project
|Purpose   : to create employment and wages outcome variables for tax project
|History   : Date       By Description
			 12Mar2022  SR Merged employment data with Tax and Census data. 
						Issue: Employment data is per zipcode. 
						Tax and Census data is per fips code. Merged using a lookup file
\*=================================================================================================*/

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;

%let data = &root.\data;
%let extracts = C:\QCEW Data - Ohio\ES202\extracts;
libname input "&root.\data";
libname extr "&extracts.";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*importing data files. Created 5-digits state-county fips and 5-digit county-subdivision fips;
proc import datafile="&data.\ZIP-COUNTY-FIPS_2017-06.csv" dbms=csv replace out=zip_fips (where= (strip(lowcase(state)) = "oh"));
run;
proc sql;
	create table masterfile_2006q1_2021q2 as
		select *, input(zip,5.) as zipcode, sum(m1,m2,m3) as num_employed
			from extr.masterfile_2006q1_2021q2
				where lowcase(strip(state)) = "oh" and year <= 2020
			 
;
quit;
proc sql;
	create table road_tax_and_census_oh as
		select *, input(substr(strip(put(tendigit_fips, 10.)),1, 5), 5.) as stco_fips, input(substr(strip(put(tendigit_fips, 10.)), 6),5.) as cosub_fips
			from input.road_tax_and_census_oh 
				where lowcase(strip(description)) = "r" and year >= 2006
;
quit;

*aggregating by zip code and year;
proc sql;
	create table mstr_agg_zip_yr as 
		select zipcode, year, count(*) as count, sum(num_employed) as num_employed, sum(wage) as wage
			from masterfile_2006q1_2021q2
					group by zipcode, year
;
quit;

*joining ohio fips code to employment data (using zipcodes);
proc sql;
	create table mstr_zip_fip_join as
		select *
			from mstr_agg_zip_yr as a
				inner join zip_fips as b
					on a.zipcode = b.ZIP;
quit;

*aggregating by fips codes and year;
proc sql;
	create table mstr_agg_zip_fip_yr as 
		select stcountyfp, year, count(*) as count, sum(num_employed) as num_employed, sum(wage) as wage
			from mstr_zip_fip_join
					group by stcountyfp, year
;
quit;

*creating lag variables (upto 5 years);
proc sort data= mstr_agg_zip_fip_yr out=mstr_agg_zip_fip_yr2 ; by stcountyfp year; run;
data mstr_agg_zip_fip_yr_lag;
	set mstr_agg_zip_fip_yr2;
		by stcountyfp;
		if first.stcountyfp then counter = 1; else counter +1;
			num_employed_lag1 = lag1(num_employed);
			num_employed_lag2 = lag2(num_employed);
			num_employed_lag3 = lag3(num_employed);
			num_employed_lag4 = lag4(num_employed);
			num_employed_lag5 = lag5(num_employed);
			wage_lag1 = lag1(wage);
			wage_lag2 = lag2(wage);
			wage_lag3 = lag3(wage);
			wage_lag4 = lag4(wage);
			wage_lag5 = lag5(wage);
		if counter <= 1 then num_employed_lag1 = .;
		if counter <= 2 then num_employed_lag2 = .;
		if counter <= 3 then num_employed_lag3 = .;
		if counter <= 4 then num_employed_lag4 = .;
		if counter <= 5 then num_employed_lag5 = .;
		if counter <= 1 then wage_lag1 = .;
		if counter <= 2 then wage_lag2 = .;
		if counter <= 3 then wage_lag3 = .;
		if counter <= 4 then wage_lag4 = .;
		if counter <= 5 then wage_lag5 = .;
		drop counter;
run;

*creating lead variables (upto 5 years);
proc sort data= mstr_agg_zip_fip_yr out=mstr_agg_zip_fip_yr4 ; by stcountyfp descending year; run;
data mstr_agg_zip_fip_yr5;
	set mstr_agg_zip_fip_yr4;
		by stcountyfp;
		if first.stcountyfp then counter = 1; else counter +1;
			num_employed_lead1 = lag1(num_employed);
			num_employed_lead2 = lag2(num_employed);
			num_employed_lead3 = lag3(num_employed);
			num_employed_lead4 = lag4(num_employed);
			num_employed_lead5 = lag5(num_employed);
			wage_lead1 = lag1(wage);
			wage_lead2 = lag2(wage);
			wage_lead3 = lag3(wage);
			wage_lead4 = lag4(wage);
			wage_lead5 = lag5(wage);
		if counter <= 1 then num_employed_lead1 = .;
		if counter <= 2 then num_employed_lead2 = .;
		if counter <= 3 then num_employed_lead3 = .;
		if counter <= 4 then num_employed_lead4 = .;
		if counter <= 5 then num_employed_lead5 = .;
		if counter <= 1 then wage_lead1 = .;
		if counter <= 2 then wage_lead2 = .;
		if counter <= 3 then wage_lead3 = .;
		if counter <= 4 then wage_lead4 = .;
		if counter <= 5 then wage_lead5 = .;
		drop counter;
run;
proc sort data=mstr_agg_zip_fip_yr5  out=mstr_agg_zip_fip_yr_lead ; by stcountyfp year; run;

*joining lag and lead years together;
data df_mstr_agg;
	merge mstr_agg_zip_fip_yr_lead (in=mstr_lead) mstr_agg_zip_fip_yr_lag (in=mstr_lag);
		by stcountyfp year;
run;

* final merge: joining tax levy for roads, U.S census and employment data;
proc sql;
	create table input.tax_census_emplmnt_oh as
		select *
			from road_tax_and_census_oh (rename=(year = year_a)) as a
				left join df_mstr_agg as b
					on a.stco_fips = b.stcountyfp and a.year_a = b.year
;
quit;