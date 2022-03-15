/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Employment Project
|Purpose   : perform data checks and benchmarking on QCEW - Ohio data
|History   : Date       By Description
			 18feb2022  SR Checked validity of masterfile_2006q1_2020q4 file. Found one merging issue (year 2020)
\*=================================================================================================*/

%let in = C:\QCEW Data - Ohio\ES202\extracts;
 
libname input "&in.";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*----------------------------------------------------------------------------------------
*	NAICS code: duplicate check
*----------------------------------------------------------------------------------------;

*checking if naics code is available for observations in each year: from 2006 to 2019;
%macro check_naics(from = , to = );
%do i = &from. %to &to.;
	proc sql;
		create table df_employment_oh_&i. as 
			select * /*, substr(strip(naics),1,2) as NAICS_2dig*/
				from input.masterfile_2006q1_2020q4 (obs = 100)
					where naics_1 is not missing and year = &i.
		;
	quit;
	%put NOTE: Year: &i., Num obs: %util_aux_nobs(df_employment_oh_&i.);
%end;

%mend check_naics;
%check_naics(from = 2006, to = 2020);
*2019 has missing naics code;

*remove columns which have all missing values;
%util_dat_drop_empty_cols(df = input.masterfile_2006q1_2020q4, out_df = masterfile_2006q1_2020q4);

%put NOTE: number of variables before: %util_aux_nvars(input.masterfile_2006q1_2020q4) ;
*73;
%put NOTE: number of variables after: %util_aux_nvars(masterfile_2006q1_2020q4) ;
*66;

*----------------------------------------------------------------------------------------
*	Duplicate variables
*----------------------------------------------------------------------------------------;


* "_1" variables are duplicates. They contain year 2020 information;
proc sql;
	create table df_naics_1 as 
		select * /*, substr(strip(naics),1,2) as NAICS_2dig*/
			from input.masterfile_2006q1_2020q4 
				where naics_1 is not missing
	;
quit; 

*confirmation: only 2020 data was affected by these duplicate variables. Looks like a merging issue;
proc sql noprint;
	create table count_yr as
		select distinct year_1
			from input.masterfile_2006q1_2020q4
				where naics_1 is not missing
;
quit; *2020;
proc sql noprint;
	create table count_yr2 as
		select distinct year
			from input.masterfile_2006q1_2020q4
				where naics_1 is missing
;
quit;
*2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019;
proc sql noprint;
	create table count_yr3 as
		select distinct year
			from input.masterfile_2006q1_2020q4
				where naics is not missing
;
quit;
*2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018;

*----------------------------------------------------------------------------------------
*	Year 2019 data check
*----------------------------------------------------------------------------------------;
proc sql;
	create table df_employment_oh_2019 as
		select *
			from input.df_employment_oh_2019;
quit;
*NAICS code variable is missing for year 2019;


*----------------------------------------------------------------------------------------
*	Illegible data entries i.e. observations creating unnecessary spikes in naics code 52 and 62
*----------------------------------------------------------------------------------------;
* identifying cause of spike in 2012Q2 (Finance and Insurance);
proc sql;
	create table spike_oh_2012q2 as 
		select * 
			from input.masterfile_2006q1_2020q4 (keep = Year Quarter UIN EIN Legal Address City State Zip m1 m2 m3 Wage)
				where strip(EIN) = "043583679"
					order by year, quarter
;
quit;
*043583679: Liberty Mutual group, had fake/illegible data entries. Remove.;

* identifying cause of spike in NAICS code:62 in 2010Q2 (Health care) in wage variable;
proc sql;
	create table spike_oh_2010q2 as 
		select * 
			from input.masterfile_2006q1_2020q4 (keep = Year Quarter UIN EIN NAICS Legal Address City State Zip m1 m2 m3 Wage)
				where strip(EIN) = "201731623"
					order by wage desc
;
quit;
*201731623: CHS-CLERMONT NCC, INC., OPR., had one illegible data entry. Remove.;

* identifying cause of spike in NAICS code:62 in 2013Q4 (Health care) in number-employed variable;
proc sql;
	create table spike_oh_2013q4 as 
		select *,sum(m1, m2, m3) as num_employed
			from input.masterfile_2006q1_2020q4 (keep = Year Quarter UIN EIN NAICS Legal Address City State Zip m1 m2 m3 Wage)
				where strip(EIN) = "462603341" 
					order by calculated num_employed desc
;
quit;
*462603341: ENTERAHEALTH INC, had one illegible data entry for M1 (equal to 998,000). Remove.;

