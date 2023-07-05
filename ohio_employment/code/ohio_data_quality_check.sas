/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Employment Project
|Purpose   : perform data  quality check. compares master dataset with QCEW reports by ODJFS (source: https://ohiolmi.com/Home/QCEW/QCEWpubs)
|History   : Date       By Description
			 7jul2023   SR 
\*=================================================================================================*/

%let in = C:\QCEW Data - Ohio\ES202\extracts;
%let dataset = masterfile_2006q1_2021q2; 

libname input "&in.";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

* importing dataset;
proc sql;
	create table df as
		select *, sum(m1,m2,m3) as emp_persons
			from input.&dataset.
/*				where strip(meei) ^= "3"*/
;
quit;

* aggregate by qtr, year and meei;
proc sql;
	create table df_yr_qtr_mei as
		select quarter, year, meei, sum(wage) as tot_wages, sum(emp_persons) as tot_emp_persons
			from df
			 group by quarter, year, meei
;
quit;

* aggregate by qtr, year;
proc sql;
	create table df_yr_qtr as
		select quarter, year, sum(wage) as tot_wages, sum(emp_persons) as avg_emp_persons
			from df
			 group by quarter, year
;
quit;

* aggregate by year (take the average of employed person, not the sum);
proc sql;
	create table df_yr as
		select year, sum(tot_wages) as tot_wages, mean(avg_emp_persons) as avg_emp_persons
			from df_yr_qtr
			 group by year
;
quit;


