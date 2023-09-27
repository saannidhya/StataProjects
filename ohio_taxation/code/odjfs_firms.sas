/*=================================================================================================*\
|Author(s) : Saani Rawat
|Purpose   : Creating firms related creation and destruction rates
|History   : Date 		By Description
			 14 Sep 23  SR Used ODJFS data file with TENDIGIT_FIPS to prepare outcome vars for Ohio Taxation project
|Inputs	   : in.df_sort_mth
|Outputs   : 
\*=================================================================================================*/

%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects;
%let root_emp = &root.\ohio_employment;
%let root_tax = &root.\ohio_taxation;
%let out = &root_tax.\data\employment;
libname emp_data "&root_emp.\data";
libname out "&out.";

*----------------------------------------------------------------------------------------
*	importing monthly, unit-level ODJFS data upto 2018 (all NAICS codes).
* 	Creating firm count variables at monthly and yearly level
*----------------------------------------------------------------------------------------;
proc sql;
	create table df_sort_mth as
		select *
			from out.df_sort_mth (obs =100000)
				where year <= 2018
;
quit;

proc sql;
    create table firms_per_fips_mth as
	    select tendigit_fips, date, count(distinct unique_id) as firms_count
 		   from out.df_sort_mth
			 where year <= 2018 
    			group by tendigit_fips, date;
quit;

proc sql;
    create table firms_per_fips_yr as
	    select tendigit_fips, year, count(distinct unique_id) as firms_count
 		   from out.df_sort_mth
			 where year <= 2018 
    			group by tendigit_fips, year;
quit;

proc sql;
	create table fips_cnt as
		select distinct tendigit_fips
			from out.df_sort_mth
			 where year <= 2018;
quit;

data out.df_firm_vars_fips_yr (drop= firms_count_lag);
	set firms_per_fips_yr;
		by tendigit_fips;
		firms_count_lag = lag(firms_count);
			if first.tendigit_fips then firms_count_lag = .;
			if firms_count_lag ne . then do;
				firms_created = firms_count - firms_count_lag;
				firm_creation_rate = (firms_count - firms_count_lag)/firms_count_lag;
			end;
			else do;
				firms_created = .;
				firm_creation_rate = . ;
			end;
			firm_change_rate = firm_creation_rate;
			firms_change = firms_created;
			if firm_creation_rate < 0 then firm_destruction_rate = firm_creation_rate;
			else firm_destruction_rate = 0;
			if firms_created < 0 then firm_destroyed = firms_created;
			else firm_destroyed = 0;
			if (firm_creation_rate < 0) and (firm_creation_rate ne .) then firm_creation_rate = 0;
			if (firms_created < 0) and (firms_created ne .) then firms_created = 0;
run;

%util_dat_aggregate(df = out.df_firm_vars_fips_yr,
out_df = test,
group = year,
sum = firms_count)

data test2;
    set test;
    date = mdy(1, 1, Year); * Convert the year to the January 1st date of that year;
    format date date9.; * This will make the date display in a YYYY-MM-DD format;
run;


%util_plt_line(df = test2
                , x = date
                , y = firms_count
                , x_lab = "Date"
                , y_lab = "N.o of firms"
                , title = "Firms in Ohio"
                , subtitle = "2006 - 2018"
                , highlight_x_start = "30sep2007"D 
                , highlight_x_end   = "30jun2009"D 
                , legend_lab = ""
				, highlight_x_lab = "Recession period" 
                , y_scale = comma20.
);

