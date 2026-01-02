/*=================================================================================================*\
|Author(s) : Saani Rawat
|Purpose   : Creating Jobs and firms related creation and destruction rates
|History   : Date 		By Description
			 12 Sep 23  SR Used ODJFS data file with TENDIGIT_FIPS to prepare outcome vars for Ohio Taxation project
|Inputs	   : odjfs_employment_df.sas7bdat
|Outputs   : out.df_sort_mth, out.df_job_vars_mth_fips, out.df_empl_all_mth_fips, out.df_empl_all_mth
\*=================================================================================================*/

%let start_time = %sysfunc(time());

%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects;
%let root_emp = &root.\ohio_employment;
%let root_tax = &root.\ohio_taxation;
%let out = &root_tax.\data\employment;
libname emp_data "&root_emp.\data";
libname out "&out.";

proc printto new log="&out.\odjfs_data_setup.log";
run;


*----------------------------------------------------------------------------------------
*	importing & prepping ODJFS data (all NAICS codes)
*----------------------------------------------------------------------------------------;
*qtrly;
proc sql;
	create table df as
		select intnx("qtr",yyq(year,quarter),0,"end") as date format = yyq6.,
				*,
				substr(strip(put(naics,best6.)),1,2) as naics_2dg, 
				substr(strip(put(naics,best6.)),1,3) as naics_3dg
			from emp_data.odjfs_employment_df
				where strip(state) = "OH"
;
quit;
*padding zeroes for year 2020;
data df;
	set df;
		 pad = put(input(pad,best3.),z3.);
		 RepUnit = put(input(RepUnit,best5.),z5.);
		 uin = put(input(uin,best7.),z7.);
		 unique_id = catx(strip(pad),strip(uin),strip(repunit));
run;
* sorting dataset by unique_id and date;
proc sort data=df  out=df_sort;
	by unique_id tendigit_fips date;
run;
* converting quarterly data into monthly data;
proc transpose data=df_sort out=df_sort_m (drop=_label_ wage rename=(col1 = num_employed)) NAME=month;
	by unique_id tendigit_fips	date	Year	Quarter	Pad	UIN	RepUnit	EIN	PUIN	PRUN	SUIN	SRUN	Legal	Trade	Address	City	State	Zip	Zip4	RUD	MEEI	OrgType	County	naics	Wage	pl_zip NAME NAMELSAD INTPTLAT INTPTLON naics_2dg	naics_3dg;
	var m1 m2 m3;
run;
proc sql;
	create table out.df_sort_mth (drop = date2 month2) as 
		select  input(substr(strip(month2),2),2.) as month,
				intnx("month",intnx("qtr",yyq(year,quarter),0,"beg"),calculated month-1,"end")  as date format = date9.,
				*
			from df_sort_m (rename = (date = date2 month = month2))
		;
quit;

*----------------------------------------------------------------------------------------
*	Job creation and Job destruction variables
*----------------------------------------------------------------------------------------;
* storing all distinct dates in a list and requisite date column names;
proc sql noprint;
	select distinct date into :dts_qtr separated by " "
		from df_sort;
quit;
%let dates_dts_qtr =date_%sysfunc(tranwrd(&dts_qtr.,%str( ),%str(_df date_)))_df;
proc sql noprint;
	select distinct date into :dts_mth separated by " "
		from out.df_sort_mth;
quit;
%let dates_dts_mth =date_%sysfunc(tranwrd(&dts_mth.,%str( ),%str(_df date_)))_df;
%put &=dts_qtr;
%put &=dates_dts_qtr;
%put &=dates_dts_mth;

/*%let df = df_sort_mth;*/
/*%let sub_category = all;*/
/*%let county_sub = ;*/

/*%macro test(df = , sub_category = all, county_sub = );*/
/*	%if (&sub_category. ne ) %then %do;*/
/*		%put WARNING: sub_category is &sub_category.;*/
/*	%end;*/
/*	%else %do;*/
/*		%put WARNING: sub_category is ALL;*/
/*	%end;*/
/*%mend;*/
/*%test;*/
/*%macro test2(x = );*/
/*	%put WARNING: x is %sysfunc(findw("Messi"|Messi|623|624,&x.,|));*/
/*%mend;*/
/*%test2(x = Messi);*/


* macro that creates dataset containing jobs created and destroyed variables;					  
%macro make_vars(df = , sub_category = all, county_sub = );
	*setting macro vars;
	%let dts = &dts_mth.;
	%let dates_dts = &dates_dts_mth.;

	* transposing to get unique_id as rows and quarters as columns. This deals with firms which originate and vanish in between the periods.;
	%util_dat_pivot_wider(df = &df. (keep = unique_id date num_employed 
											%if (&sub_category. ne all)  %then %do;
												naics_3dg
												where=(naics_3dg = "&sub_category.")
											%end;
											%if (&county_sub. ne ) %then %do;
												tendigit_fips
												where = (tendigit_fips = &county_sub.)
											%end;
										   ) 
	                      , out_df = df_sorted
	                      , names_from = date
	                      , values_from = num_employed
						  , names_prefix = date_
						  , values_fill = 0
	                      );

	* calculating the difference between two consecutive dates;
	data df_sorted2;
		set df_sorted 
						%if (&sub_category. ne all)  %then %do;
							(drop= naics_3dg)
						%end; 
						%if (&county_sub. ne ) %then %do;
							(drop= tendigit_fips)
						%end;
						;
			date_%scan(&dts.,1,' ')_df = 0;
		%do i = 2 %to %sysfunc(countw(&dts.));
			%let dt1 = %scan(&dts.,%eval(&i.-1),' ');
			%let dt2 = %scan(&dts.,&i.,' ');
			date_&dt2._df = date_&dt2. - date_&dt1.; 
		%end;
		keep unique_id _name_ &dates_dts.;
	run;
	* storing all positive changes in jobs created df;
	data df_jobs_created;
		set df_sorted2;
		%do j = 1 %to %sysfunc(countw(&dates_dts.));
			%let dt = %scan(&dates_dts.,&j.,' ');
			if &dt.>= 0 then &dt. = &dt.;
			else &dt. = 0;
		%end;
	run;
	* storing all negative changes in jobs destroyed df;
	data df_jobs_destroyed;
		set df_sorted2;
		%do j = 1 %to %sysfunc(countw(&dates_dts.));
			%let dt = %scan(&dates_dts.,&j.,' ');
			if &dt. < 0 then &dt. = &dt.;
			else &dt. = 0;
		%end;
	run;
	* aggregating for each time period AND each tendigit_fips;
	proc summary data=df_jobs_created;
		var &dates_dts.;
		output out=df_jobs_created_agg (drop = _type_ _freq_) sum = ;
	run;
	proc summary data=df_jobs_destroyed;
		var &dates_dts.;
		output out=df_jobs_destroyed_agg (drop = _type_ _freq_) sum = ;
	run;
	*transposing back to create a time series of jobs created and jobs destroyed;
	%util_dat_pivot_longer(df = df_jobs_created_agg
	                      , out_df = df_jobs_created_agg_t
						  , names_to = date
						  , values_to = jobs_created
	                      );
	%util_dat_pivot_longer(df = df_jobs_destroyed_agg
	                      , out_df = df_jobs_destroyed_agg_t
						  , names_to = date
						  , values_to = jobs_destroyed
	                      );
	*merging jobs created and jobs destroyed datasets;
	proc sql;
		create table 
					%if (&county_sub. eq )  %then %do;
						work.df_job_vars_&sub_category.
					%end;
					%if (&county_sub. ne ) %then %do;
						work.df_job_vars_&county_sub._&sub_category.
					%end; 
							(drop = date_a date_b) as 
				select compress(date_a,"date_f") as date, (jobs_created + jobs_destroyed) as  change_in_num_employed, *
						, "&sub_category." as category
					%if (&county_sub. ne ) %then %do;
						, &county_sub. as tendigit_fips
					%end;
					from df_jobs_created_agg_t (rename = (date = date_a)) as a, 
						 df_jobs_destroyed_agg_t (rename = (date = date_b)) as b
						where a.date_a = b.date_b;
	quit;
			
%mend make_vars;
*three cases: all categories and all county subdivisions, one specific category, or one specific subdivision;
/*%make_vars(df = out.df_sort_mth, sub_category = all);*/
/*%make_vars(df = out.df_sort_mth, sub_category = 621);*/
/*%make_vars(df = out.df_sort_mth, county_sub = 3906115000, sub_category = all);*/
/*data df_sort_mth_sub;*/
/*	set out.df_sort_mth (obs=100000);*/
/*run;*/
proc sql noprint;
	select distinct tendigit_fips into :unique_fips separated by " "
		from out.df_sort_mth;
quit;
/*%put &=unique_fips;*/

*macro to loop over each county subdivision;
%macro loop_over_make_vars(data = , sub_category = all, county_sub_list = , out_df = );
	data &out_df.;
	    length date $17. category $3.; 
	    format change_in_num_employed jobs_created jobs_destroyed tendigit_fips best32.;	    
	    date = "";
	    change_in_num_employed = .;
	    jobs_created = .;
	    jobs_destroyed = .;
	    category = "";
	    tendigit_fips = .;
	    stop; 
	run;

	%do qq = 1 %to %sysfunc(countw(&county_sub_list.));
		%let county_sub = %scan(&county_sub_list.,&qq.,' ');
/*		%put WARNING: county_sub is &county_sub.;*/
		%make_vars(df = &data., sub_category = &sub_category., county_sub = %scan(&county_sub_list.,&qq.,' '));
/*		%put WARNING: Finshed iteration number : &qq.;*/
        proc append base=&out_df. data=work.df_job_vars_&county_sub._&sub_category. force; 
        run;
        proc datasets library=work nolist;
            delete df_job_vars_&county_sub._&sub_category.;
        run;
	%end;

%mend loop_over_make_vars;
/*%let ct_list = %scan(&unique_fips., 1, ' ') %scan(&unique_fips., 2, ' ') %scan(&unique_fips., 3, ' ');*/
%loop_over_make_vars(data = out.df_sort_mth, sub_category = all, county_sub_list = &unique_fips., out_df = out.df_job_vars_mth_fips);

%let end_time = %sysfunc(time());
%let timeTaken = %sysevalf(%sysfunc(mdy(1,1,1960))+&end_time.-&start_time.);
%let timeTaken = %sysfunc(putn(&timeTaken.,e8601tm15.6));

%put NOTE: Program took &timeTaken. to run;
proc printto log=log;
run;


*----------------------------------------------------------------------------------------
*	make aggregate employment variable
*----------------------------------------------------------------------------------------;
* aggregating by date to cross-check jobs created and destroyed numbers later;	
%util_dat_aggregate(
                   df       = out.df_sort_mth
                 , out_df   = out.df_empl_all_mth_fips
                 , group    = date tendigit_fips
                 , sum      = num_employed 
);
%util_dat_aggregate(
                   df       = out.df_sort_mth
                 , out_df   = out.df_empl_all_mth
                 , group    = date
                 , sum      = num_employed 
);
/*%util_plt_line(df = df_empl_all_mth*/
/*                , x = date*/
/*                , y = num_employed*/
/*                , x_lab = "Date"*/
/*                , y_lab = "Number of Employed persons"*/
/*                , title = "Employment level"*/
/*                , subtitle = "2006 - 2021"*/
/*                , highlight_x_start = "30sep2007"D "31dec2019"d */
/*                , highlight_x_end   = "30jun2009"D "30jun2021"d*/
/*                , legend_lab = ""*/
/*				, highlight_x_lab = "Recession period" */
/*                , y_scale = comma20.*/
/*);*/
