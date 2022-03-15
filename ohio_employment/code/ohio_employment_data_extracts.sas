/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Employment Project
|Purpose   : Create individual datasets for each year using masterfile_2006q1_2020q4.sas7bdat
|History   : Date       By Description
			 18feb2022  SR Checked validity of masterfile_2006q1_2020q4 file. Found one merging issue (year 2020)
			 07mar2022	SR Fixed 2020 merge issue. Added new 2021 data (upto Q2). Create master df file (2006-2021Q2)
\*=================================================================================================*/

%let in_loc = C:\QCEW Data - Ohio\ES202;

libname in ("&in_loc.","&in_loc.\2021");
libname out "&in_loc.\extracts";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*create master df in sas7bdat;
proc import datafile="&in_loc.\MasterFile_2006Q1_2020Q4.dta" dbms=STATA replace out=masterfile_2006q1_2020q4; run;
proc import datafile="&in_loc.\UC_2020_1.dta" dbms=STATA replace out=df_2020q1 (drop = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own); run;
proc import datafile="&in_loc.\UC_2020_2.dta" dbms=STATA replace out=df_2020q2 (drop = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own); run;
proc import datafile="&in_loc.\UC_2020_3.dta" dbms=STATA replace out=df_2020q3 (drop = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own); run;
proc import datafile="&in_loc.\UC_2020_4.dta" dbms=STATA replace out=df_2020q4 (drop = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own); run;
proc import datafile="&in_loc.\2021\current_UCMA2021Q1.csv" dbms = csv replace out=df_2021q1 (drop = comment cipseaflag phone liab_date add_source eol_date react_date auxnaics own); run;
proc import datafile="&in_loc.\2021\current_UCMA2021Q2.csv" dbms = csv replace out=df_2021q2 (drop = comment cipseaflag phone liab_date add_source eol_date react_date auxnaics own); run;

* cleaning 2021 data files as they have different variable names (compared to previous years);
proc datasets lib=work nolist;
	modify df_2021q1;
		rename rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county;

	modify df_2021q2;
		rename rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county;

quit;

%macro loop(qtr_list = 2020q1 2020q2 2020q3 2020q4 2021q1 2021q2);
	%do i = 1 %to %sysfunc(countw(&qtr_list.));
		data df_%scan(&qtr_list.,&i.," ") (drop= naics2 year2 quarter2 county2 m1_ m2_ m3_ wage_);
			retain year quarter ;
			set df_%scan(&qtr_list.,&i.," ") (rename= (naics = naics2 year = year2 quarter = quarter2 county = county2 m1 = m1_ m2 = m2_ m3 = m3_ wage = wage_));
				naics = input(naics2, best32.);
				year = input(year2, best32.);
				quarter = input(quarter2, best32.);
				county = input(county2, best32.);
				m1 = input(m1_, best32.);
				m2 = input(m2_, best32.);
				m3 = input(m3_, best32.);
				wage = input(wage_, best32.);
		run;
	%end;
%mend ;
%loop();

*removing illegible entries using EINs (see ohio_data_checks.sas for more details);
*removing duplicate columns created due to incorrect merge for 2020;
proc sql;
	create table out.MasterFile_2006Q1_2019Q4 as 
		select 	  Year
				, Quarter
				, Pad
				, UIN
				, RepUnit
				, EIN
				, PUIN
				, PRUN
				, SUIN
				, SRUN
				, Legal
				, Trade
				, Address
				, City
				, State
				, Zip
				, Zip4
				, RUD
				, MEEI
				, OrgType
				, County
				, input(naics, best32.) as naics
				, M1
				, M2
				, M3
				, Wage
			from masterfile_2006q1_2020q4
				where strip(EIN) ^= "043583679" and 
					  strip(EIN) ^= "201731623" and 
					  strip(EIN) ^= "462603341" and
					  year <= 2019

;
quit;

*mostly ohio, some obs from outside. But how come we have so many observations from outside OH?;
proc sql;
	create table temp as 
		select state, count(*)
			from out.MasterFile_2006Q1_2019Q4
				group by state;
quit;

*checking if variable names match before appending;
%util_aux_colnames( lib = out
                  , df    = MasterFile_2006Q1_2019Q4
                  , out_m_colnames = master_cols
);
%util_aux_colnames( lib = work
                  , df    = df_2020q1
                  , out_m_colnames = df_2020q1_cols
);
%util_aux_colnames( lib = work
                  , df    = df_2021q1
                  , out_m_colnames = df_2021q1_cols
);
%put &=master_cols;
%put &=df_2020q1_cols;
%put &=df_2021q1_cols;

*if dataset already exists, then delete it. Necessary before appending;
%if %sysfunc(exist(out.masterfile_2006q1_2021q2)) %then %do;
	%util_dat_delete_df(df = out.masterfile_2006q1_2021q2);
%end;

*append all years together (2006 onwards);
proc sql;
	create table out.masterfile_2006q1_2021q2 as 
	   select *
	   	  from out.masterfile_2006q1_2019q4
		   outer union corr
	   select *
	      from df_2020q1	
		   outer union corr
	   select *
	      from df_2020q2
		   outer union corr
	   select *
	      from df_2020q3
		   outer union corr
	   select *
	   	  from df_2020q4
		   outer union corr
	   select *
	      from df_2021q1
		   outer union corr
	   select *
	   	  from df_2021q2
	;
quit;

*dividing master dataset into subsets by year;
%macro separate_master_DF();
	proc sql noprint;
			select distinct year into :year_list separated by " "
				from out.masterfile_2006q1_2021q2
					where year is not missing
	;
	quit;
	%do i = 1 %to %sysfunc(countw(&year_list.));
		proc sql;
			create table out.df_employment_oh_%scan(&year_list.,&i.,' ') as 
				select *
					from out.masterfile_2006q1_2021q2
						where year = %scan(&year_list.,&i.,' ');
		quit;
	%end;

%mend separate_master_DF;
%separate_master_DF();




