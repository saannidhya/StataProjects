/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Employment Project
|Purpose   : Create master dataset from individual files provided by Ohio JFS (via Dr. Michael Jones)
|History   : Date       By Description
			 18feb2022  SR Checked validity of masterfile_2006q1_2020q4 file. Found one merging issue (year 2020)
			 07mar2022	SR Fixed 2020 merge issue. Added new 2021 data (upto Q2). Create master df file (2006-2021Q2)
			 06apr2022  SR Added data with NAICS code. Fixed issue of MEEI == 2 as per Dr. Jones' suggestion
			 28apr2023  SR Created a new dataset that contains only unique address for Haiqing Liu. Dataset name: unique_addresses.sas7bdat
			 30may2023  SR Cleaned unique_addresses.sas7bdat to remove spurious addresses
\*=================================================================================================*/

%let in_loc = C:\QCEW Data - Ohio\ES202;
%let out_csv = &in_loc.\extracts;

libname in ("&in_loc.","&in_loc.\2021");
libname out "&in_loc.\extracts";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*Import macro;
%macro import_df(in_loc = , df = , file_type = , out_df = , drop_list = , rename_list = );

	proc import datafile="&in_loc.\&df." 
		%if %sysfunc(findw(%upcase(&file_type.), CSV)) %then %do; dbms = csv %end;
		%if %sysfunc(findw(%upcase(&file_type.), STATA)) %then %do; dbms = stata %end;
		replace 
		out = &out_df.
	(
				%if &drop_list. ^= %then %do;
					drop = &drop_list.
				%end;
				%if &rename_list. ^= %then %do;
					rename = (&rename_list.)
				%end;
			where=(strip(meei) ^= "2")  /*removing meei == 2*/
	)
	;
	run;

%mend import_df;

*----------------------------------------------------------------------------------------
*	Importing data
*----------------------------------------------------------------------------------------;
*2006-2018 will be used;
proc import datafile="&in_loc.\MasterFile_2006Q1_2020Q4.dta" dbms=STATA replace out=masterfile_2006q1_2020q4; run;
*2019;
%import_df(in_loc = &in_loc.,
			df = current_UCMA191.csv, file_type = csv, out_df = df_2019q1, 
			drop_list = comment cipseaflag phone auxnaics own add_source	liab_date	eol_date	react_date, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county
)
%import_df(in_loc = &in_loc.,
			df = current_UCMA192.csv, file_type = csv, out_df = df_2019q2, 
			drop_list = comment cipseaflag phone auxnaics own add_source	liab_date	eol_date	react_date, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county
)
%import_df(in_loc = &in_loc.,
			df = current_UCMA193.csv, file_type = csv, out_df = df_2019q3, 
			drop_list = comment cipseaflag phone auxnaics own add_source	liab_date	eol_date	react_date, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county
)
%import_df(in_loc = &in_loc.,
			df = current_UCMA194.csv, file_type = csv, out_df = df_2019q4, 
			drop_list = comment cipseaflag phone auxnaics own add_source	liab_date	eol_date	react_date, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zip = zip pl_zipx = zip4 org_type = orgtype cnty = county
)
* 2020;
%import_df(in_loc = &in_loc., df = UC_2020_1.dta, file_type = stata, out_df = df_2020q1, drop_list = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own)
%import_df(in_loc = &in_loc., df = UC_2020_2.dta, file_type = stata, out_df = df_2020q2, drop_list = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own)
%import_df(in_loc = &in_loc., df = UC_2020_3.dta, file_type = stata, out_df = df_2020q3, drop_list = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own)
%import_df(in_loc = &in_loc., df = UC_2020_4.dta, file_type = stata, out_df = df_2020q4, drop_list = comment cipseaflag phone liabdate addsource eoldate reactdate auxnaics own)
*2021;
%import_df(in_loc = &in_loc.\2021,
			df = current_UCMA2021Q1.csv, file_type = csv, out_df = df_2021q1, 
			drop_list = comment cipseaflag phone liab_date add_source eol_date react_date auxnaics own, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zipx = zip4 org_type = orgtype cnty = county
)
%import_df(in_loc = &in_loc.\2021,
			df = current_UCMA2021Q2.csv, file_type = csv, out_df = df_2021q2, 
			drop_list = comment cipseaflag phone liab_date add_source eol_date react_date auxnaics own, 
			rename_list = rep_unit = repunit pl_ad1 = address pl_city = city pl_state = state pl_zipx = zip4 org_type = orgtype cnty = county
)

*----------------------------------------------------------------------------------------
*	Cleaning data
*----------------------------------------------------------------------------------------;

* converting data type for each variable;
%macro loop(qtr_list = 2019q1 2019q2 2019q3 2019q4 2020q1 2020q2 2020q3 2020q4 2021q1 2021q2);
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
* for 2006-2018;
proc sql;
	create table  MasterFile_2006Q1_2018Q4 as 
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
					  year <= 2018

;
quit;

*----------------------------------------------------------------------------------------
*	Exporting Master Data
*----------------------------------------------------------------------------------------;

*append all years together (2006 onwards) and exporting as sas dataset;
proc sql;
	create table out.masterfile_2006q1_2021q2	(where = (strip(EIN) ^= "043583679" and 
														  strip(EIN) ^= "201731623" and 
														  strip(EIN) ^= "462603341")) 
				as 
	   select *
	   	  from masterfile_2006q1_2018q4
		   outer union corr
	   select *
	      from df_2019q1	
		   outer union corr
	   select *
	      from df_2019q2	
		   outer union corr
	   select *
	      from df_2019q3	
		   outer union corr
	   select *
	      from df_2019q4	
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
*19,435,152 obs;


*----------------------------------------------------------------------------------------
*	Creating a dataset that contains unique addresses
*----------------------------------------------------------------------------------------;

proc sql;
	create table unique_addresses as
		select distinct address, city, state, zip
			from out.masterfile_2006q1_2021q2;
quit;
* 1,144,121 obs. Some are blank.


*----------------------------------------------------------------------------------------
*	cleaning unique addresses
*----------------------------------------------------------------------------------------;
*importing and removing text issues;
proc sql;
	create table out.unique_addresses as
		select strip(address) as Address, strip(city) as City, state, zip
			from unique_addresses
				where address is not missing and 
					  not strip(address) in ("**ADDRESS NEEDED**", "** ADDRESS NEEDED **", ".", "0",",", "1", "'","NONE", "NO ADDRESS PROVIDED") and 
					  strip(state) = "OH"
;
quit;

*----------------------------------------------------------------------------------------
*	exporting cleaned unique_addresses dataset to a csv file for ArcGIS Pro
*----------------------------------------------------------------------------------------;
proc export data=out.unique_addresses
   outfile="&out_csv.\unique_addresses.csv"
   dbms=csv replace;
   putnames=yes;
run;

