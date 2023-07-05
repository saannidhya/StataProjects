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
			 15jun2023  SR Used unique_addresses_spatial_join.csv, which was created in ArcGIS Pro after geocoding appropriately formatted 
						   addresses in unique_addresses.sas7bdat, to identify tendigitfips for each observation in masterfile_2006q1_2020q4 file for Ohio Taxation project.
\*=================================================================================================*/

*setting up macro variables;
%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_employment;
%let data = &root.\data;
%let data_gis = &data.\address_geocoding\address_geocoding_arcgis;
%let in_loc = C:\QCEW Data - Ohio\ES202;
%let out_csv = &in_loc.\extracts;

libname in ("&in_loc.","&in_loc.\2021");
libname out "&in_loc.\extracts";
libname data "&data.";

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
*	Converting the exported SAS dataset into a Stata dataset
*----------------------------------------------------------------------------------------;
proc export 
		    data=out.masterfile_2006q1_2021q2 
		    outfile="&out_csv.\masterfile_2006q1_2021q2.dta" 
		    dbms=dta 
		    replace;
run;

*----------------------------------------------------------------------------------------
*	Creating a dataset that contains unique addresses
*----------------------------------------------------------------------------------------;
proc sql;
	create table unique_addresses as
		select distinct address, city, state, county, zip
			from out.masterfile_2006q1_2021q2;
quit;
* 1,211,442 obs. Some are blank.;


*----------------------------------------------------------------------------------------
*	cleaning unique addresses
*----------------------------------------------------------------------------------------;
*importing and removing text issues;
proc sql;
	create table out.unique_addresses as
		select strip(lowcase(address)) as Address, strip(lowcase(city)) as City, state, zip
			from unique_addresses
				where address is not missing and 
					  not strip(lowcase(address)) in ("**address needed**", "** address needed **", ".", "0",",", "1", "'","none", "no address provided") and 
					  strip(state) = "OH" and
					  (index(address, '') = 0) and (index(city, '') = 0) /* removes 3 observations with this special character */
;
quit;

*identifying observations with special character  . ArcGIS Pro stops reading the file whenever it identifies this special character;
/*proc sql;*/
/*    create table spec_char_obs as*/
/*    select **/
/*    from unique_addresses*/
/*    where (index(address, '') > 0) or (index(city, '') > 0);*/
/*quit;*/

*----------------------------------------------------------------------------------------
*	exporting cleaned unique_addresses dataset to a csv file for ArcGIS Pro
*----------------------------------------------------------------------------------------;
proc export data=out.unique_addresses
   outfile="&out_csv.\unique_addresses.csv"
   dbms=csv replace;
   putnames=yes;
run;

*----------------------------------------------------------------------------------------
*	Dividing unique addresses file into different parts
*----------------------------------------------------------------------------------------;




*----------------------------------------------------------------------------------------
*	Combining out.masterfile_2006q1_2021q2 with geocoded unique addresses from ArcGIS Pro
*	Only "usual" unique addresses were matched by ArcGIS Pro.
*----------------------------------------------------------------------------------------;

proc import datafile="&data_gis.\unique_addresses_export_after_spatial_join.csv" dbms=csv replace 
			out=unique_addresses_w_fips (keep= Address	City	State	Zip GEOID	NAME	NAMELSAD INTPTLAT	INTPTLON rename=(geoid = tendigit_fips)); 
run;

data unique_addresses_w_fips_ (drop=zip rename=(zip_ = zip) );
 set unique_addresses_w_fips;
	Zip_ = PUT(Zip, best.);
run;


/*data unique_addresses_w_fips_2;*/
/* set unique_addresses_w_fips_;*/
/* 	where INTPTLON is missing;*/
/*run;*/

proc sql;
	create table unique_addresses_w_fips_2 as
	select distinct strip(lowcase(a.address)) as address,  strip(lowcase(a.city)) as city, strip(lowcase(a.state)) as state, strip(lowcase(a.zip)) as zip,
					tendigit_fips, NAME, NAMELSAD, INTPTLAT, INTPTLON
	from unique_addresses_w_fips_ as a;
quit;

proc sql;
	create table masterfile_2006q1_2021q2 (drop = address_ city_ state_ zip_) as
		select *
			from out.masterfile_2006q1_2021q2 as a
				left join unique_addresses_w_fips_2 (rename = (address=address_ city=city_ state=state_ zip=zip_)) as b
					on strip(lowcase(a.address)) = strip(lowcase(b.address_)) and 
					   strip(lowcase(a.city)) = strip(lowcase(b.city_)) and 
					   strip(lowcase(a.state)) = strip(lowcase(b.state_)) and 
					   strip(lowcase(a.zip)) = strip(lowcase(b.zip_));
quit;

/*%put WARNING: total obs: %util_aux_nobs(out.masterfile_2006q1_2021q2); *20,365,595;*/

proc sql;
	create table data.odjfs_employment_df as
		select *
			from masterfile_2006q1_2021q2 
				where INTPTLAT is not missing and INTPTLON is not missing;
quit;

* 20241727 obs;
/*%put WARNING: data.odjfs_employment_df obs: %util_aux_nobs(data.odjfs_employment_df); *4,306,668;*/


/*proc sql;*/
/*	create table data.odjfs_employment_df_no_zip (drop = address_ city_ state_ zip_) as*/
/*		select **/
/*			from out.masterfile_2006q1_2021q2 as a*/
/*				inner join unique_addresses_w_fips_ (rename = (address=address_ city=city_ state=state_ zip=zip_)) as b*/
/*					on strip(lowcase(a.address)) = strip(lowcase(b.address_)) and */
/*							strip(lowcase(a.city)) = strip(lowcase(b.city_)) and */
/*							strip(lowcase(a.state)) = strip(lowcase(b.state_)) */
/*							;*/
/*quit;*/
