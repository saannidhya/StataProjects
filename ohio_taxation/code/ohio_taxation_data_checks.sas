/*=================================================================================================*\
|Author(s) : Saani Rawat
|Source    : Ohio Taxation Project
|Purpose   : perform data checks and benchmarking on QCEW - Ohio data
|History   : Date       By Description
			 09Mar2022  SR Performing data checks on following datasets: cosub_place_county_votes_property2.dta, cosub_place_panel_property2_9018.dta
\*=================================================================================================*/

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;

%let data = &root.\data;
libname input "&root.\data";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

*importing data files;
proc import datafile="&root.\data\cosub_place_county_votes_property2.dta" dbms=STATA replace out=cosub_plc_cnty_votes_prpty; run;
proc import datafile="&root.\data\cosub_place_panel_property2_9018.dta" dbms=STATA replace out=cosub_plc_panel_prpty; run;
proc import datafile="&data.\cosub_place_panel_property2_9018.dta" dbms=STATA replace out=census_df (where=(tendigit_fips ^= .)); run;
proc import datafile="&root.\data\fire_votes_outcomes_9118.dta" dbms=STATA replace out=fire_votes; run;
proc import datafile="&data.\tax_levies_ohio7.xlsx" dbms=xlsx replace out=tax_levies_oh; run;

*------------------------------------------------------------;
*performing data checks: cosub_place_panel_property2_9018.dta;
*------------------------------------------------------------;

*sorting before counting;
proc sort data = cosub_plc_panel_prpty (where=(TENDIGIT_FIPS ^= .)) out=cosub_plc_panel_prpty2;
  by TENDIGIT_FIPS year;
run;

data cosub_plc_panel_prpty3;
	set cosub_plc_panel_prpty2;
			count + 1;
			by TENDIGIT_FIPS;
		if first.TENDIGIT_FIPS then do;
			count = 1;
		end;
run;

proc sql;
	create table tdgt_fips_count as
		select TENDIGIT_FIPS, count(*) as count
			from cosub_plc_panel_prpty3
				group by TENDIGIT_FIPS
					having calculated count ^= 29
;
quit;
*Note: 3911386656, 3915328448, 3915331664 do not have complete data;

data cosub_plc_panel_prpty4;
	set cosub_plc_panel_prpty3;
		where TENDIGIT_FIPS in (3911386656, 3915328448, 3915331664);
run;
*These 3 counties have unbalanced panel data.;

data cosub_plc_panel_prpty5;
	set cosub_plc_panel_prpty3;
		where TENDIGIT_FIPS not in (3911386656, 3915328448, 3915331664);
run;
*2253 unique sub-divisions and 29 years (1990-2019) gives 65,337. Now, we have balanced panel data.

*---------------------------------------------------;
*performing data checks: us census data;
*---------------------------------------------------;
proc sql;
	create table temp as 
		select distinct year
			from census_df;
run;
*years: 1990-2018 (29 years);
proc sql;
	create table temp as 
		select distinct TENDIGIT_FIPS
			from census_df
				where TENDIGIT_FIPS is not missing;
run;
*unique tendigit_fips: 2256;


*sorting before counting;
proc sort data = census_df (where=(TENDIGIT_FIPS ^= .)) out=census_df2;
  by TENDIGIT_FIPS year;
run;
*checking if any FIPS have unbalanced data;
proc sql;
	create table tdgt_fips_count as
		select TENDIGIT_FIPS, count(*) as count
			from census_df2
				group by TENDIGIT_FIPS
					having calculated count ^= 29
;
quit;
*Note: 3911386656 (Wright-Patterson), 3915328448 (Franklin Township), 3915331664 (Green Township) do not have complete data;

*total obs: 65,434;
proc sql;
	create table census_df3 as 
		select *
			from census_df2 where TENDIGIT_FIPS not in (.,3911386656, 3915328448, 3915331664);
run;
* 65,337 unique observations (same as for cosub_place_county_votes_property2.dta);

*---------------------------------------------------;
*performing data checks: tax levy data;
*---------------------------------------------------;
*key columns for tax levy data: TENDIGIT_FIPS, year, purpose2, votes for;
*In words, in one year, in one area, for one purpose, there cannot be more than one same amount of votes;

proc sql;
	create table x as
	select distinct purpose2
	from tax_levies_oh
;
quit;


proc sql;
	create table temp_tax as 
		select count(distinct TENDIGIT_FIPS)as TENDIGIT_FIPS, count(distinct year)as year, count(distinct purpose2) as purpose2 
			from tax_levies_oh;
quit;
*unique tendigit_fips: 2623
n.o of years: 28
n.o of tax purposes: 28;
*total tax levy obs: 52789;

* taking only road taxes;
proc sql;
	create table road_tax_levies_oh as 
		select * 
			from tax_levies_oh
				where TENDIGIT_FIPS is not missing and strip(lowcase(purpose2)) = "roads"
					order by tendigit_fips, year;
run;
* road tax levy obs: 4687;