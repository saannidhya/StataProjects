/*=================================================================================================*\
|Author(s) : Saani Rawat 
|Purpose   : To merge voting and us census datasets
|History   : Date       By Description
|          2022-03-09 SR Initial version. 
\*=================================================================================================*/

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;

%let data = &root.\data;

libname data "&data.";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);


* loading tax levy data;
proc import datafile="&data.\tax_levies_ohio7.xlsx" dbms=xlsx replace out=tax_levies_oh;
run;

* loading us census data;
proc import datafile="&data.\cosub_place_panel_property2_9018.dta" dbms=STATA replace out=census_df (where=(tendigit_fips ^= .));
run;

*-------------------------------------------------------;
*merging two datasets: road tax levy and us census data ;
*-------------------------------------------------------;
%util_dat_clean_names(df = tax_levies_oh)

* taking only road taxes;
proc sql;
	create table road_tax_levies_oh as 
		select  *, 
				votes_for/(votes_for + votes_against) as prop_votes_for, 
				case 
					when calculated prop_votes_for < 0.5 then "fail"
					else "pass"
				end as vote_result
			from tax_levies_oh
				where TENDIGIT_FIPS is not missing and strip(lowcase(purpose2)) = "roads"
					order by tendigit_fips, year;
run;

*merging;
proc sql;
	create table data.road_tax_and_census_oh as 
		select *   
			from road_tax_levies_oh as a
				left join census_df (rename=(TENDIGIT_FIPS = TENDIGIT_FIPS_c year = year_c)) as b
					on a.TENDIGIT_FIPS=b.TENDIGIT_FIPS_c and a.year=b.year_c
						order by tendigit_fips, year;
run;

