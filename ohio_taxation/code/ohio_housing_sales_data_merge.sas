*----------------------------------------------------------------------------------------------------
|Author(s) : Saani Rawat
|Source    : Ohio Taxation Project
|Purpose   : To append ohiohousesales9516_cleaned.csv (old) and ohiohousesales_1621_cleaned (new)
|History   : Date       By 		Description
			 07May22    SR		Started data merge
			 08May22    SR		Appended based on matching var names only
*----------------------------------------------------------------------------------------------------;


%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;
%let input = &root.\data;

libname input "&input.";
libname output "&root.\data\outputs";

%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);

ERRORS = 5;

*----------------------------------------------------------------------------------------
*	Importing data
*----------------------------------------------------------------------------------------;
* variable mapping table;
proc import datafile="&root.\docs\oh_housing_sales_variable_mapping.xlsx" dbms=xlsx replace out=var_mapping_tbl;
	sheet="final_mapping_table";
run;
*store var names old dataset;
proc sql noprint;
		select oh_house_sales_old into :var_names separated by " "
			from var_mapping_tbl;
quit;
* loading old data;
/*proc import datafile="&input.\ohiohousesales9516_cleaned.dta" dbms=STATA replace out=input.oh_sales_old;*/
/*run;*/
proc import datafile="&input.\outputs\ohiohousesales9516_cleaned.csv" dbms=csv replace out=oh_sales_old (keep=&var_names. where=(year^= 2016));
run; 
* loading new data;
/*proc import datafile="&input.\ohiohousesales_1621_cleaned.dta" dbms=STATA replace out=input.oh_sales_new;*/
/*run;*/
proc import datafile="&input.\outputs\ohiohousesales_1621_cleaned.csv" dbms=csv replace out=oh_sales_new (keep=&var_names.);
run;


*----------------------------------------------------------------------------------------
*	Conforming column types before appending datasets
* section, municipality_code, subdivision_tract_number, adjusted_gross_square_feet, stories_code, number_of_units, pool
*----------------------------------------------------------------------------------------;
data oh_sales_new2 (drop= section2 municipality_code2 subdivision_tract_number2 adjusted_gross_square_feet2);
	set oh_sales_new (rename=(section = section2 municipality_code = municipality_code2 subdivision_tract_number = subdivision_tract_number2 
							  adjusted_gross_square_feet = adjusted_gross_square_feet2 number_of_units = number_of_units2 pool = pool2));
		section = put(section2,7.);
		municipality_code = put(municipality_code2,10.);
		subdivision_tract_number = put(subdivision_tract_number2,10.);
		adjusted_gross_square_feet = input(adjusted_gross_square_feet2,best32.);
		number_of_units = input(number_of_units2,best32.);
		pool = input(pool2,best32.);
run;
data oh_sales_old2 (drop= stories_code2);
	set oh_sales_old (rename=(stories_code = stories_code2));
		stories_code = put(stories_code2,7.);
run;

*----------------------------------------------------------------------------------------
*	Appending old and new housing sales dataset (by name)
*   Note: Currently only using matching variable names
*----------------------------------------------------------------------------------------;

proc sql;
	create table output.oh_sales_df as
		select *
			from oh_sales_old2 as a
				union corr
		select *
			from oh_sales_new2 as b
;
quit;

%put NOTE: %util_aux_nobs(oh_sales_old);; *3,610,200;
%put NOTE: %util_aux_nobs(oh_sales_new);; *1,588,478;