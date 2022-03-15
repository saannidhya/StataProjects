/*=================================================================================================*\
|Author(s) : Saani Rawat 
|Purpose   : To create graph of road tax vote result (running variable) on different U.S census variables
|History   : Date     By Description
|            2022-03-09 SR create pdf files
\*=================================================================================================*/

%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation;

%let data = &root.\data;
%let out_plt = &data.\outputs\plots;
libname data "&data.";
libname out_plt "&data.\outputs\plots";

*loading in utility functions;
%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);


%macro running_plots(y_list = , x= );
	%do i = 1 %to %sysfunc(countw(&y_list.));
		%util_plt_point(df = data.road_tax_and_census_oh,
						x = &x.,
						y = %scan(&y_list.,&i.," ")
		)
	%end;
%mend running_plots;

proc contents noprint data=data.road_tax_and_census_oh out=vars (keep=
																	name type where=(type = 1 and 
																	name not in ("PLACEFP0","TENDIGIT_FIPS_c","dup","votes_against","votes_for","year","year_c")));
run;
proc sql noprint;
	select name into :y_list separated by " "
	from vars
;
quit;

*running all plots and savings as pdf;
goptions device=actximg;
ods pdf file= "&out_plt.\census_vars_vs_prop_votes_for.pdf" ;
%running_plots(y_list = &y_list., x = prop_votes_for)
ods pdf close;



