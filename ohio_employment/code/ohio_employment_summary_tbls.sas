/*=================================================================================================*\
|Author(s) : Saani Rawat
|Purpose   : Ohio employment data: summary tables
|History   : Date       By Description
			 03-02-2022 SR Removed illegible data entries and then created summary tables and plots
\*=================================================================================================*/

* specify root before running;
%let root = C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_employment;
%let in = C:\QCEW Data - Ohio\ES202;

%include "C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions\util_load_macro_functions.sas";
%util_load_macro_functions(C:\Users\rawatsa\OneDrive - University of Cincinnati\sas_utility_functions,subfolder=1);
%util_fmt_std_graphics(debug=0);

libname input ("&in.","&in.\extracts");	
libname output "&root.\data";

*input master dataset;
proc sql;
	create table df as
		select *, substr(strip(put(naics,best6.)),1,2) as naics_2dg, sum(m1,m2,m3) as num_employed
			from input.masterfile_2006q1_2021q2
;
quit;

* summary table by 2-digit naics code, quarter and year;
%util_dat_aggregate(
                   df       = df
                 , out_df   = df_agg_q_y 
                 , group    = naics_2dg quarter year
                 , sum      = wage num_employed
);
%util_dat_aggregate(
                   df       = df
                 , out_df   = df_agg_y /*(where = (strip(naics_2dg) is not missing))*/
                 , group    = naics_2dg year
                 , sum      = wage num_employed
);
%util_dat_aggregate(
                   df       = df
                 , out_df   = df_agg 
                 , group    = year quarter 
                 , sum      = wage num_employed
);
%util_dat_aggregate(
                   df       = master_og_2006q1_2020q4
                 , out_df   = master_agg_2006q1_2020q4
                 , group    = year quarter 
                 , sum      = wage num_employed
);
* creating date column for plots;
data df_agg;
	set df_agg;
	date = yyq(year,quarter); format date yyq6.;
run;
data master_agg_2006q1_2020q4;
	set master_agg_2006q1_2020q4;
	date = yyq(year,quarter); format date yyq6.;
run;

*----------------------------------------------------------------------------------------
*	NAICS code processing
*----------------------------------------------------------------------------------------;

*addings naics 2-digit code names;
proc import datafile="&root.\data\naics_codes_table.csv" dbms=csv out=naics_codes replace; run;
data df_agg_q_y2;
	set df_agg_q_y;
	naics_2dg_cd = input(strip(naics_2dg),best32.);
	date = yyq(year,quarter); format date yyq6.;
run;

*----------------------------------------------------------------------------------------
*	Summary tables
*----------------------------------------------------------------------------------------;
proc sql;
create table output.summry_tbl (where = (strip(naics_2dg) ^= "0"))  as
	select *
		from df_agg_q_y2 as a
			left join naics_codes as b
				on a.naics_2dg_cd = b.naics_2dg_code
					where naics_2dg_cd is not missing 
							and strip(naics_2dg) ^= "00" 
							and strip(naics_2dg) ^= "5"
						order by naics_2dg_cd, year, quarter
	;
quit;
proc sql;
	create table output.summry_tbl_by_indstry (where = (strip(industry) ^= "")) as
		select industry, date, sum(wage) as wage, sum(num_employed) as num_employed
			from output.summry_tbl
				group by industry, date
;
quit;

*----------------------------------------------------------------------------------------
*	Plots
*----------------------------------------------------------------------------------------;

* plotting quarterly data by 2-digit NAICS;
*num employed;
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("finance and insurance",
							"health care and social assistance",
							"information",
							"management of companies and enterprises",
							"manufacturing",
							"mining, quarrying, and oil and gas extraction",
							"other services (except public administration)")))
                , x = date
                , y = num_employed
                , color = industry
                , x_lab = "Date"
                , y_lab = "Number of Employed persons"
                , title = "Ohio: Employment level by Industry (chart 1)"
                , subtitle = "2006 - 2021"
/*                , highlight_x_start = "01jan2019"D*/
/*                , highlight_x_end   = "31dec2019"D*/
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("accommodation and food services",
							"administrative and support and waste management and remediation services",
							"agriculture, forestry, fishing and hunting",
							"arts, entertainment, and recreation",
							"construction",
							"educational services") ))
                , x = date
                , y = num_employed
                , color = industry
                , x_lab = "Date"
                , y_lab = "Number of Employed persons"
                , title = "Ohio: Employment level by Industry (chart 2)"
                , subtitle = "2006 - 2021"
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("professional, scientific, and technical services",
							"public administration",
							"real estate and rental and leasing",
							"retail trade",
							"transportation and warehousing",
							"utilities",
							"wholesale trade")))

                , x = date
                , y = num_employed
                , color = industry
                , x_lab = "Date"
                , y_lab = "Number of Employed persons"
                , title = "Ohio: Employment level by Industry (chart 3)"
                , subtitle = "2006 - 2021"
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);
* wages;
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("finance and insurance",
							"health care and social assistance",
							"information",
							"management of companies and enterprises",
							"manufacturing",
							"mining, quarrying, and oil and gas extraction",
							"other services (except public administration)") ))
                , x = date
                , y = wage
                , color = industry
                , x_lab = "Date"
                , y_lab = "Wages"
                , title = "Ohio: Wages level by Industry (chart 1)"
                , subtitle = "2006 - 2018"
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("accommodation and food services",
							"administrative and support and waste management and remediation services",
							"agriculture, forestry, fishing and hunting",
							"arts, entertainment, and recreation",
							"construction",
							"educational services") ))
                , x = date
                , y = wage
                , color = industry
                , x_lab = "Date"
                , y_lab = "Number of Employed persons"
                , title = "Ohio: Employment level by Industry (chart 2)"
                , subtitle = "2006 - 2021"
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);
%util_plt_line(df = output.summry_tbl_by_indstry (where = (lowcase(strip(industry)) in 
						   ("professional, scientific, and technical services",
							"public administration",
							"real estate and rental and leasing",
							"retail trade",
							"transportation and warehousing",
							"utilities",
							"wholesale trade")))

                , x = date
                , y = wage
                , color = industry
                , x_lab = "Date"
                , y_lab = "Number of Employed persons"
                , title = "Ohio: Employment level by Industry (chart 3)"
                , subtitle = "2006 - 2021"
                , legend_lab = ""
                , y_scale = comma20.
                , color_palette = &palette_OkabeIto.
);