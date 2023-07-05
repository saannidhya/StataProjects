#==========================================================================================================#
# Purpose : Employment Data setup before programs. Loads all employment datasets. Filters, cleans and aggregates datasets
# Name    : Saani Rawat
# Created : 07/03/2023
# Log     : 
#           07/03/2023: 
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects"
data_emp <- paste0(root,"/ohio_employment/data")
code_emp <- paste0(root,"/ohio_employment/code")
data_tax <- paste0(root,"/ohio_taxation/data")
code_tax <- paste0(root,"/ohio_taxation/code")

# loading packages
packages <- c("Rbearcat", "tidyverse", "lubridate", "haven", "stringr", "here", "knitr", "janitor", "scales","data.table","rdrobust")
check_and_install <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
lapply(packages, check_and_install)

#================================================================#
#  loading employment dataset (with geocoded addresses) ----
#================================================================#

employment_df <- haven::read_sas(paste0(data_emp,"/odjfs_employment_df.sas7bdat")) %>%
                  janitor::clean_names()  %>% 
                  mutate(persons = (m1 + m2 + m3)/3, 
                         unique_id = paste0(pad,uin,rep_unit))

# unique_id column has problems because when you paste the three cols, some unique_ids tend to overlap i.e. 0 + 10 + 1 = 0 + 1 + 10 = 0101 

employment_df %>% select(meei) %>% unique()

#================================================================#
#  Data checks ----
#================================================================#
# df is your data frame
duplicate_rows <- employment_df[duplicated(employment_df[, c("year", "quarter", "pad", "uin", "rep_unit")]) | 
                       duplicated(employment_df[, c("year", "quarter", "pad", "uin", "rep_unit")], fromLast = TRUE),]

# print duplicate rows
# View(duplicate_rows)

# Note: for year 2019 and quarter 3 & 4, some of the observations were assigned two different geocodes (long and lat) by ArcGIS.
#       I do not know why this might be (54 observations)


# employment_df %>% filter((uin == "0831615") & (rep_unit == "00155")) %>% View()
## need to remove the following observations:
#================================================================#
# 1.  uin == 1425962, rep == 00000, tendigit_fips == 3915326166
# 2.  uin == 1156401, rep == 00000, tendigit_fips == 3909955118
# 3.  uin == 0607666, rep == 00000, tendigit_fips == 3902940824
# 4.  uin == 0831615, rep == 00155, tendigit_fips == 3911381494
# 5.  uin == 1657015, rep == 00004, tendigit_fips == 3915901336 (no prev quarter to compare with, arbitarily chose 3915901336 to remove)
# 6.  uin == 1543034, rep == 00019, tendigit_fips == 3904541740
# 7.  uin == 0773502, rep == 00000, tendigit_fips == 3904981242
# 8.  uin == 1722768, rep == 00000, tendigit_fips == 3909344856  (no prev quarter to compare with, arbitarily chose 3909344856 to remove)
# 9.  uin == 0981827, rep == 00000, tendigit_fips == 3909372067
# 10. uin == 1316409, rep == 00000, tendigit_fips == 3904918000
# 11. uin == 1457637, rep == 00000, tendigit_fips == 3900551688
# 12. uin == 0957724, rep == 00000, tendigit_fips == 3904983342
# 13. uin == 1715275, rep == 00000, tendigit_fips == 3915508056
# 14. uin == 1478390, rep == 00000, tendigit_fips == 3901384602
# 15. uin == 0733251, rep == 00103, tendigit_fips == 3903571416
# 16. uin == 1438339, rep == 00000, tendigit_fips == 3914779674
# 17. uin == 0716484, rep == 00000, tendigit_fips == 3904983342
# 18. uin == 1248178, rep == 00000, tendigit_fips == 3915528098 
# 19. uin == 1471008, rep == 00000, tendigit_fips == 3915528098 
# 20. uin == 1688663, rep == 00000, tendigit_fips == 3903766656
# 21. uin == 1495014, rep == 00034, tendigit_fips == 3915580892
# 22. uin == 1457285, rep == 00000, tendigit_fips == 3903504500
# 23. uin == 1512191, rep == 00000, tendigit_fips == 3916521238
# 24. uin == 0726873, rep == 00122, tendigit_fips == 3903537240 (prev quarters were unreliable, arbitarily chose 3909344856 to remove)
# 25. uin == 0726873, rep == 00131, tendigit_fips == 3906115000
# 26. uin == 1495014, rep == 00003, tendigit_fips == 3905704220
# These observations were removed after checking year 2019 quarter 3,4 values for the specific uins with quarters 1, 2
#================================================================#


# Create a data frame with the above 26 conditions for removal
remove_df <- data.frame(
  uin = c("1425962", "1156401", "0607666", "0831615", "1657015", "1543034", "0773502", "1722768", "0981827", 
          "1316409", "1457637", "0957724", "1715275", "1478390", "0733251", "1438339", "0716484", "1248178", 
          "1471008", "1688663", "1495014", "1457285", "1512191", "0726873", "0726873", "1495014"),
  rep_unit = c('00000', '00000', '00000', '00155', '00004', '00019', '00000', '00000', '00000', 
          '00000', '00000', '00000', '00000', '00000', '00103', '00000', '00000', '00000', 
          '00000', '00000', '00034', '00000', '00000', '00122', '00131', '00003'),
  tendigit_fips = c(3915326166, 3909955118, 3902940824, 3911381494, 3915901336, 3904541740, 
                    3904981242, 3909344856, 3909372067, 3904918000, 3900551688, 3904983342, 
                    3915508056, 3901384602, 3903571416, 3914779674, 3904983342, 3915528098, 
                    3915528098, 3903766656, 3915580892, 3903504500, 3916521238, 3903537240, 
                    3906115000, 3905704220)
)

# Suppose your original data frame is named df
# Remove the rows
employment_df2 <- anti_join(employment_df, remove_df, by = c("uin", "rep_unit", "tendigit_fips"))


# num observations removed
nrow(employment_df) -  nrow(employment_df2)


# if we check duplicates now: 
employment_df2[duplicated(employment_df2[, c("year", "quarter", "pad", "uin", "rep_unit")]) | 
                duplicated(employment_df2[, c("year", "quarter", "pad", "uin", "rep_unit")], fromLast = TRUE),]

# no duplicates left now based on year, quarter, pad, uin, rep_unit

# View(employment_df2)

#============================================#
# Data Cleaning & Aggregation ----
#============================================#

emp <- employment_df2 %>% 
        select(c(year, quarter, pad, uin, rep_unit, tendigit_fips, ein, naics, wage, persons))



# aggregating by fips, year and quarter
emp_df_agg_fips_yr_qtr <- emp %>% 
  group_by(tendigit_fips, year, quarter) %>%
  summarise(tot_wages = sum(wage, na.rm = TRUE),
            tot_persons = sum(persons, na.rm = TRUE))
emp_df_agg_fips_yr_qtr %>% View()


# Aggregating further by year and quarter only for comparison with QCEW reports by ODJFS (see "emp_benchmarking" tab in excel sheet)

emp_df_agg_yr_qtr <- emp_df_agg_fips_yr_qtr %>% 
  group_by(year, quarter) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = sum(tot_persons, na.rm = TRUE))

# Aggregating by fips, year
emp_df_agg_fips_yr <- emp_df_agg_fips_yr_qtr %>% 
  group_by(tendigit_fips, year) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = mean(tot_persons, na.rm = TRUE))


# (73 + 94 + 106 + 101)/4
# 127779 + 158007 + 189911 + 162227

#==========================================================#
# Merging employment data with census variables data ----
#==========================================================#



#==========================================================#
# Merging employment + census data with roads data ----
#==========================================================#
