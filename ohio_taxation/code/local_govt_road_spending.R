#==============================================================================================================#
# Purpose : Computing the value of local government spending cuts when you fail a road tax levy
# Name    : Saani Rawat
# Created : 01/28/2025
# Log     : 
#           01/28/2025: computing the "money lost"
#           02/18/2025: computing the reduction in revenue and spending after a cut in taxes
#           03/04/2025: Computing the "money lost" as a % of total roads budget for areas with close elections
#==============================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")
spend_reports_loc <- paste0(data,"/spending reports/")

source(paste0(code,"/utility_functions.R"))

# running data setup code
source(paste0(code,"/housing_data_setup.R"))


#============================================================================================================#
#     Computing the "money lost" i.e. cost to local govt of failing a road tax levy
#============================================================================================================#

#> Methodology:
#> Payable taxes per year per household = millagepercent x 0.35 x avg appraised value
#> Money lost = Payable taxes per year per household x n.o of households


nrow(roads_and_census2)
colnames(roads_and_census2)
colnames(roads_and_census)

summary(as.numeric(roads_and_census$millagepercent))

#> summary(as.numeric(roads_and_census$millagepercent))
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.045   1.000   2.000   1.874   2.000   8.000 
# sort(unique(roads_and_census$millagepercent))

##=== avg appraisal value calculation ===##

# original housing dataset
hs <- haven::read_dta(paste0(shared,"/housesales_9521_slim.dta"))
head(hs)
View(hs)

hs2 <- hs %>% filter(!is.na(SALE_AMOUNT)) 
# gg %>% filter(!is.na(SALE_AMOUNT)) 
# nrow(hs) - hs %>% filter(is.na(SALE_AMOUNT)) %>% nrow()

hs_winsorized <- winsorize_data(list(hs2) , "SALE_AMOUNT") # truncating the 1% tails

# group by fips and year to compute mean sale price per year. This will be our "appraised value".
# Note: sale_amount has been deflated to 2010 $$.
hs_app <- hs_winsorized[[1]] %>% janitor::clean_names() %>%
            group_by(tendigit_fips, year) %>% 
            summarize(avg_appraised_val = mean(sale_amount)) %>%
            mutate(tendigit_fips = as.numeric(tendigit_fips), year = as.numeric(year))

##=== merging with referendums data ===##
rf_data <- roads_and_census %>%
  left_join(hs_app, by = c("tendigit_fips", "year")) %>%
  filter(!is.na(avg_appraised_val)) %>% 
  arrange(tendigit_fips, year)

##=== computing tax val per HH per year ===##
tax_val <- rf_data %>% 
              mutate(tax_per_hh_per_yr = (as.numeric(millagepercent)/1000) * 0.35 * avg_appraised_val,
                     num_houses = pop/2.39,
                     tax_per_yr = tax_per_hh_per_yr*num_houses) %>% # 2.39 people per household in 2020 (as per ohio.gov: https://dam.assets.ohio.gov/image/upload/development.ohio.gov/research/census/20230829-census-2020-demographic-profile-charting-the-changes.pdf) 
                    relocate(c("tax_per_hh_per_yr", "avg_appraised_val", "num_houses", "tax_per_yr"), .before = millagepercent)

# aggregate
summary(tax_val$tax_per_hh_per_yr)
sd(tax_val$tax_per_hh_per_yr)

summary(tax_val$tax_per_yr)
sd(tax_val$tax_per_yr)

# by renewed and cut
tax_val %>% 
  group_by(treated) %>%
  summarize(mean_tax_per_hh_per_yr = mean(tax_per_hh_per_yr), sd_tax_per_hh_per_yr = sd(tax_per_hh_per_yr))

tax_val %>% 
  group_by(treated) %>%
  summarize(mean_tax_tax_per_yr = mean(tax_per_yr), sd_tax_per_yr = sd(tax_per_yr))

tax_val %>% filter(treated == 1) %>% pull(tax_per_yr) %>% summary()
tax_val %>% filter(treated == 1) %>% pull(tax_per_hh_per_yr) %>% summary()

roads_and_census$tendigit_fips %>% unique %>% length

summary(tax_val$tax_per_yr)


#============================================================================================================#
#     Estimating the reduction in revenue and spending after a cut in taxes
#============================================================================================================#

# importing spending data (year end data)
renewed <- readxl::read_xlsx(paste0(spend_reports_loc, "renewed/renewed_spending_reports.xlsx")) %>%
  filter(grepl("township", name, ignore.case = TRUE)) %>%
  mutate(name = tolower(name))

# only keep rows that contain the word "Township" in their names
# Note: numbers for cities are unreliable. Thus, doing analysis ONLy on townships within bw.

cut     <- readxl::read_xlsx(paste0(spend_reports_loc, "cut/cut_spending_reports.xlsx")) %>%
filter(grepl("township", name, ignore.case = TRUE)) %>% 
  mutate(name = tolower(name))

# Note: numbers for cities are unreliable. Thus, doing analysis ONLy on townships within bw.

# importing cpi data (year beginning data)
cpi_df <- readr::read_csv(paste0(data,"/CPIAUCSL_NBD20100101.csv")) %>% rename(cpi = CPIAUCSL_NBD20100101) %>%
  mutate(year = lubridate::year(observation_date) - 1, cpi_deflator = cpi/100) # since base of 2010 has cpi of 100
# Note: shifting a year due to year-beg and year-end discrepancy in govt data and cpi  

renewed <- renewed %>% 
  left_join(select(cpi_df, c(year, cpi_deflator)), by = c("year")) %>%
  mutate(property_tax_d = property_tax/cpi_deflator,
         public_works_d = public_works/cpi_deflator)

renewed2 <- renewed %>%
  separate(name, into = c("township", "county"), sep = ",") %>%
  mutate(
    township = trimws(township),
    county = trimws(gsub("county", "", county))
)



cut <- cut %>% 
  left_join(select(cpi_df, c(year, cpi_deflator)), by = c("year")) %>%
  mutate(property_tax_d = property_tax/cpi_deflator,
         public_works_d = public_works/cpi_deflator)

cut2 <- cut %>%
  separate(name, into = c("township", "county"), sep = ",") %>%
  mutate(
    township = trimws(township),
    county = trimws(gsub("county", "", county))
  )

# importing spendin analysis excel
last_votes <- readxl::read_xlsx(paste0(spend_reports_loc, "spending_analysis.xlsx"), sheet = "rd_fips_close_elections_gs_bw") %>%
  mutate(subdivision = tolower(subdivision),
         county = tolower(county)) %>%
  rename(township = subdivision) %>%
  select(tendigit_fips, township, county, max_year, pop)
  
# joining renewed and last_votes by township and county
renewed_ <- renewed2 %>%
  inner_join(last_votes, by = c("township", "county")) %>%
  mutate(after_election_flag = if_else(year >= max_year, 1, 0)) %>%
  arrange(township, county, year)

# joining cut and last_votes by township and county
cut_ <- cut2 %>%
  inner_join(last_votes, by = c("township", "county")) %>%
  mutate(after_election_flag = if_else(year >= max_year, 1, 0)) %>%
  arrange(township, county, year)


## Analysis ##

# Regression for property_tax_d
renewed_mod_property_tax <- lm(property_tax_d ~ after_election_flag + factor(year) + factor(tendigit_fips), 
                         data = renewed_)
summary(renewed_mod_property_tax)

# Regression for public_works_d
renewed_mod_public_works <- lm(public_works_d ~ after_election_flag + factor(year) + factor(tendigit_fips), 
                         data = renewed_)
summary(renewed_mod_public_works)



# Regression for property_tax_d
cut_mod_property_tax <- lm(property_tax_d ~ after_election_flag + factor(year) + factor(tendigit_fips), 
                           data = cut_)
summary(cut_mod_property_tax)
mean(cut_$property_tax_d)
median(cut_$property_tax_d)

# Regression for public_works_d
cut_mod_public_works <- lm(public_works_d ~ after_election_flag + factor(year) + factor(tendigit_fips), 
                           data = cut_)
summary(cut_mod_public_works)

# Coefficient is negative, but not stat significant, likely due to limited number of observations.



#============================================================================================================#
#      Computing the "money lost" as a % of public works expense
#============================================================================================================#

# importing township_reports_all.csv
all_townships <- readxl::read_xlsx(paste0(spend_reports_loc, "/township_reports_all.xlsx")) %>%
  filter(grepl("township", name, ignore.case = TRUE)) %>%
  mutate(name = tolower(name)) %>%
  separate(name, into = c("township", "county"), sep = ",") %>%
  mutate(
    township = trimws(township),
    county = trimws(gsub("county", "", county))
  ) %>% rename(subdivision = township)
# importing subdivision and county name
cty_sub_names <- readxl::read_excel(paste0(data,"/ohio-only-all-geocodes-2016.xlsx")) %>% janitor::clean_names() %>% 
  select(all_of(c("tendigit_fips", "name_note_if_split_between_two_counties", "county_name", "split_flag"))) %>% 
  rename(subdivision = name_note_if_split_between_two_counties, county = county_name) %>%
  mutate(subdivision = tolower(if_else(split_flag == 1,
                               trimws(str_replace(subdivision, "(village|city).*", "\\1")),
                               subdivision)),
         county = tolower(county))

# joining all_townships and cty_sub_names by township and county

fips_list <- c(3902374119, 3915162988, 3915142168, 3908174608, 3909356966, 3915319036, 3911377504, 3911377504, 3900729624, 3903573264, 3903580990, 3908585484, 3909963968, 3913946578, 3915156294, 3902351912, 3902978890, 3904781718, 3906176028, 3908559430, 3909975126, 3915162078, 3915328448, 3905503590, 3905911003, 3905704720, 3905704724, 3913303086, 3900902750, 3903526446, 3904129694, 3905513988, 3908518196, 3908523618, 3908546494, 3909903198, 3915141314, 3915318658, 3901366628, 3902346788, 3902923730, 3903310030, 3904361714, 3906116616, 3906131752, 3908549056, 3908559416, 3909907468, 3915112000, 3915138094, 3915374130, 3917341328)

close_cty_sub_names <- cty_sub_names %>% filter(tendigit_fips %in% fips_list)


close_townships <- all_townships %>% 
                      inner_join(close_cty_sub_names, by = c("subdivision", "county")) %>%
                      left_join(select(cpi_df, c(year, cpi_deflator)), by = "year" ) %>%
                      mutate(public_works_d = public_works/cpi_deflator)
  
mean(close_townships$public_works_d, na.rm = TRUE)
median(close_townships$public_works_d, na.rm = TRUE)
# > mean(close_townships$public_works_d, na.rm = TRUE)
# [1] 1528404
# > median(close_townships$public_works_d, na.rm = TRUE)
# [1] 1299030