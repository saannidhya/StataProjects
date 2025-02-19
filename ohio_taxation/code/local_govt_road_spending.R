#==========================================================================================================#
# Purpose : Computing the value of local government spending cuts when you fail a road tax levy
# Name    : Saani Rawat
# Created : 01/28/2025
# Log     : 
#           01/28/2025: computing the "money lost"
#           02/18/2025: computing the reduction in revenue and spending after a cut in taxes
#==========================================================================================================#

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
renewed <- readxl::read_xlsx(paste0(spend_reports_loc, "renewed/renewed_spending_reports.xlsx"))
cut     <- readxl::read_xlsx(paste0(spend_reports_loc, "cut/cut_spending_reports.xlsx"))

# importing cpi data (year beginning data)
cpi_df <- readr::read_csv(paste0(data,"/CPIAUCSL_NBD20100101.csv")) %>% rename(cpi = CPIAUCSL_NBD20100101) %>%
  mutate(year = lubridate::year(observation_date) - 1, cpi_deflator = cpi/100) # since base of 2010 has cpi of 100
# Note: shifting a year due to year-beg and year-end discrepancy in govt data and cpi  

renewed <- renewed %>% 
  left_join(select(cpi_df, c(year, cpi_deflator)), by = c("year")) %>%
  mutate(property_tax_d = property_tax/cpi_deflator,
         public_works_d = public_works/cpi_deflator)


## ggplot plotting a line

ggplot(data = renewed) +
  geom_line(aes(x = year, y = property_tax_d, color = "Property Tax")) +
  geom_line(aes(x = year, y = public_works_d, color = "Public Works")) +
  labs(title = "Property Tax and Public Works Spending",
       x = "Year",
       y = "Spending (deflated to 2010 $$)",
       color = "Spending Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  

