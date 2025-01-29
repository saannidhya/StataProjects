#==========================================================================================================#
# Purpose : Computing the value of local government spending cuts when you fail a road tax levy
# Name    : Saani Rawat
# Created : 01/28/2025
# Log     : 
#           01/28/2025: 
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

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

summary(tax_val$tax_per_yr)
summary(tax_val$tax_per_hh_per_yr)

tax_val %>% filter(treated == 1) %>% pull(tax_per_yr) %>% summary()
tax_val %>% filter(treated == 1) %>% pull(tax_per_hh_per_yr) %>% summary()
