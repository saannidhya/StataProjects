#==========================================================================================================#
# Purpose : Data setup before programs. Loads all housing datasets. Filters, cleans and aggregates datasets
# Name    : Saani Rawat
# Created : 07/27/2022
# Log     : separated data setup from original program 
#==========================================================================================================#

# loading packages
packages <- c("Rbearcat", "tidyverse", "lubridate", "haven", "stringr", "here", "knitr", "janitor", "scales","data.table","rdrobust")
for (pkg in packages){
  library(pkg, character.only = TRUE)
}

# global vars
cutoff <- 50

# specify the shared location
shared <- "//cobshares.uccob.uc.edu/economics$/Julia/roads"


# covariates list 
covs_list <- c("TENDIGIT_FIPS", "year" ,"TENDIGIT_FIPS_year" ,"pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl", "inctaxrate")


# loading census df
census <- haven::read_dta(paste0(data,"/cosub_place_panel_property2_9018.dta")) %>%
  dplyr::select(covs_list) %>%
  dplyr::rename(vote_year = year) %>%
  janitor::clean_names()



#============================================#
#  Importing Housing datasets as a list ----
#============================================#


# storing all housing dfs as a list
dataset_names <- stringr::str_remove(list.files(shared,
                                                pattern = "matches",
                                                recursive = TRUE),
                                     paste0(".", "dta")) 
# import data
housing_dfs <- purrr::map(list.files(shared,
                                     pattern = "matches",
                                     recursive = TRUE,
                                     full.names = TRUE),
                          haven::read_dta)
# assign names to housing dfs
housing_dfs <- stats::setNames(housing_dfs, dataset_names)


#============================================#
# Data Cleaning ----
#============================================#
# filtering all dfs: keeping renewals, dropping levies that last forever and dropping missing values of sale amount
dfs <- purrr::map(housing_dfs, ~.x %>% 
                    filter(description == "R" & duration != 1000) %>%
                    janitor::clean_names() %>%
                    drop_na(sale_amount) %>%
                    mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0),
                           ln_sale_amount = log(sale_amount))            
)

# |- filtering ---- 
# get colnames from t+1 to t+10
# yr_t_plus_names <- dfs$housing_roads_census_t_plus_10_matches %>% select(starts_with("yr_t_plus")) %>% colnames() %>% sort()
yr_t_names <- dfs$housing_roads_census_t_plus_10_matches %>% select(starts_with("yr_t_")) %>% colnames() %>% sort()

# create aggregate datasets 
# Dataset contains the following indices:
# i: tendigit_fips, t = year
dfs_agg <- purrr::map2(.x = dfs, .y = yr_t_names, ~ .x %>% 
                         drop_na(sale_amount) %>%
                         group_by(tendigit_fips, eval(parse(text = .y)), year, votes_pct_for) %>%
                         rename(vote_year = year, year = `eval(parse(text = .y))`) %>%
                         summarize(median_sale_amount = median(sale_amount, na.rm = TRUE),
                                   median_ln_sale_amount = median(ln_sale_amount, na.rm = TRUE))              
)

# datasets with covariates
dfs_agg_covs <- purrr::map(.x = dfs_agg, ~ .x %>% 
                                  dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                                  ungroup()
                           )

# creating a dataset with sale_amount_per_sq_feet separately.
# Reasoning: for sale_amount_per_sq_feet, both variables (sale_amount and universal_building_square_feet) have to be non missing.
#            if created sale_amount_per_sq_feet in dfs list above, some observations which require only sale_amount for 
#            median_sale_amount variable might be dropped.

dfs_agg_per <- purrr::map2(housing_dfs, yr_t_names, function(df,yr){
                dataset <- df %>% 
                            filter(description == "R" & duration != 1000) %>%
                            janitor::clean_names() %>%
                            drop_na(sale_amount) %>%
                            drop_na(universal_building_square_feet) %>%
                            mutate(sale_amount_per_sq_feet = sale_amount/universal_building_square_feet)
                dataset_summarize <- dataset %>%
                          group_by(tendigit_fips, eval(parse(text = yr)), year, votes_pct_for) %>%
                          rename(vote_year = year, year = `eval(parse(text = yr))`) %>%
                          summarize(median_sale_amount_per_sq_feet = median(sale_amount_per_sq_feet, na.rm = TRUE))   
                return(dataset_summarize)
      }
      )

# purrr::map(dfs_agg, ~.x %>% nrow())
# purrr::map(dfs_agg_per, ~.x %>% nrow())
# purrr::map(housing_dfs, ~.x %>% nrow())
