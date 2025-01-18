#==========================================================================================================#
# Purpose : Data setup before programs. Loads all housing datasets. Filters, cleans and aggregates datasets
# Name    : Saani Rawat
# Created : 07/27/2022
# Log     : 
#           07/27/2022: separated data setup from original program 
#           06/17/2023: combined several previously made changes. See github repo for details
#           09/11/2023: replaced STATA created "merged" datasets with identical csv files from ohio_road_housing_census_merge.R
#           02/17/2024: replaced csv files with STATA created "merged" (a.k.a "matches") datasets
#           07/23/2024: added mean and sd info needed for the paper
#           07/26/2024: computing top 10 renewals and cuts from road levies dataset 
#           10/15/2024: created house price growth variable (relative to vote year and YOY)
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
vars_list <- c("TENDIGIT_FIPS", "year" ,"pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")


# loading census df
# census <- haven::read_dta(paste0(data,"/cosub_place_panel_property2_9018.dta")) %>%
census <- haven::read_dta(paste0(data,"/census_property_9021.dta")) %>%
  dplyr::select(vars_list) %>%
  dplyr::rename(vote_year = year) %>%
  janitor::clean_names()

# importing roads and census dataset. Selecting only renewals and levies that do not last forever. Separating into treatment and control groups.
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != "1000") %>%
  janitor::clean_names() %>%
  mutate(votes_pct_against = 100 - votes_pct_for) %>%
  mutate(treated = if_else(votes_pct_against > cutoff, 1, 0))    


#============================================#
#  Importing Housing datasets as a list ----
#============================================#
# storing all housing dfs as a list
dataset_names <- stringr::str_remove(list.files(paste0(shared),
                                                pattern = "matches",
                                                recursive = FALSE),
                                     paste0(".", "dta")) 
# import data
housing_dfs <- purrr::map(list.files(paste0(shared),
                                     pattern = "matches",
                                     recursive = FALSE,
                                     full.names = TRUE),
                          haven::read_dta) 
# assign names to housing dfs
housing_dfs <- stats::setNames(housing_dfs, dataset_names)


#============================================#
# Data Cleaning ----
#============================================#
# filtering all dfs: keeping renewals, dropping levies that last forever and dropping missing values of sale amount
dfs <- purrr::map(housing_dfs, ~.x %>% 
                    filter(description == "R" & duration != "1000") %>%
                    janitor::clean_names() %>%
                    drop_na(sale_amount) %>%
                    mutate(votes_pct_against = 100 - votes_pct_for) %>%
                    mutate(treated = if_else(votes_pct_against > cutoff, 1, 0),
                           ln_sale_amount = log(sale_amount))            
)


# |- filtering ---- 
# get colnames from t+10
# yr_t_plus_names <- dfs$housing_roads_census_t_plus_10_matches %>% select(starts_with("yr_t_plus")) %>% colnames() %>% sort()
yr_t_names <- dfs$housing_roads_census_t_plus_10_matches %>% select(starts_with("yr_t_")) %>% colnames() %>% sort()

# create aggregate datasets 
# Dataset contains the following indices:
# i: tendigit_fips, t = year
dfs_agg <- purrr::map2(.x = dfs, .y = yr_t_names, ~ .x %>% 
                         drop_na(sale_amount) %>%
                         group_by(tendigit_fips, eval(parse(text = .y)), year, votes_pct_against) %>%
                         rename(vote_year = year, year = `eval(parse(text = .y))`) %>%
                         summarize(median_sale_amount = median(sale_amount, na.rm = TRUE),
                                   median_ln_sale_amount = median(ln_sale_amount, na.rm = TRUE)) 
)

# creating a dataset with sale_amount_per_sq_feet separately.
# Reasoning: for sale_amount_per_sq_feet, both variables (sale_amount and universal_building_square_feet) have to be non missing.
#            if created sale_amount_per_sq_feet in dfs list above, some observations which require only sale_amount for 
#            median_sale_amount variable might be dropped.

dfs_agg_per <- purrr::map2(housing_dfs, yr_t_names, function(df,yr){
                dataset <- df %>% 
                            filter(description == "R" & duration != "1000") %>%
                            janitor::clean_names() %>%
                            drop_na(sale_amount) %>%
                            drop_na(universal_building_square_feet) %>%
                            mutate(sale_amount_per_sq_feet = sale_amount/universal_building_square_feet,
                                   votes_pct_against = 100 - votes_pct_for)
                dataset_summarize <- dataset %>%
                          group_by(tendigit_fips, eval(parse(text = yr)), year, votes_pct_against) %>%
                          rename(vote_year = year, year = `eval(parse(text = yr))`) %>%
                          summarize(median_sale_amount_per_sq_feet = median(sale_amount_per_sq_feet, na.rm = TRUE))   
                return(dataset_summarize)
      }
      )

dfs_agg_per_covs <- purrr::map(.x = dfs_agg_per, ~ .x %>% 
                             dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                             ungroup()
)

# purrr::map(dfs_agg, ~.x %>% nrow())
# purrr::map(dfs_agg_per, ~.x %>% nrow())
# purrr::map(housing_dfs, ~.x %>% nrow())


#============================================#
# Urban vs Rural ----
#============================================#
# importing the urban vs rural file
# twp_places_urban <- haven::read_dta(paste0(data,"/twp_places_urban.dta")) %>% janitor::clean_names()
# 
# dfs_agg_urb <- purrr::map(dfs_agg, ~ .x %>%
#                                      left_join(twp_places_urban, by = "tendigit_fips")
#                          )

# dfs_agg$housing_roads_census_t_plus_2_matches$tendigit_fips %>% unique() %>% length()
# twp_places_urban$tendigit_fips %>% unique() %>% length()
# write.csv(dfs_agg$housing_roads_census_t_plus_2_matches$tendigit_fips %>% unique(), "hf.csv", row.names = FALSE)
# write.csv(twp_places_urban$tendigit_fips %>% unique(), "twp.csv", row.names = FALSE)

# Why are there a good amount of non-matches?

# class(twp_places_urban$tendigit_fips)
# class(dfs_agg$housing_roads_census_t_plus_2_matches$tendigit_fips)


# purrr::map(dfs_agg, ~ .x %>%
#              
#              )


#============================================#
# Split by millage size ----
#============================================#
dfs_agg_mill <- purrr::map2(.x = dfs, .y = yr_t_names, ~ .x %>% 
                         drop_na(sale_amount) %>%
                         group_by(tendigit_fips, eval(parse(text = .y)), year, votes_pct_against, millagepercent) %>%
                         rename(vote_year = year, year = `eval(parse(text = .y))`) %>%
                         summarize(median_sale_amount = median(sale_amount, na.rm = TRUE),
                                   median_ln_sale_amount = median(ln_sale_amount, na.rm = TRUE))              
)

dfs_agg_mill_covs <- purrr::map(.x = dfs_agg_mill, ~ .x %>% 
                             dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                             ungroup()
)



# beepr::beep("mario")

#============================================#
# Mean and SD for Data section of paper ----
#============================================#

# mean and sd for sale amount
map_dbl(dfs, ~mean(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
# 166,082
map_dbl(dfs, ~sd(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()

housing_dfs_winsorized <- winsorize_data(dfs, "sale_amount", na.rm = TRUE)
# mean house value, keeping only t + 0 to t + 10
map_dbl(housing_dfs_winsorized, ~mean(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
map_dbl(housing_dfs_winsorized, ~sd(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()


#============================================#
# Top 10 renewals and cuts ----
#============================================#

roads_and_census2 <- roads_and_census %>%
  mutate(renewals = if_else(treated == 0, 1, 0),
         cuts = if_else(treated == 1, 1, 0))

roads_and_census2 %>%
  group_by(tendigit_fips, treated) %>%
  summarise(
    renewals = sum(renewals),
    cuts = sum(cuts)
  ) %>% 
  filter(treated == 0) %>%
  arrange(desc(renewals)) %>% head(10)

roads_and_census2 %>%
  group_by(tendigit_fips, treated) %>%
  summarise(
    renewals = sum(renewals),
    cuts = sum(cuts)
  ) %>% 
  filter(treated == 1) %>%
  arrange(desc(cuts)) %>% head(10)

# Top 10 renewals
top10_ren <- roads_and_census %>% 
  group_by(tendigit_fips) %>%
  count(treated) %>% 
  filter(treated == 0) %>%
  arrange(desc(n)) %>% head(10) %>% ungroup()
# top10_ren$tendigit_fips

# Top 10 cuts
top10_cuts <- roads_and_census %>% 
  group_by(tendigit_fips) %>%
  count(treated) %>% 
  filter(treated == 1) %>%
  arrange(desc(n)) %>% head(10) %>% ungroup()
# top10_cuts$tendigit_fips

# roads_and_census2 %>% filter(between(votes_pct_against, 35, 65)) %>% pull(tendigit_fips) %>% unique %>% as_tibble_col(column_name =  "tendigit_fips") %>%
#   write_csv(here("data/outputs/tables/fips_list_for_google_earth.csv"))

# roads_and_census %>% 
#   filter(tendigit_fips %in% top10_ren$tendigit_fips) %>% select(tendigit_fips, year, votes_pct_against, treated) %>% filter(treated == 0) %>% head(20)

# roads_and_census %>% 
  # filter(tendigit_fips %in% top10_cuts$tendigit_fips) %>% select(tendigit_fips, year, votes_pct_against, treated) %>% filter(treated == 1) %>% head(20)


#==================================================#
# House Price Growth (Relative to vote year)  ----
#==================================================#

# adding vote_year houses 
# r_c_h <- roads_and_census %>% 
#           left_join(hs_agg, by = c("tendigit_fips", "year")) %>% rename(num_houses_base = count) %>% 
#   mutate(yr_t_minus_3 = year - 3, 
#          yr_t_minus_2 = year - 2, 
#          yr_t_minus_1 = year - 1,
#          yr_t_plus_0 = year,
#          yr_t_plus_1 = year + 1,
#          yr_t_plus_2 = year + 2,
#          yr_t_plus_3 = year + 3,
#          yr_t_plus_4 = year + 4,
#          yr_t_plus_5 = year + 5,
#          yr_t_plus_6 = year + 6,
#          yr_t_plus_7 = year + 7,
#          yr_t_plus_8 = year + 8,
#          yr_t_plus_9 = year + 9,
#          yr_t_plus_10 = year + 10) %>%
#   select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
#   arrange(tendigit_fips, year)
# 
# hs_agg_mgd <- purrr::map(yrs, function(y){
#   hs_agg %>% rename_with(~ y , year) %>% 
#     inner_join(r_c_h, by = c("tendigit_fips", y)) %>%
#     select(-starts_with("yr_t"), all_of(y)) %>%
#     relocate(y, .after = tendigit_fips) %>%
#     mutate(median_sale_amount_growth = (median_sale_amount.x - median_sale_amount.y)/median_sale_amount.y ,
#            mean_sale_amount_growth = (mean_sale_amount.x - mean_sale_amount.y)/mean_sale_amount.y,
#            ) %>%
#     relocate(year, median_sale_amount_growth, mean_sale_amount_growth, treated, votes_pct_against, .after = y)
# })
# names(hs_agg_mgd) <- paste0(gsub("^yr", "housing_roads_census", yrs), "_matches")
# 
# 
# 
