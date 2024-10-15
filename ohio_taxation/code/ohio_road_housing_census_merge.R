###################################################################################################################################################
# created: 07/09/2022
# by: Saani Rawat
# purpose: use STATA datasets to check if merge performed by ohio_road_housing_census_merge.do can be replicated
#          The datasets created should match the following: housing_roads_census_`t_type'_`t_abs'_matches.dta 
# Log:
# 1. 07/09/2022: created
# 2. 06/20/2023: updated to match the housing datasets. Only outputting housing_roads_census_`t_type'_`t_abs'_matches.dta
#                Note: Housing dataset housing_agg_roads_census_`t_type'_`t_abs'.dta" created in housing_data_setup.R (named dfs_agg)
#                Note: Housing dataset housing_agg_roads_census_per_`t_type'_`t_abs'.dta" created in housing_data_setup.R (named dfs_agg_per)
# 3. 09/04/2023: Added placebo cutoffs of t-3 and t+0
# 4. 10/15/2024: Generating Housing Price aggregate variable from housing prices 
###################################################################################################################################################



# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# specify the shared location
shared <- "//cobshares.uccob.uc.edu/economics$/Julia/roads"


#======================================================================#
# Roads dataset ----
#======================================================================#

cutoff = 50
rd_var_list = c("year", "pop", "TENDIGIT_FIPS", "TENDIGIT_FIPS_year", "childpov", "poverty", "pctwithkids", "pctsinparhhld", "pctnokids", "pctlesshs", "pcthsgrad", 
                "pctsomecoll", "pctbachelors", "pctgraddeg", "unemprate", "medfamy", "pctrent", "pctown", "pctlt5", "pct5to17", "pct18to64", "pct65pls", "pctwhite", 
                "pctblack", "pctamerind", "pctapi", "pctotherrace", "pctmin", "raceherfindahl", "pcthisp", "pctmarried", "pctnevermarr", "pctseparated", "pctdivorced", 
                "lforcepartrate", "incherfindahl", "inctaxrate", "tax_type", "purpose2", "description", "millage_percent", "duration", "votes_for", "votes_against")

# roads dataset
rd <- haven::read_dta(paste0(data,"/roads_levies2_census_9118.dta"))
rd_fips <- sort(unique(rd$TENDIGIT_FIPS))
# length(rd_fips)


roads_and_census <- rd %>%
                      select(all_of(rd_var_list)) %>% 
                      janitor::clean_names() %>%
                      mutate(votes_pct_for = (votes_for / (votes_for + votes_against))*100,
                             votes_pct_for_cntr = abs(votes_pct_for - cutoff)) %>%
                      group_by(tendigit_fips, year) %>% 
                      arrange(tendigit_fips, year, votes_pct_for_cntr) %>% 
                      mutate(count = row_number()) %>% 
                      filter(count == 1) %>% 
                      mutate(yr_t_minus_3 = year - 3, 
                             yr_t_minus_2 = year - 2, 
                             yr_t_minus_1 = year - 1,
                             yr_t_plus_0 = year,
                             yr_t_plus_1 = year + 1,
                             yr_t_plus_2 = year + 2,
                             yr_t_plus_3 = year + 3,
                             yr_t_plus_4 = year + 4,
                             yr_t_plus_5 = year + 5,
                             yr_t_plus_6 = year + 6,
                             yr_t_plus_7 = year + 7,
                             yr_t_plus_8 = year + 8,
                             yr_t_plus_9 = year + 9,
                             yr_t_plus_10 = year + 10,
                             rd_flag = 1) %>%
                    select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
                    arrange(tendigit_fips, year)


#======================================================================#
# Housing dataset ----
#======================================================================#

# housing dataset
hs <- haven::read_dta(paste0(shared,"/housesales_9521_slim.dta"))
head(hs)
View(hs)


# past and future years list
yrs <- c(paste0("yr_t_minus_",as.character(1:3)), paste0("yr_t_plus_",as.character(0:10)))

# hss <- purrr::map(yrs, ~ hs %>% 
#                    janitor::clean_names() %>%
#                    arrange(tendigit_fips, year) %>%
#                    rename( {{.x}} := "year") %>%
#                    mutate(sale_amount_per_sq_feet = sale_amount/universal_building_square_feet,
#                           tendigit_fips = as.numeric(tendigit_fips),
#                           {{.x}} := as.numeric({{.x}}),
#                           hs_flag = 1)
#                  )

# start.time <- Sys.time()
# creating copies of (slim) raw housing dataset such that we have t_minus and t_plus variables in the dataset (to merge on)
hss <- purrr::map(yrs, ~ hs %>% 
                    janitor::clean_names() %>%
                    arrange(tendigit_fips, year) %>%
                    mutate(sale_amount_per_sq_feet = sale_amount/universal_building_square_feet,
                           tendigit_fips = as.numeric(tendigit_fips),
                           hs_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year)) 
                    # arrange(tendigit_fips, {{.x}})
                  
)
names(hss) <- yrs

#========================================================================================================#
# Merging housing and roads (using TENDIGIT_FIPS and year variable : t periods ahead and behind) ----
# Keeping only matches (to keep non-matches too, use full_join insted)
#========================================================================================================#

mgd <- purrr::map2(hss, yrs, function(x, y){
                              x %>% inner_join(roads_and_census, by = c("tendigit_fips", y))
        })

names(mgd) <- paste0(gsub("^yr", "housing_roads_census", names(mgd)), "_matches")

# purrr::map_dbl(mgd, nrow) 

# purrr::imap(mgd, ~ write.csv(.x, paste0(data,"/housing/", .y, ".csv"), row.names = FALSE))
# beepr::beep("mario")

# Note: mgd contains datasets that match housing_roads_census_`t_type'_`t_abs'_matches.dta created in Stata

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
# Winsorization on full housing data for paper ----
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#


mean(hs %>% filter(!is.na(SALE_AMOUNT)) %>% select(SALE_AMOUNT) %>% pull())
median(hs %>% filter(!is.na(SALE_AMOUNT)) %>% select(SALE_AMOUNT) %>% pull())

gs <- hs %>% filter(!is.na(SALE_AMOUNT)) 
# gg %>% filter(!is.na(SALE_AMOUNT)) 
# nrow(hs) - hs %>% filter(is.na(SALE_AMOUNT)) %>% nrow()

hs_winsorized <- winsorize_data(list(gs) , "SALE_AMOUNT")

mean(gs$SALE_AMOUNT)
round(min(hs_winsorized[[1]]$SALE_AMOUNT))
round(max(hs_winsorized[[1]]$SALE_AMOUNT))

round(mean(hs_winsorized[[1]]$SALE_AMOUNT)) # 138,565
round(sd(hs_winsorized[[1]]$SALE_AMOUNT)) # 108,687


#======================================================================#
# Generating House Price Aggregate dataset ----
#======================================================================#

# Using house-level sale amount data, grouping by tendigit_fips and year. This data will be used to calculate the growth in house prices
# data before 1995 is missing for sale_amount. Hence, we start from 1995
hs_agg <- hs %>% 
              janitor::clean_names() %>%
              group_by(tendigit_fips, year) %>% 
              summarise(total_sale_amount = sum(sale_amount, na.rm = TRUE),
                        median_sale_amount = median(sale_amount, na.rm = TRUE),
                        mean_sale_amount = mean(sale_amount, na.rm = TRUE),
                        count = n() ) %>% 
              ungroup() %>% 
              arrange(tendigit_fips, year) %>% 
              filter(year >= 1995) %>% 
              mutate(tendigit_fips = as.numeric(tendigit_fips), year = as.numeric(year))
