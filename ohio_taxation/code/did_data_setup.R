###################################################################################################################################################
# created: 03/05/2024
# by: Saani Rawat
# purpose: create dataset for Diff-in-diff analysis
# Log:
# 1. 03/05/2024: created. Finished writing up till mgd_gr
###################################################################################################################################################

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")
# specify the shared location
shared <- "//cobshares.uccob.uc.edu/economics$/Julia/roads"
cutoff <- 50
# importing libraries
library(haven)
library(tidyverse)
library(janitor)
library(purrr)
library(MatchIt)

#======================================================================#
# Housing dataset ----
#======================================================================#

# housing dataset
hs <- haven::read_dta(paste0(shared,"/housesales_9521_slim.dta"))

# group by TENDIGIT_FIPS and year and compute the median sale price and number of observations in each group

hs_g <- hs %>% 
  group_by(TENDIGIT_FIPS, year) %>% 
  summarise(median_sale_price = median(SALE_AMOUNT, na.rm = TRUE),
            num_houses = n()) %>% 
  ungroup() %>% 
  filter(between(year, "1995", "2021")) %>% janitor::clean_names()

# convert tendigit_fips to numeric
hs_g$tendigit_fips <- as.numeric(hs_g$tendigit_fips)
hs_g$year <- as.numeric(hs_g$year)

# convert the data into panel format (take housing and roads information)
# hs 
# length(unique(hs_g$TENDIGIT_FIPS)) *length(unique(hs_g$year))
# nrow(hs_g)


#======================================================================#
# Introducing roads voting and census data  ----
#======================================================================#

# roads dataset
rd <- haven::read_dta(paste0(data,"/roads_levies2_census_9118.dta"))
rd_fips <- sort(unique(rd$TENDIGIT_FIPS))# length(rd_fips)

roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != "1000") %>%
  janitor::clean_names() %>%
  mutate(votes_pct_against = 100 - votes_pct_for) %>%
  mutate(treated = if_else(votes_pct_against > cutoff, 1, 0)) %>% 
  mutate(yr_t_minus_5 = year - 5,
         yr_t_minus_4 = year - 4,
         yr_t_minus_3 = year - 3, 
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
         yr_t_plus_10 = year + 10) %>%
  select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
  arrange(tendigit_fips, year)

# add a column that increases per observation i.e. 1, 2, 3... 
roads_and_census$vote_id <- 1:nrow(roads_and_census)

# past and future years list
yrs <- c(paste0("yr_t_minus_",as.character(1:5)), paste0("yr_t_plus_",as.character(0:10)))

# creating copies of (slim) raw housing dataset such that we have t_minus and t_plus variables in the dataset (to merge on)
hss_g <- purrr::map(yrs, ~ hs_g %>% 
                    mutate({{.x}} := as.numeric(year), dataset = .x) %>%
                    select(-c(year)) 
)
names(hss_g) <- yrs

# merge the housing and roads data
mgd_g <- purrr::map2(hss_g, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y))
})

mgd_gr <- mgd_g %>% bind_rows() %>%
  mutate(ord = str_extract(dataset, pattern = "minus_[0-9]+|plus_[0-9]+"),
         ord = as.numeric(ifelse(str_detect(ord, "minus"), 
                                 paste0("-", str_extract(ord, "[0-9]+")), 
                                 str_extract(ord, "[0-9]+"))),
         treated = if_else(votes_pct_against > cutoff, 1, 0),
         time = if_else(ord > 0, 1, 0),
         did = time*treated) %>%
  select(tendigit_fips, vote_id, year, dataset, ord, votes_pct_against, time, treated, did, everything()) %>%
  arrange(tendigit_fips, vote_id, ord) %>%
  select(-starts_with("yr_t_"))


#======================================================================#
# separating out each post-treatment years ----
#======================================================================#

# using t-1 as pre year
did_dfs <- purrr::map(1:10, ~ mgd_gr %>% filter(ord %in% c(-1, .x) ))
names(did_dfs) <- paste0("t_plus_", 1:10)



#======================================================================#
# Aggregating by ord and group type ----
#======================================================================#


# group by ord and time and compute mean of median sale price
did_dfs_mean <- mgd_gr %>% 
                            group_by(ord, treated) %>% 
                            summarise(mean_median_sale_price = mean(median_sale_price, na.rm = TRUE)) %>% 
                            ungroup() %>% mutate(ln_mean_median_sale_price = log(mean_median_sale_price)) 
# did_dfs_mean$base_price <- 

did_dfs_mean <- did_dfs_mean %>% 
                  mutate(base_price = if_else(treated == 0, 
                                              filter(did_dfs_mean, ord == -5 & treated == 0) %>% select(mean_median_sale_price) %>% pull ,
                                              filter(did_dfs_mean, ord == -5 & treated == 1) %>% select(mean_median_sale_price) %>% pull),
                         diff_price = mean_median_sale_price - base_price)

# plot line plot with ord on x axis, mean_median_sale_price on y axis and different lines for treated and untreated
ggplot(did_dfs_mean, aes(x = ord, y = diff_price, color = factor(treated))) + 
  geom_line() + 
  labs(title = "Mean Median Sale Price by Ord and Treated", 
       x = "Ord", 
       y = "Mean Median Sale Price") + 
  theme_minimal() + 
  theme(legend.position = "bottom") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "black")  +
  scale_x_continuous(breaks = c(-5:10))


#======================================================================#
# Running global DID regressions ----
#======================================================================#


dta <- did_dfs$t_plus_4 %>% filter(!is.na(medfamy)) %>% select(tendigit_fips, vote_id, year, ord, 
                                                               votes_pct_against, time, treated, did, median_sale_price, medfamy, pop)

# running a global DID regression
did_regx <- map(did_dfs, ~lm(median_sale_price ~ treated + time + did + medfamy, data = .x))
map(did_regx, ~summary(.x) )

# running global regression discontinuity
dfs_agg_covs_rd <- map(dfs_agg_covs, ~ .x %>% 
                        mutate(votes_pct_against_cntr = votes_pct_against - cutoff,
                               treated = if_else(votes_pct_against_cntr > 0, 1, 0),
                               interaction_term = votes_pct_against_cntr*treated))

rd_regx <- map(dfs_agg_covs_rd, ~ lm(median_sale_amount ~ votes_pct_against_cntr + treated + interaction_term + medfamy, 
                                     data = .x))
map(rd_regx, ~summary(.x))
rd_regx$housing_roads_census_t_plus_10_matches$coefficients[["treated"]]

#======================================================================#
# Propensity score matching & Local Diff-in-Diff ----
#======================================================================#

local_did <- function(data, y_covs = c("medfamy","pop"), treat_covs = covs_list){
  
  data <- data %>% drop_na()
  
  # find correlation between treatment variable and other covariates
  v <- cor(data %>% select("treated", treat_covs), use = "complete.obs")
  
  # get the top 10 variables that are correlated with treated variable
  top_10_covs <- names(sort(v[,"treated"], decreasing = TRUE)[2:11])
  
  # create a propensity score model based on the top 10 covariates
  propensity_model <- glm(as.formula(paste("treated", "~", paste(top_10_covs, collapse = " + "))), family = binomial(), data = data)
  data$propensity_score <- predict(propensity_model, type = "response")
  
  # Matching
  match_it <- matchit(treated ~ propensity_score, data = data, method = "nearest")
  matched_data <- match.data(match_it)
  
  # local DID
  frmla <- as.formula(paste("median_sale_price ~ treated + time + did +", paste(y_covs, collapse = " + ")))
  return(lm(frmla, data = matched_data))
}

# running for all years
did_local_models <- map(did_dfs, ~ local_did(.x))
map(did_local_models, ~summary(.x))

