###################################################################################################################################################
# created: 03/05/2024
# by: Saani Rawat
# purpose: create dataset for Diff-in-diff analysis
# Log:
# 1. 03/05/2024: created. Finished writing up till mgd_gr
###################################################################################################################################################



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
  filter(between(year, 1995, 2021)) %>% janitor::clean_names()

# convert tendigit_fips to numeric
hs_g$tendigit_fips <- as.numeric(hs_g$tendigit_fips)
hs_g$year <- as.numeric(hs_g$year)

# convert the data into panel format (take housing and roads information)

hs 

length(unique(hs_g$TENDIGIT_FIPS)) *length(unique(hs_g$year))
nrow(hs_g)


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
         time = if_else(ord >= 0, 1, 0)) %>%
  select(tendigit_fips, vote_id, year, dataset, ord, votes_pct_against, time, treated, everything()) %>%
  arrange(tendigit_fips, vote_id, ord) %>%
  select(-starts_with("yr_t_"))


