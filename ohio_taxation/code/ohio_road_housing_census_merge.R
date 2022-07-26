######################################################################################################################
# created: 07/09/2022
# by: Saani Rawat
# purpose: use STATA datasets to check if merge performed by ohio_road_housing_census_merge.do can be replicated
# Log:
# 1. 07/09/2022: used to download call report txt files from 2001 to 2021
######################################################################################################################


#####################
#checking merge in R
#####################

# setting working directory
setwd("//cobshares.uccob.uc.edu/economics$/Julia/roads")

# roads dataset
rd <- haven::read_dta("roads_levies2_census_9118.dta")
rd_fips <- sort(unique(rd$TENDIGIT_FIPS))
length(rd_fips)

# housing dataset
hs <- haven::read_dta("housesales_9521_slim.dta")
hs_fips <- sort(as.numeric(unique(hs$TENDIGIT_FIPS)))
length(hs_fips)


# merge performed in STATA
md <- haven::read_dta("housing_roads_census_t_plus_1.dta", col_select = c("TENDIGIT_FIPS","yr_t_plus_1","year","SALE_AMOUNT","_merge"))

# filtering roads (same as STATA file)
rds <- rd %>%
    select(c("year", "pop", "TENDIGIT_FIPS", "TENDIGIT_FIPS_year", "inctaxrate", "tax_type",
             "purpose2", "description", "millage_percent", "duration", "votes_for", "votes_against")) %>%
    mutate(year_t_plus_1 = year + 1, rd_flag = 1,
           votes_pct_for = (votes_for/(votes_for+votes_against))*100,
           votes_pct_for_cntr = abs(votes_pct_for - 50)) %>%
    relocate(TENDIGIT_FIPS, year, year_t_plus_1) %>%
    arrange(TENDIGIT_FIPS, year) %>%
    group_by(TENDIGIT_FIPS, year) %>% 
    mutate(dup = row_number()-1) %>%
    filter(dup == 0)

# cleaning housing (same as STATA file)
hss <- hs %>%
        select(c("TENDIGIT_FIPS", "year","SALE_AMOUNT")) %>%
        rename(year_t_plus_1 = year) %>%
        mutate(TENDIGIT_FIPS = as.numeric(TENDIGIT_FIPS), year_t_plus_1 = as.numeric(year_t_plus_1), hs_flag = 1)

# outer join of housing and roads datasets
mrg <- hss %>%
        full_join(rds, by = c("TENDIGIT_FIPS","year_t_plus_1"))


# number of _merge == 3
nrow(mrg %>% filter(hs_flag == 1 & rd_flag == 1)) # 373,077

# number of _merge == 1
nrow(mrg %>% filter(hs_flag == 1 & is.na(rd_flag))) # 6,772,762

# number of _merge == 2
nrow(mrg %>% filter(is.na(hs_flag) & rd_flag == 1)) # 119
