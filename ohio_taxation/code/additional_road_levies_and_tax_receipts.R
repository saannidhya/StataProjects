#==========================================================================================================#
# Purpose : Does passing road tax levy for additional funding lead to more property tax revenue? 
# Name    : Saani Rawat
# Created : 05/08/2025
# Log     : 1. 05/08/2025: Created the file
#> 
#> This script analyzes the impact of passing a road tax levy for additional funding on property tax revenue and public works spending.
#==========================================================================================================#


library(tidyverse)
library(haven)
library(data.table)
library(fixest)

#> Need to
#> 1. Deflate the property tax revenue to 2010 U.S dollars
#> 2. Merge the property tax revenue data with the road tax levy data
#> 3. Make sure the timing of the road tax levy is correct i.e. the road tax levy is passed before the property tax revenue is collected
#> 4. We want to see what happens to revenue once the road tax levy for additional money is passed, say after 5 years: take the avg
#> 

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")
spend_reports_loc <- paste0(data,"/spending reports/")

#=========================================================================================#
#>                  Expenses 
#=========================================================================================#

# expense reports import
report_loc <- paste0(data,"/roads/Road Quality/outputs/tables")

cpi_df <- readr::read_csv(paste0(data,"/CPIAUCSL_NBD20100101.csv")) %>% rename(cpi = CPIAUCSL_NBD20100101) %>%
  mutate(cpi_deflator = cpi/100, 
         year = lubridate::year(observation_date)) %>% select(-observation_date) 
  # mutate(year = lubridate::year(observation_date) - 1, cpi_deflator = cpi/100) 

townships_exp <- readxl::read_excel(paste0(report_loc, "/township_reports_all.xlsx")) %>% mutate(type = "township") %>% 
  mutate(
    # everything before the comma, then drop the word “township”
    township = name %>% 
      str_remove(",.*$") %>%              # remove “, <county> county”
      str_remove("\\s+township$") %>%     # drop trailing “ township”
      str_trim(),
    
    # text that sits between the comma and the word “county”
    county = name %>% 
      str_extract("(?<=, )[[:alpha:] ]+(?= county)") %>% 
      str_trim()
  ) %>%
  arrange(name, year) %>% filter(year >= 2000) %>% 
  left_join(select(cpi_df, c(year, cpi_deflator)), by = c("year")) %>%
  mutate(property_tax_d = property_tax/cpi_deflator,
         public_works_d = public_works/cpi_deflator)

villages_exp <- readxl::read_excel(paste0(report_loc, "/village_reports_all.xlsx")) %>% mutate(type = "village")


# expenses <- bind_rows(townships_exp, villages_exp)

sort(unique(townships_exp$property_tax))

#=========================================================================================#
#>                  Tax Levy : Election results for Additional money
#=========================================================================================#


r_and_a <- haven::read_dta("data/roads_and_census.dta") 
readr::write_csv(r_and_a, "data/roads_and_census.csv")

roads_a <- haven::read_dta("data/roads_and_census.dta") %>% 
  filter(description == "A") %>%
  mutate(if_pass = if_else(votes_pct_for > 50, 1, 0),
         margin = (votes_pct_for - 50)/100,
         votes_pct_for = votes_pct_for/ 100,
         votes_pct_against = votes_pct_against/100,
         hold_election = 1,
         margin_ds = margin*if_pass) %>% janitor::clean_names() %>%
  select(tendigit_fips, year, subdivisionname, subdivisiontype, county, votes_pct_for, margin, margin_ds, taxtype, purpose2, description, millagepercent, duration, votesfor, votesagainst, votes_pct_for, votes_pct_against, hold_election, if_pass, margin, margin_ds,
         # census variables
         # pop, medfamy, childpov, poverty, pctwithkids, pctsinparhhld, pctnokids, pctlesshs, pcthsgrad, pctsomecoll, pctbachelors, pctgraddeg, unemprate, pctrent, pctown, pctlt5, pct5to17, pct18to64, pct65pls, pctwhite, pctblack, pctamerind, pctapi, pctotherrace, pctmin, raceherfindahl, pcthisp, pctmarried, pctnevermarr, pctseparated, pctdivorced, lforcepartrate, incherfindahl
  ) %>%
  mutate(subdivisiontype = tolower(subdivisiontype),
         subdivisionname = tolower(subdivisionname),
         county = tolower(county)) %>%
  arrange(tendigit_fips, year) 

# 68% are townships, 19% are villages, 13% are cities
roads_a %>% mutate(subdivisiontype = tolower(subdivisiontype)) %>%
  group_by(subdivisiontype) %>%
  summarise(n = n(), prop = n/nrow(.)) 

# how many match with the township data. 43% match rate
townships_exp %>% distinct(township, type, county) %>%
  rename(subdivisionname = township, subdivisiontype = type) %>%
  inner_join(roads_a %>% distinct(subdivisionname, subdivisiontype, county), by = c("subdivisionname", "subdivisiontype", "county")) 



#=========================================================================================#
#>                  Tax Levy Election Results --> Tax Revenue Received? An analysis
#=========================================================================================#

roads_clean <- roads_a %>% 
  mutate(across(c(subdivisionname, subdivisiontype, county),
                ~ str_trim(str_to_lower(.x)))) %>% 
  select(year_elec = year,
         township      = subdivisionname,
         type          = subdivisiontype,
         county,
         if_pass,
         taxtype,
         purpose2,
         millagepercent
         ,everything()
         ) %>% filter(year_elec >= 2000 & type == "township")



towns_clean <- townships_exp %>% 
  mutate(across(c(township, type, county),
                ~ str_trim(str_to_lower(.x)))) %>% 
  select(year,
         township,
         type,
         county,
         property_tax,
         property_tax_d
         # ,everything()
  ) %>% mutate(property_tax_d = round(property_tax_d))

exp <- towns_clean %>% 
  filter((township %in% unique(roads_clean$township)) & (county %in% unique(roads_clean$county))) 

levy <- roads_clean %>% 
  filter(str_detect(tolower(taxtype),  "property"),
         str_detect(tolower(purpose2), "roads")) %>% 
  mutate(start_post  = year_elec + 1,
         end_post    = year_elec + 5,
         elect_id    = row_number())   

# ALL receipts in [-5, +5]
receipts_all <- levy %>%          # passed + failed road‑levy proposals
  left_join(towns_clean,          # bring in every receipt we have
            by = c("township", "county", "type")) %>% 
  # keep only the 5y windows and drop the election year itself
  filter(year >= year_elec - 5,
         year <= year_elec + 5,
         year != year_elec) %>% 
  mutate(rel_year = year - year_elec,
         period   = if_else(rel_year < 0, "pre", "post"))

# summarise separately for PRE and POST
levy_stats_long <- receipts_all %>% 
  group_by(elect_id, township, county, type, if_pass, millagepercent, duration, year_elec, period) %>% 
  summarise(avg_tax = mean(log(property_tax_d + 0.001), na.rm = TRUE),
            n_obs   = sum(!is.na(property_tax)),
            .groups = "drop") %>% 
  mutate(avg_tax = na_if(avg_tax, NaN)) %>%  # turn all‑NA means (NaN) into NA
  mutate(post = as.integer(period == "post")) 

levy_stats <- levy_stats_long %>% 
  pivot_wider(names_from  = period,
              values_from = c(avg_tax, n_obs),
              names_glue  = "{period}_{.value}") %>%
  filter(!is.na(post_avg_tax) & !is.na(pre_avg_tax)) %>%
  mutate(change_abs  = post_avg_tax - pre_avg_tax,
         change_pct  = (post_avg_tax - pre_avg_tax) / pre_avg_tax) %>%
  filter(!(elect_id %in% c(1)))

did_elections <- feols(avg_tax ~ if_pass | county + period,  # FE at county; or township if you prefer
                       cluster = ~ county,
                       data = levy_stats_long)
# > exp(0.263789) - 1
# [1] 0.3018535
# Passing an additional road tax levy increases property tax revenue by 30% in the post period! Not a DiD though?

levy_stats_long_filtered %>% 
  filter(!(elect_id %in% c(1))) %>%  
  group_by(if_pass, post) %>%
  summarize(avg_tax = mean(avg_tax, na.rm = TRUE))

levy_stats_long_filtered <- levy_stats_long %>%
  filter(!(elect_id %in% c(1))) %>%  
  group_by(elect_id) %>%
  filter(n_distinct(period) == 2) %>%
  ungroup()


did_mod <- feols(avg_tax ~ post * if_pass | county ,
                 cluster = ~ county,
                 data = levy_stats_long)
