#=========================================================================================================================================#
# Purpose : Does passing road tax levy for additional funding lead to more property tax revenue? 
# Name    : Saani Rawat
# Created : 08/26/2025
# Log     : 1. 08/26/2025: Created the file
#> 
#> This script analyzes the impact of passing a road tax levy for additional funding on property tax revenue and public works spending.
#=========================================================================================================================================#

# Setup 
library(tidyverse)
library(haven)
library(data.table)
library(fixest)
library(quantreg)

# locations
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# PRIMARY QUESTION: For townships, do I see changes in funding when "New money" comes in i.e. additional road tax levy passes?


#==========================================================================================#
# importing data
#==========================================================================================#


roads_a <- haven::read_dta("data/roads_and_census.dta") %>% 
  mutate(if_pass = if_else(votes_pct_for > 50, 1, 0),
         margin = (votes_pct_for - 50)) %>% 
  janitor::clean_names() %>%
  select(tendigit_fips, year, subdivisionname, subdivisiontype, county, votes_pct_for, margin, taxtype, purpose2, description, millagepercent, duration, votes_pct_for, if_pass) %>%
  filter(toupper(subdivisiontype) == "TOWNSHIP" & description %in% c("A", "-999")) %>%
  filter(year >= 2000)


# cpi
cpi_year <- read_csv("data/CPIAUCSL_NBD20100101.csv" , show_col_types = FALSE) %>%
  rename(cpi = CPIAUCSL_NBD20100101) %>%
  mutate(year = year(observation_date)) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
cpi_base_2010 <- cpi_year %>% filter(year == 2010) %>% pull(cpi) %>% unique()
cpi_year <- cpi_year %>% mutate(deflator_2010 = cpi_base_2010 / cpi)

# reports
twp_reports <- readxl::read_excel("data/spending reports/township_reports_all.xlsx") 
twp_real_2010 <- twp_reports %>%
  mutate(year = as.integer(year)) %>%
  left_join(cpi_year %>% select(year, deflator_2010), by = "year") %>%
  mutate(
    property_tax_real_2010 = property_tax * deflator_2010,
    public_works_real_2010 = public_works * deflator_2010
  ) %>% filter(year >= 2000)

# township name-fips mapping
twp_name_fips_map <- readxl::read_excel("data/ohio-only-all-geocodes-2016-edited.xlsx") %>%
  select(`TENDIGIT_FIPS`, `name (note if split between two counties)`, `county name`) %>%
  rename(
    fips = `TENDIGIT_FIPS`,
    name = `name (note if split between two counties)`,
    county = `county name`
  ) %>%
  filter(str_detect(name, regex("township", ignore_case = TRUE))) %>%
  mutate(name = str_trim(str_remove(name, "\\s*township$"))) %>%
  mutate(name = tolower(name)) %>%
  mutate(county = tolower(str_trim(county))) %>%
  rename(township = name) %>%
  mutate(township = case_when(fips == 3911711248 ~ "canaan",
                              fips == 3904118010 ~ "columbus city",
                              # fips == 3901725970 ~ "fairfield",
                              fips == 3915328448 ~ "franklin",
                              fips == 3916928504 ~ "franklin",
                              fips == 3915331664 ~ "green",
                              fips == 3915336651 ~ "hudson",
                              fips == 3909567752 ~ "roche de boeuf",
                              fips == 3901783150 ~ "west chester",
                              TRUE ~ township)) %>%
  distinct(fips, township, county, .keep_all = TRUE)


#==========================================================================================#
# Deflating to 2010 dollars
#==========================================================================================#

# separate township and county
twp_real_2010_sep <- twp_real_2010 %>%
  separate(name, into = c("township", "county"), sep = ",", remove = FALSE) %>%
  mutate(
    township = trimws(township), # remove leading/trailing spaces
    county   = trimws(county)
  ) %>%
  mutate(
    township = str_trim(str_remove(township, "\\s*township$")),
    county   = str_trim(str_remove(county, "\\s*county$"))
  ) %>%
  filter(property_tax != 0 | public_works != 0) %>%
  arrange(township, county, year)

# Adding tendigit_fips to govt spending/revenue data
twp_real_2010_sep2 <- twp_real_2010_sep %>%
  left_join(twp_name_fips_map, by = c("township", "county")) %>%
  relocate(fips, .before = township) %>%
  select(-name) %>%
  rename(fiscal_year = year)


#==========================================================================================#
# Extracting relevant spending/revenue info i.e. around the elections
#==========================================================================================#

# Set time window
L <- 1; W <- 3

events2 <- roads_a %>%
  filter(toupper(subdivisiontype) == "TOWNSHIP", description %in% c("A", "-999")) %>% # keep township additional/“A” elections only
  rename(fips = tendigit_fips, election_year = year) %>%
  arrange(fips, election_year) %>%
  group_by(fips) %>%
  mutate(prev_election_year = dplyr::lag(election_year),
         next_election_year = dplyr::lead(election_year)) %>%
  ungroup() %>%
  mutate(t_start       = election_year + L,
         next_t_start  = ifelse(is.na(next_election_year), NA_integer_,
                                next_election_year + L),
         # helpful flags
         has_next_in_window = !is.na(next_election_year) & (next_election_year - election_year) <= (W + L),
         has_prev_in_window = !is.na(prev_election_year) & (election_year - prev_election_year) <= (W + L))


# View(roads_a)
# View(events2)
# build stacked event-time rows and join finance (already deflated to 2010 $)
# Note: We only right-truncate the post window if there is a next election within the window. We don't left-truncate the pre window as we what has happened in the past (in terms of "monetary jumps", not election results per se) is not affected by  concurrent election (reasonable assumption) and we also will lose even more data.
stack_trunc <- events2 %>%
  tidyr::crossing(k = -W:W) %>%
  mutate(fiscal_year = election_year + k,
         is_pre  = k %in% (-W:-1),
         is_post_raw = fiscal_year >= (election_year + L) & fiscal_year <= (election_year + L + W)) %>%
  # RIGHT-TRUNCATE the post window at (next_t_start - 1)
  mutate(is_post = dplyr::case_when(
    is.na(next_t_start) ~ is_post_raw,
    TRUE ~ is_post_raw & fiscal_year <= (next_t_start - 1L)
  ),
  keep = is_pre | is_post) %>%
  # toss rows outside the truncated window
  filter(keep) %>%
  # bring in outcomes
  left_join(twp_real_2010_sep2, by = c("fips","fiscal_year"))


# event-level summaries
evsum <- stack_trunc %>%
  group_by(fips, election_year) %>%
  summarise(
    pass = first(if_pass),
    
    # counts of usable pre/post years for each outcome
    npre_tax  = sum(is_pre  & !is.na(property_tax_real_2010)),
    npost_tax = sum(is_post & !is.na(property_tax_real_2010)),
    npre_pw   = sum(is_pre  & !is.na(public_works_real_2010)),
    npost_pw  = sum(is_post & !is.na(public_works_real_2010)),
    
    # pre/post means (will be NA if no obs in that side)
    pre_tax   = mean(property_tax_real_2010[is_pre],  na.rm = TRUE),
    post_tax  = mean(property_tax_real_2010[is_post], na.rm = TRUE),
    pre_pw    = mean(public_works_real_2010[is_pre],  na.rm = TRUE),
    post_pw   = mean(public_works_real_2010[is_post], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # jumps only when both sides exist
    d_tax = ifelse(npre_tax  > 0 & npost_tax  > 0, post_tax - pre_tax, NA_real_),
    d_pw  = ifelse(npre_pw   > 0 & npost_pw   > 0, post_pw  - pre_pw,  NA_real_)
  )

ev_tax <- evsum %>% filter(!is.na(d_tax))
ev_pw  <- evsum %>% filter(!is.na(d_pw))

# Property Tax Revenue # 
ev_tax_long <- ev_tax %>% select(-c("pre_pw", "post_pw","d_tax","d_pw")) %>%
  pivot_longer(
    cols = c(pre_tax, post_tax),
    names_to = "period",
    values_to = "tax_amount"
  ) %>%
  mutate(post = ifelse(period == "post_tax", 1, 0),
         treat = pass*post,
         ln_tax_amount = log(tax_amount))
# 532 rows

ev_tax_long %>% 
  group_by(pass, post) %>%
  summarise(count = n(),
            mean_tax = mean(tax_amount, na.rm = TRUE),
            median_tax = median(tax_amount, na.rm = TRUE),
            sd_tax = sd(tax_amount, na.rm = TRUE),
            .groups = "drop") 

# ev_tax_long %>%
#   group_by(pass, post) %>%
#   summarise(count = n(),
#             ln_mean_tax = mean(ln_tax_amount, na.rm = TRUE),
#             ln_median_tax = median(ln_tax_amount, na.rm = TRUE),
#             ln_sd_tax = sd(ln_tax_amount, na.rm = TRUE),
#             .groups = "drop")  

# Public Works Spending # 
ev_pw_long <- ev_tax %>% select(-c("pre_tax", "post_tax","d_tax","d_pw")) %>%
  pivot_longer(
    cols = c(pre_pw, post_pw),
    names_to = "period",
    values_to = "pw_amount"
  ) %>%
  mutate(post = ifelse(period == "post_pw", 1, 0),
         treat = pass*post,
         ln_pw_amount = log(pw_amount)
         ) 
# View(ev_pw_long)

# comparison of means: pre-post for pass-fail
ev_pw_long %>%
  group_by(pass, post) %>%
  summarise(count = n(),
            mean_pw = mean(pw_amount, na.rm = TRUE),
            median_pw = median(pw_amount, na.rm = TRUE),
            sd_pw = sd(pw_amount, na.rm = TRUE),
            .groups = "drop")
# ev_pw_long %>%
#   group_by(pass, post) %>%
#   summarise(count = n(),
#             ln_mean_pw = mean(ln_pw_amount, na.rm = TRUE),
#             ln_median_pw = median(ln_pw_amount, na.rm = TRUE),
#             ln_sd_pw = sd(ln_pw_amount, na.rm = TRUE),
#             .groups = "drop")            


#==========================================================================================#
# Analysis 
#==========================================================================================#

lz <- function(x) log1p(pmax(x, 0))

# A) Event-level DID on pre/post aggregates
evsum2 <- evsum %>%
  mutate(
    dln_tax = ifelse(npre_tax > 0 & npost_tax > 0, lz(post_tax) - lz(pre_tax), NA_real_),
    dln_pw  = ifelse(npre_pw > 0 & npost_pw > 0, lz(post_pw ) - lz(pre_pw ), NA_real_),
    ln_pre_tax = ifelse(npre_tax > 0, lz(pre_tax), NA_real_),
    ln_pre_pw  = ifelse(npre_pw > 0,  lz(pre_pw ), NA_real_)
  )
# View(evsum2)
ev_tax_did <- evsum2 %>% filter(!is.na(dln_tax))
ev_pw_did  <- evsum2 %>% filter(!is.na(dln_pw))
# View(ev_pw_did)
# View(ev_tax_did)

# Difference-in-means with a baseline control; cluster by fips
m_tax = feols(dln_tax ~ pass + ln_pre_tax, data = ev_tax_did, vcov = ~fips)
m_pw  = feols(dln_pw  ~ pass + ln_pre_pw,  data = ev_pw_did,  vcov = ~fips)
etable(m_tax, m_pw, se.below = TRUE)

summary(m_tax)
summary(m_pw)
# View(ev_tax_did)

ev_tax_did %>%
  mutate(d_tax = post_tax - pre_tax) %>%
  group_by(pass) %>%
  summarise(mean_d_tax = mean(d_tax, na.rm = TRUE),
            median_d_tax = median(d_tax, na.rm = TRUE),
            sd_d_tax = sd(d_tax, na.rm = TRUE),
            count = n(),
            .groups = "drop")

ev_tax_did %>%
  group_by(pass) %>%
  summarise(mean_dln_tax = mean(dln_tax, na.rm = TRUE),
            median_dln_tax = median(dln_tax, na.rm = TRUE),
            sd_dln_tax = sd(dln_tax, na.rm = TRUE),
            count = n(),
            .groups = "drop")

ev_pw_did %>%
  group_by(pass) %>%
  summarise(mean_dln_pw = mean(dln_pw, na.rm = TRUE),
            median_dln_pw = median(dln_pw, na.rm = TRUE),
            sd_dln_pw = sd(dln_pw, na.rm = TRUE),
            count = n(),
            .groups = "drop")


# Median regression on deltas (robust to heavy tails).
rq_tax <- rq(dln_tax ~ pass + ln_pre_tax, data = ev_tax_did, tau = 0.5) # quantile regression on median quantile tau = 0.5
rq_pw  <- rq(dln_pw  ~ pass + ln_pre_pw,  data = ev_pw_did,  tau = 0.5)
summary(rq_tax)
summary(rq_pw)
# Clustered bootstrap for s.e. Inference: using a simple township-level bootstrap for CIs.
set.seed(123)
cl <- unique(ev_tax_did$fips)
B  <- 1000
boot_beta <- replicate(B, {
  samp <- sample(cl, length(cl), replace = TRUE)
  idx  <- ev_tax_did$fips %in% samp
  coef(rq(dln_tax ~ pass + ln_pre_tax, data = ev_tax_did[idx,], tau = 0.5))["pass"]
})
quantile(boot_beta, c(.025,.5,.975))

colnames(roads_a)
mean(as.numeric(roads_a[["millagepercent"]]), na.rm = TRUE)

# Key Findings:
# Using collapsed difference-in-differences at the election level with right-truncated windows, we find that passing an additional road-funding levy raises township property-tax revenues by about 9–10% and public-works spending by about 9% relative to failed elections. Median regressions yield similar effects (≈14% and 9% respectively), indicating that results are not driven by outliers. These estimates establish a strong and economically meaningful first stage: the levy pass generates higher budgets.