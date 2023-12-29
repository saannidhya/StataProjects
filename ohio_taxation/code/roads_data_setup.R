#==========================================================================================================#
# Purpose : Data setup for running variable. Creates running variable for "R","A" types and also drops obs 
#           that violate SUTVA.
# Name    : Saani Rawat
# Created : 10/12/2023
# Log     : 1. updated the code 
#==========================================================================================================#

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
roads_vars <- c("tax_type", "purpose2", "description", "millage_percent", "duration", "votes_for", "votes_against")

# importing roads  dataset
rd <- haven::read_dta(paste0(data,"/roads_levies2_census_9118.dta"))

# cleaning roads dataset: creating vote share variables, keeping the obs per fips per year closest to cutoff, adding fail flag
# note: rds contains both additional and renewal road tax levies
rds <- rd %>%
  select(all_of(rd_var_list)) %>% 
  janitor::clean_names() %>%
  mutate(votes_pct_for = (votes_for / (votes_for + votes_against))*100,
         votes_pct_for_cntr = abs(votes_pct_for - cutoff)) %>%
  group_by(tendigit_fips, year) %>% 
  arrange(tendigit_fips, year, votes_pct_for_cntr) %>% 
  mutate(count = row_number()) %>% 
  filter(count == 1) %>%
  arrange(tendigit_fips, year) %>% 
  mutate(fail_flg = if_else(votes_pct_for < cutoff, 1, 0))
# rds_additional_vts: filtering out additional road tax levies, creating a flag to identify additional levies that passed. These will be contaminating our Treatment Effect.
rds_additional_vts <- rds %>%
  select(all_of(c("tendigit_fips","year", roads_vars, "votes_pct_for", "fail_flg"))) %>%
  filter(description == "A") %>% 
  mutate(pass_flg = if_else(votes_pct_for >= cutoff, 1, 0))


#======================================================================#
# Eliminating contaminated obseravations for dfs_agg ----
#======================================================================#

# extracting year list
df_names <- gsub(".*?(t_minus_\\d+|t_plus_\\d+).*", "\\1", names(dfs_agg))

# adding column that contains dataset names  
dfs_agg_n <- purrr::map2(dfs_agg, df_names, ~ .x %>% mutate(dataset = .y))

# combing t-3, t-2 , ..., t+10 datasets into one dataset called dfs_agg_all
dfs_agg_all <- dfs_agg_n %>% bind_rows() %>% arrange(tendigit_fips, vote_year, year)

# Performing a left join on dfs_agg_all with "additional" road tax levy dataset called rds_additional_vts
# numeric_df column is s.t. t_minus_3 = 1, t_minus_2 = 2, and so on..
dfs_agg_all_add <- dfs_agg_all %>% left_join(rds_additional_vts, by = c("tendigit_fips", "year")) %>%
                      ungroup() %>%
                      mutate(numeric_df = as.numeric(factor(dataset, levels = c("t_minus_3", "t_minus_2", "t_minus_1", "t_plus_0", 
                                                                                "t_plus_1", "t_plus_2", "t_plus_3", "t_plus_4", 
                                                                                "t_plus_5", "t_plus_6", "t_plus_7", "t_plus_8", 
                                                                                "t_plus_9", "t_plus_10"))))

# Create a grouping variable based on breaks (break happens whenever "gap" of more than 1 appears in the consecutive obs)
# We do this to identify whether the next obs in dataset belongs to same tax levy or different tax levy (count of "group" increases each time a new tax levy is identified)
dfs_agg_all_add$`break` <- c(TRUE, diff(dfs_agg_all_add$numeric_df) != 1)
dfs_agg_all_add$group <- cumsum(dfs_agg_all_add$`break`)

# group by each separate election
# replacing NAs with zeros in pass flag. Non NA values remain the same
# identifying the first pass flag for the group and then creating drop flag based on the first pass flag
# Once a first pass flag (for "A" levies) is observed, drop flag == 1 for all observations after that one (in the group)
dff <- dfs_agg_all_add %>%
          group_by(group) %>%
          mutate(pass_flg = if_else(is.na(pass_flg), 0, pass_flg)) %>%
          mutate(first_pass_flg = if_else((pass_flg == 1) & (row_number() == min(row_number()[pass_flg == 1])), 1, 0)) %>%
          mutate(drop_flg = if_else( (cumsum(pass_flg) >= 1) & (first_pass_flg != 1) , 1, 0)) %>%
          filter(!(drop_flg == 1)) # remove the drop_flg == 1 observations
# note: I get warning whenever there is a group s.t. row_number()[pass_flg == 1] is missing i.e. has no values


# Generating dfs_agg_pure from dfs_agg after eliminating contaminated obseravations ----
# create separate datasets for t-3 up to t+10
dfs_agg_pure <- purrr::map(df_names, ~ dff %>%  filter(dataset == .x)  %>% ungroup() %>%
                                               select(-c("tax_type","purpose2","description", "millage_percent","duration",
                                                         "votes_for","votes_against","votes_pct_for.y", "fail_flg","pass_flg",
                                                         "numeric_df","break","group", "first_pass_flg","drop_flg")) %>% 
                                               rename(votes_pct_for = votes_pct_for.x))
names(dfs_agg_pure) <- paste0("housing_roads_census_", df_names, "_matches")
# dfs_agg_pure


# Adding covariates to dfs_agg_pure  ----
dfs_agg_pure_covs <- purrr::map(.x = dfs_agg_pure, ~ .x %>% 
                                     dplyr::left_join(y = census %>% rename(vote_year = year), by = c("tendigit_fips","vote_year")) %>%
                                     ungroup()
)
dfs_agg_pure_covs


#======================================================================#
# Eliminating contaminated observations for dfs_agg_per ----
#======================================================================#

