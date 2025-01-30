#==========================================================================================================#
# Purpose : Identify which covariates are balanced between treatment and control groups
# Name    : Saani Rawat
# Created : 07/27/2022
# Log     : 07/29/2022 : finished loop for covariate balance test for all datasets
#           11/23/2023 : Removed work on post-outcome covariates datasets (datasets that contain both outcome 
#                        and covariates). Now, doing balancing only on covariates and running variables.
#           06/24/2024 : Updated the code to make it compatible with other files    
#           07/25/2024 : Added t-3, t-2, t+1 housing datasets to the table for housing outcome
#           08/15/2024 : Added t-3, t-2, t+1 housing datasets to the table for employment and wages outcomes
#==========================================================================================================#

library(kableExtra)
library(cobalt)

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))
source(paste0(code,"/employment_data_setup.R"))
# running reg discontinuity with covs for bandwidth and selected covariates info
source(paste0(code,"/road_spending_reg_discontinuity_agg_w_covariates.R"))

# list of covariates
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")


#============================================================================================================#
#     Covariate discontinuity test (before introducing outcome variable) ----
#============================================================================================================#
# |- RD using covariates ####

regs_covs <- purrr::map(covs_list, .f = function(var){
                        rdrobust::rdrobust(y = roads_and_census[[var]], 
                                           x = roads_and_census$votes_pct_against, 
                                           c = cutoff, 
                                           all = TRUE)
                      })

purrr::walk(covs_list, .f = function(var) {
  print(paste0("Outcome variable is ",var))
  summary(regs_covs[[var]]) 
})

# only pcthsgrad showed slight evidence of discontinuity. Every other variable is continuous around cutoff.



#============================================================================================================#
#                        Balancing on the covariates in design phase (no outcome vars introduced) ----
#============================================================================================================#

#=======================================#
# Love plot
#=======================================#
bal.plot(roads_and_census, var.name = "votes_pct_against", covs = "pop", group = "treated", type = "histogram")
# roads_and_census$

#=======================================#
# Using matching techniques
#=======================================#
  
m.out <- MatchIt::matchit(formula = treated ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctnokids + pctlesshs + 
                              pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctrent + pctown + 
                              pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + 
                              pctmin + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl,
                            data = roads_and_census_2, method = 'nearest', estimand = "ATT")

summary(m.out)
bal_tab = bal.tab(m.out, un = T)
love.plot(bal_tab, thresholds = 0.1)

nrow(m.out$X)

#=======================================#
# Using weighting techniques
#=======================================#

# plotting unadjusted mean differences between treated and untreated 
balance_stats <- bal.tab(treated ~ ., data = roads_and_census %>% select(treated,all_of(covs_list)), un = TRUE, s.d.denom = "treated")
love.plot(balance_stats, thresholds = 0.2)

covs_my_list <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctlesshs", "pctsinparhhld", "pctlt5")
balance_stats <- bal.tab(treated ~ ., data = roads_and_census %>% select(treated,all_of(covs_my_list)), un = TRUE, s.d.denom = "treated")
love.plot(balance_stats, thresholds = 0.2)


# using weightit and propensity score to balance covariates
w.out <- WeightIt::weightit(formula = treated ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctnokids + pctlesshs + 
                   pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctrent + pctown + 
                   pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + 
                   pctmin + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl,
                 data = roads_and_census, method = 'ps', estimand = "ATE")
bal_tab = bal.tab(w.out, un = T)
love.plot(bal_tab, thresholds = 0.1)
summary(w.out)

weighted_roads_and_census <- roads_and_census %>% mutate(weights = w.out$weights)

# Need to take a subset of covariates and then do weighting based on those variables.

# data("lalonde", package = "cobalt") #If not yet loaded
# covs <- subset(lalonde, select = -c(treat, re78, nodegree, married))
# lalonde$p.score <- glm(treat ~ age + educ + race + re74 + re75,
#                        data = lalonde,
#                        family = "binomial")$fitted.values
# bal.tab(covs, treat = lalonde$treat)


#=======================================#
# doing t-test on entire sample
#=======================================#

roads_and_census_means <-   roads_and_census %>%
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>%
  group_by(treated) %>%
  summarize(across(-c(tendigit_fips, year, taxtype, purpose2, description, 
                      votes_pct_for, votes_pct_for_cntr, votesagainst, votesfor), 
                   ~mean(.x, na.rm=TRUE) ))


# t_tests <- purrr::map(.x = col_list[col_list != c("median_sale_amount", "median_ln_sale_amount", "inctaxrate")], 
#                       .f = ~ t_test(df = roads_and_census, 
#                                     var = .x)) %>% 
#             bind_rows() 

# column names renaming list for graphical purposes
new_names <- c(pop = "Population", poverty = "Poverty Rate", pctmin = "Minority Rate", medfamy = "Median Family Income", 
               pct18to64 = "% 18-64 Age Group", pctsinparhhld = "% Households with Children under 18", pctlt5 = "% of population under 5",
               pctrent = "% Renters", pctlesshs = "% Less than High School Education", pctwhite = "% White", pctwithkids = "% With Kids",
               pctblack = "% Black",  pctapi = "% Asian & Pacific Islander", incherfindahl = "Income Heterogeneity Index",
               pct5to17 = "% 5-17 Age Group", raceherfindahl = "Race Heterogeneity Index",
               pcthsgrad = "% did not graduate High-School", pctnokids = "% with No Kids", 
               pctseparated = "% Separated", childpov = "% Children below poverty rate", unemprate = "Unemployment Rate",
               pctown = "% Homeowners", pctamerind = "% American Indian",
               pctotherrace = "% Other Races", pctmarried = "% Married", treated = "treated")

# rows should not contain NA values, keep within effective bandwidth
chr_lst <- unique(flatten_chr(covs_final))
roads_and_census_2 <- roads_and_census %>% drop_na()  %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) 

roads_and_census_3 <- roads_and_census_2 %>% select(chr_lst[chr_lst != "pctsomecoll"], treated) %>% 
  rename_with(~new_names[.x], .names = chr_lst[chr_lst != "pctsomecoll"])

covs_bal_tab <- bal.tab(roads_and_census_3 %>% select(-treated) %>% 
                          rename(),
                        treat = roads_and_census_3$treated)
love.plot(covs_bal_tab, thresholds = 0.5, colors = c("#1e90ff"))

#  standardize variables
roads_and_census_std <- roads_and_census_2 %>% mutate(across(chr_lst, ~ (.-mean(.))/sd(.)))

# group by treated and the compute mean and standard deviation. Transpose the table
roads_and_census_std %>% group_by(treated) %>% summarize(across(chr_lst, list(mean = ~mean(., na.rm = TRUE), 
                                                                      sd = ~sd(., na.rm = TRUE))) ) %>% 
  pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = treated, values_from = value, names_prefix = "treated_") %>%
  mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""),
         std_mean_diff = treated_1 - treated_0)


# th <- roads_and_census %>% filter(treated == 1) %>% select(pctlesshs) %>% pull() 
# 
# mean(th)/sd(th) - mean(tu)/sd(tu)
# 
# 
# tu <- roads_and_census %>% filter(treated == 0) %>% select(pctlesshs) %>% pull() 
# tu
# mean(tu)/sd(tu)

#============================================================================================================#
#                        Covariates Table for paper  ----
#============================================================================================================#

# colnames(roads_and_census)
# 
# roads_and_census %>% View()

# global: aggregate
means_global_agg <- roads_and_census %>% select(all_of(covs_list)) %>%
  summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))), count = n()) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "global_value") %>% 
  mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", "")) 
# means_global_agg %>% print(n = 67)
# global: passed and failed
means_global_div <- roads_and_census %>% select(treated, votes_pct_against, all_of(covs_list)) %>%
  group_by(treated) %>%
  summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))), count = n()) %>%
  pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = treated, values_from = value, names_prefix = "global_treated_") %>% 
  mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", "")) %>%
  filter(variable != "votes_pct_against")
# means_global_div %>% tail()
# means_global_div %>% print(n = 67)

# average bandwidth length (from t+0 to t+10)
avg_bw <- mean(map_dbl(gs[4:length(gs)], ~round(.x$bws[1,],1)[[1]]))

# effective 1: agg
means_eff_agg <-   roads_and_census %>% select(votes_pct_against, all_of(covs_list)) %>%
    filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
    summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        sd = ~sd(., na.rm = TRUE))), count = n()) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "effective_value") %>% 
    mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""))
# means_eff_agg  %>% tail()
# effective 1: passed and failed
means_eff_div <-   roads_and_census %>% select(treated, votes_pct_against, all_of(covs_list)) %>%
    filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% 
    select(-votes_pct_against) %>%
    group_by(treated) %>%
    summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        sd = ~sd(., na.rm = TRUE))), count = n()) %>%
    pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>% 
    pivot_wider(names_from = treated, values_from = value, names_prefix = "effective_treated_") %>% 
    mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""))
# means_eff_div  %>% tail()
# means_global_agg %>% print(n = 67)


#------- Covariates Table ---------#  
tbl1 <- means_global_agg %>% 
          left_join(means_global_div, by = c("variable","statistic")) %>%
          left_join(means_eff_agg, by = c("variable","statistic")) %>%
          left_join(means_eff_div, by = c("variable","statistic"))  

tbl1
# tbl1 %>% tail()

# Define a function to add parentheses for 'sd'
add_parentheses <- function(x, stat) {
  ifelse(!is.na(stat) & stat == "sd", paste0("[", x, "]"), as.character(x))
}

# exporting tbl1 as csv
tbl1 %>% 
  # filter(variable %in% c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctlesshs", "pctsinparhhld", "pctlt5", "count")) %>%
  filter(variable %in% unique(flatten_chr(covs_final))) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  relocate(variable, statistic, everything()) %>%
  select(-effective_value) %>%
  rename(`Global: Full Sample` = global_value, `Global: Failed levies` = global_treated_0, `Global: Passed levies` = global_treated_1,
         `Eff: Passed levies` = effective_treated_1, `Eff: Failed levies` = effective_treated_0)  %>%
  rowwise() %>% 
  # mutate(across(where(is.numeric), ~add_parentheses(., statistic)))  %>%
  ungroup() %>% print(. , n = 48)
  # write.csv(paste0(tables,"/covar_bal_tab.csv"), row.names = FALSE)



# mean/median/sd vote share
roads_and_census$votes_pct_against %>% mean %>% round
roads_and_census$votes_pct_against %>% median %>% round
roads_and_census$votes_pct_against %>% sd %>% round
# mean/median/sd vote share excluding The Great Recession years (2008-2009)
roads_and_census %>% filter(year != 2008 & year != 2009) %>% pull(votes_pct_against) %>% mean %>% round
roads_and_census %>% filter(year != 2008 & year != 2009) %>% pull(votes_pct_against) %>% median %>% round
roads_and_census %>% filter(year != 2008 & year != 2009) %>% pull(votes_pct_against) %>% sd %>% round

# mean/median population with treated as the grouping variable and observations only within bandwidth of cutoff
pop <- roads_and_census %>% filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% 
  group_by(treated) %>% 
  summarize(mean_pop = mean(pop), median_pop = median(pop), sd_pop = sd(pop)) 
pop$mean_pop[2] - pop$mean_pop[1] # difference in means



medfamy <- roads_and_census %>% filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% filter(!is.na(medfamy)) %>%
  group_by(treated) %>% 
  summarize(mean_medfamy= mean(medfamy), median_medfamy = median(medfamy), sd_pop = sd(medfamy)) 
medfamy$mean_medfamy[2] - medfamy$mean_medfamy[1] # difference in means


#==== loop to get covariates t-tests val and s.e for paper ====#
df_eff <- roads_and_census %>% select(treated, votes_pct_against, all_of(covs_list)) %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw))

results <- list()
for (i in seq_along(covariates)) {
  cov <- covariates[i]
  t_test_result <- t.test(df_eff[df_eff$treated == 1, ] %>% pull(cov), 
                          df_eff[df_eff$treated == 0, ] %>% pull(cov), var.equal = TRUE, na.rm = TRUE)
  results$t_stat[i] <- t_test_result$statistic
  results$p_value[i] <- t_test_result$p.value
}


t_test <- t.test(df_eff[df_eff$treated == 1, ] %>% pull(covs_list[1]), 
                        df_eff[df_eff$treated == 0, ] %>% pull(covs_list[1]), var.equal = TRUE, na.rm = TRUE)

t_test$statistic

t_test$stderr

diffs_comps <- purrr::map(covs_list, ~ t.test(df_eff[[.x]] ~ df_eff$treated ))
names(diffs_comps) <- covs_list


walk2(diffs_comps, covs_list, ~ print(paste0(.y, " | Statistic: ", round(.x[["statistic"]], 2), " | Std Error: ", round(.x[["stderr"]],2), " | P-val: ", .x[["p.value"]] )))

#====================================================#
# t-3, t-2, t-1 means for Data section of paper ----
#====================================================#

## Housing Price

# global: aggregate
map2(dfs_agg_covs, names(dfs_agg_covs), ~  .x %>% mutate(dataset = .y) ) %>% 
  # .[grepl("t_minus", names(.)) ] %>%
  bind_rows() %>%
  # filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% 
  select(-votes_pct_against) %>%
  group_by(dataset) %>%
  summarize(global_mean_hp = mean(median_sale_amount), global_sd_hp = sd(median_sale_amount)) 

# global: passed and failed
map2(dfs_agg_covs, names(dfs_agg_covs), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  # .[grepl("t_minus", names(.)) ] %>% 
  bind_rows() %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(median_sale_amount), div_sd_hp = sd(median_sale_amount)) 

# effective 1: agg
map2(dfs_agg_covs, names(dfs_agg_covs), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  # .[grepl("t_minus", names(.)) ] %>% 
  bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset) %>%
  summarize(div_mean_hp = mean(median_sale_amount), div_sd_hp = sd(median_sale_amount)) 

# effective 1: passed and failed
map2(dfs_agg_covs, names(dfs_agg_covs), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  # .[grepl("t_minus", names(.)) ] %>% 
  bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(median_sale_amount), div_sd_hp = sd(median_sale_amount)) %>% print(. , n = 28)


## Employment

# global: aggregate
map2(dfs_emp_agg_per , names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y) ) %>% .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset) %>%
  summarize(global_mean_hp = mean(avg_persons), global_sd_hp = sd(avg_persons)) 

# global: passed and failed
map2(dfs_emp_agg_per, names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(avg_persons), div_sd_hp = sd(avg_persons)) 

# effective 1: passed and failed
map2(dfs_emp_agg_per, names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(avg_persons), div_sd_hp = sd(avg_persons)) 

## Wages

# global: aggregate
map2(dfs_emp_agg_per , names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y) ) %>% .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset) %>%
  summarize(global_mean_hp = mean(tot_wages), global_sd_hp = sd(tot_wages)) 

# global: passed and failed
map2(dfs_emp_agg_per, names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(wages_per_cap), div_sd_hp = sd(wages_per_cap)) 

# effective 1: passed and failed
map2(dfs_emp_agg_per, names(dfs_emp_agg_per), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(wages_per_cap), div_sd_hp = sd(wages_per_cap)) 


## Wages per cap

# global: aggregate
map2(dfs_emp_agg_p , names(dfs_emp_agg_p), ~  .x %>% mutate(dataset = .y) ) %>% .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset) %>%
  summarize(global_mean_hp = mean(wages_per_emp), global_sd_hp = sd(wages_per_emp)) 

# global: passed and failed
map2(dfs_emp_agg_p, names(dfs_emp_agg_p), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(wages_per_emp), div_sd_hp = sd(wages_per_emp)) 

# effective 1: passed and failed
map2(dfs_emp_agg_p, names(dfs_emp_agg_p), ~  .x %>% mutate(dataset = .y, treated = if_else(votes_pct_against > cutoff, 1, 0)) ) %>% 
  .[grepl("t_minus", names(.)) ] %>% bind_rows() %>%
  filter(between(votes_pct_against, cutoff - avg_bw, cutoff + avg_bw)) %>% select(-votes_pct_against) %>%
  group_by(dataset, treated) %>%
  summarize(div_mean_hp = mean(wages_per_emp), div_sd_hp = sd(wages_per_emp)) 


hpp <- hs_winsorized[[1]] %>% 
  group_by(year) %>% 
  summarize(mean_sale = mean(SALE_AMOUNT), sd = sd(SALE_AMOUNT)) %>% 
  filter(!(year %in% c(".", "1994") )) %>% mutate(year = as.numeric(year))



