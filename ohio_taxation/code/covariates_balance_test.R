#==========================================================================================================#
# Purpose : Identify which covariates are balanced between treatment and control groups
# Name    : Saani Rawat
# Created : 07/27/2022
# Log     : 07/29/2022 : finished loop for covariate balance test for all datasets
#           11/23/2023 : Removed work on post-outcome covariates datasets (datasets that contain both outcome 
#                        and covariates). Now, doing balancing only on covariates and running variables.
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
# source(paste0(code,"/employment_data_setup.R"))

# list of covariates
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")


# importing roads and census dataset. Selecting only renewals and levies that do not last forever. Separating into treatment and control groups.
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  select(-c("inctaxrate")) %>%
  filter(description == "R" & duration != 1000) %>%
  janitor::clean_names() %>%
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))    

#============================================================================================================#
#     Covariate discontinuity test (before introducing outcome variable) ----
#============================================================================================================#
# |- RD using covariates ####

regs_covs <- purrr::map(covs_list, .f = function(var){
                        rdrobust::rdrobust(y = roads_and_census[[var]], 
                                           x = roads_and_census$votes_pct_for, 
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
# Using matching techniques
#=======================================#
m.out <- MatchIt::matchit(formula = treated ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctnokids + pctlesshs + 
                              pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctrent + pctown + 
                              pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + 
                              pctmin + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl,
                            data = roads_and_census, method = 'nearest', estimand = "ATT")

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
  summarize(across(-c(tendigit_fips, year, tendigit_fips_year, tax_type, purpose2, description, 
                      votes_pct_for, votes_pct_for_cntr, votes_against, votes_for), 
                   ~mean(.x, na.rm=TRUE) ))


t_tests <- purrr::map(.x = col_list[col_list != c("median_sale_amount", "median_ln_sale_amount", "inctaxrate")], 
                      .f = ~ t_test(df = roads_and_census, 
                                    var = .x)) %>% 
            bind_rows() 



covs_bal_tab <- bal.tab(roads_and_census %>% select(col_list[col_list != c("median_sale_amount", "median_ln_sale_amount", "inctaxrate")]),
                        treat = roads_and_census$treated)
love.plot(covs_bal_tab, thresholds = 0.1)

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

colnames(roads_and_census)

# global: aggregate
means_global_agg <- roads_and_census %>% select(all_of(covs_list)) %>%
  summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))), count = n()) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "global_value") %>% 
  mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", "")) 
means_global_agg
# global: passed and failed
means_global_div <- roads_and_census %>% select(treated, all_of(covs_list)) %>%
  group_by(treated) %>%
  summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE))), count = n()) %>%
  pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = treated, values_from = value, names_prefix = "global_treated_") %>% 
  mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""))
means_global_div %>% tail()

# effective 1: agg
means_eff_agg <-   roads_and_census %>% select(votes_pct_for, all_of(covs_list)) %>%
    filter(between(votes_pct_for, 45, 55)) %>% select(-votes_pct_for) %>%
    summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        sd = ~sd(., na.rm = TRUE))), count = n()) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "effective_value") %>% 
    mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""))
means_eff_agg  %>% tail()
# effective 1: passed and failed
means_eff_div <-   roads_and_census %>% select(treated, votes_pct_for, all_of(covs_list)) %>%
    filter(between(votes_pct_for, 45, 55)) %>% select(-votes_pct_for) %>%
    group_by(treated) %>%
    summarize(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        sd = ~sd(., na.rm = TRUE))), count = n()) %>%
    pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>% 
    pivot_wider(names_from = treated, values_from = value, names_prefix = "effective_treated_") %>% 
    mutate(statistic = str_extract(variable,"mean|sd"), variable = str_replace(variable, "_mean|_sd", ""))
means_eff_div  %>% tail()

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
  filter(variable %in% c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctlesshs", "pctsinparhhld", "pctlt5", "count")) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  relocate(variable, statistic, everything()) %>%
  select(-effective_value) %>%
  rename(`Global: Full Sample` = global_value, `Global: Failed levies` = global_treated_0, `Global: Passed levies` = global_treated_1,
         `Eff: Passed levies` = effective_treated_1, `Eff: Failed levies` = effective_treated_0)  %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~add_parentheses(., statistic)))  %>%
  ungroup() %>%
  write.csv(paste0(tables,"/tbl1.csv"), row.names = FALSE)




