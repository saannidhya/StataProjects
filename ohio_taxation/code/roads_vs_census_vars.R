# purpose: analyze impact of road spending on certain census variables 
# created on: 17th March 2022
# last updated: 17th March 2022

# loading libraries
library(tidyverse)
library(haven)
library(Rbearcat)

# specify root
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects"

# loading datasets
# roads <- haven::read_dta(file.path(root, "ohio_taxation", "data","roads_levies2_census_9118.dta"))
roads <- haven::read_sas(file.path(root, "ohio_taxation", "data","road_tax_and_census_oh.sas7bdat"))
census <- haven::read_dta(file.path(root, "ohio_taxation", "data","cosub_place_panel_property2_9018.dta"))


# creating pct_votes variable
roads <- roads %>%
            mutate(pct_votes = (votes_for/(votes_for+votes_against))*100,
                   vote_result = if_else(pct_votes >= 50, "pass", "fail"),
                   across(contains("pop_lead|medfamy_lead"), ~ log(.x) ),
                   pct_votes_ctrd = pct_votes - 50 ) %>%
            filter( (description == "R")) 
            
# plotting running variable vs some outcome variable
purrr::map(grep("pctrent", colnames(roads), value = TRUE), 
           
           ~bcat_plt_point(df = roads,
                          x = pct_votes,
                          y = get({.x}),
                          color = vote_result,
                          x_refline = 50,
                          y_lab = {.x}) +
                geom_smooth(method = "lm", size = 2)
           )



# Mcrary test on running variable
ggplot(roads_k, aes(x = pct_votes_ctrd, fill = vote_result)) +
    geom_histogram(binwidth = 1, boundary = 50, color = "white")
rddensity::rdplotdensity(rdd = rddensity(roads_k$pct_votes, c = 50),
                         X = roads_k$pct_votes,
                         type = "both")

# some arbitrary cutoff

treatment_effect_finder <- function(df, y_list, x, cutoff) {
    
    # reducing dataset to some arbitrary cutoof
    df <- filter(df, pct_votes_ctrd >= -cutoff, pct_votes_ctrd <= cutoff) 
    
    # storing all the regressions
    my_lms <- purrr::map(y_list, ~ lm(df[[{.x}]] ~ df[[x]] + df[["vote_result"]], data = df))
    my_lms <- setNames(my_lms, y_list)
    
    summaries <- purrr::map(my_lms,summary)
    
    # extracting only p-values
    est_df <- purrr::map_df(summaries, function(x) x$coefficients[, 4] )
    
    # add variable names to dataset
    est_df$var_name <- y_list
    
    colnames(est_df) <- c("intercept",x,"vote_result","var_name")
    
    return(est_df)
    
}

c <- treatment_effect_finder(df = roads, 
                            y_list = grep("lead", colnames(roads), value = TRUE),
                            x = "pct_votes_ctrd",
                            cutoff = 10)

c %>% arrange(vote_result)

bcat_plt_point(df = filter(roads, between(pct_votes_ctrd, -10,10) ), 
               x = pct_votes_ctrd,
               y = log(cl2demolishedcap_lead4),
               color = vote_result) +
    geom_smooth(method = "lm", size = 2)



############## checking Census data for leads ###############################

x <- census %>%
    select(c(year, TENDIGIT_FIPS, contains("unemprate")))

