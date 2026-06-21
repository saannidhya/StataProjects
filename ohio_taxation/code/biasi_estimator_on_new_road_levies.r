#' Purpose: Run Estimator from Biasi, Lafortune and Schonholzer (2025) on additional road tax levy and employment data
#' Data cleaning done by odjfs_data_setup_for_biasi.R
#' Imported datasets: roads_emp_stacked.csv and roads_emp_stacked_by_naics.csv
#' Created by: Saani Rawat
#' Log: 
#'   1. 28Aug25: started the code
#'   2. 29Aug25: deflated wages. Ran regressions

# Dependencies --------------------------------------------------------------
library(tidyverse)
library(haven)      # read .dta
library(fixest)     # feols (multi-way FE, clustering, weights)
library(broom)      # tidiers
library(glue)       # string glue
library(janitor)    # clean_names
library(fs)         # dir_create
library(readr)

# Options / utils -----------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

# Yale colors (hex)
yaleblue <- rgb(0, 53, 107, maxColorValue = 255)
ylb      <- rgb(40,109,192, maxColorValue = 255)
yo       <- rgb(189,83, 25,  maxColorValue = 255)
ylight   <- rgb(217,233,242, maxColorValue = 255)

# Paths (EDIT root to your machine if needed) -------------------------------
# root <- "~/Dropbox/Research/School construction" # example
root     <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

# output directories
plot_dir <- file.path(root, "data/outputs/plots/BLS")
table_dir <- file.path(root, "data/outputs/tables/BLS")

# importing biasi utility functions
source(file.path(root, "code", "0_biasi_utils.R"))

# importing Biasi-style stacked datasets (prepared using odjfs_data_setup_for_biasi.R)
roads_emp_stacked <- read_csv(file.path(root, "data/employment", "roads_emp_stacked.csv")) %>% mutate(county_fips = substr(tendigit_fips, 1, 5))
roads_emp_stacked_by_naics <- read_csv(file.path(root, "data/employment", "roads_emp_stacked_by_naics.csv")) %>% mutate(county_fips = substr(tendigit_fips, 1, 5))

# CPI numbers
cpi_df <- readr::read_csv(paste0(root,"/data/CPIAUCSL_NBD20100101.csv")) %>% rename(cpi = CPIAUCSL_NBD20100101) %>%
  mutate(cpi_deflator = cpi/100, year = lubridate::year(observation_date)) %>% select(-observation_date)

# deflating wages to 2010 USD
roads_emp_stacked <- roads_emp_stacked %>%
  left_join(cpi_df, by = "year") %>%
  mutate(wage = round(wage / cpi_deflator),
         wage_per_emp = round(wage / num_employed)) %>% select(-c(cpi, cpi_deflator))
roads_emp_stacked_by_naics <- roads_emp_stacked_by_naics %>%
  left_join(cpi_df, by = "year") %>%
  mutate(wage = round(wage / cpi_deflator),
         wage_per_emp = round(wage / num_employed)) %>% select(-c(cpi, cpi_deflator))


# Variable lists (mirror Stata globals) -------------------------------------
D_lag        <- paste0("DL_", 5:1)
D_lead       <- paste0("DL", 1:10)
D_lag_small  <- paste0("DL_", 5:1)
D_lead_small <- paste0("DL", 1:10)

E_lag  <- paste0("EL_", 5:1)
E_lead <- paste0("EL", 1:10)

M_lag  <- c(paste0("ML_", 5:1), "ML_0")
M_lead <- paste0("ML", 1:10)

model_vars_all <- c(D_lag, "zero", D_lead, E_lead, E_lag, M_lag, M_lead)

# FE specs (fixest uses ^ to denote interacted FE) --------------------------
FE_main   <- c("tendigit_fips^cohort", "cohort^county_fips^year")


#==============================================================================================================#
# Aggregate Results
#==============================================================================================================#

# Keep only states that pass McCrary test (drop fips 29, 53, 40, 25)
d <- roads_emp_stacked
#   colnames(d)

# Redefine weights not to change over time (base earliest year >= 2006)
# replicate Stata logic
d2 <- d %>%
    group_by(tendigit_fips) %>%
    mutate(pop = max(pop, na.rm = TRUE)
    ) %>%
    filter(pop != -Inf) %>%
    mutate(zero = 0) 


# View(d2)
# colnames(d2)

# Prepare model var list present in data
xvars <- vars_exist(d2, model_vars_all)

  # ----------------  ----------------

# Function to analyze employment outcomes
biasi_estimator <- function(data, outcome_var, xvars, FE_main, plot_dir, 
                                     title_suffix = "", filename_suffix = "") {
  
  # Validate outcome variable
  valid_outcomes <- c("num_employed", "wage", "jobs_created", "jobs_destroyed", 
                     "num_firms", "wage_per_emp", "num_employed_per_firm")
  if (!outcome_var %in% valid_outcomes) {
    stop("outcome_var must be one of: ", paste(valid_outcomes, collapse = ", "))
  }
  
  # Prepare data with log transformation
  d_outcome <- data %>% 
    mutate(across(any_of(valid_outcomes), log)) %>%
    mutate(
      tendigit_fips = as.factor(tendigit_fips),
      year = as.factor(year)
    )
  
  # Create formula and run regression
  f_outcome <- as.formula(paste0(outcome_var, " ~ ", paste(xvars, collapse = " + "),
                                " | ", paste(FE_main, collapse = " + ")))
  m_outcome <- feols(f_outcome, data = d_outcome, weights = ~ pop, cluster = ~ tendigit_fips)
  
  # Generate event study series
  kvals <- -5:10  # Adjust range as needed
  outcome_series <- map_dfr(kvals, function(k) {
    nm <- if (k < 0) paste0("DL_", abs(k)) else if (k == 0) "zero" else paste0("DL", k)
    row <- tryCatch(get_coef(m_outcome, nm), error = function(e) tibble(b = 0, se = 0))
    tibble(k = k, b = ifelse(nm == "zero", 0, row$b), se = ifelse(nm == "zero", 0, row$se))
  })
  
  # Create plot
  plot_title <- glue("Effect of New Road Levies on {outcome_var} (%){title_suffix}")
  plot_filename <- glue("{outcome_var}_stacked_future{filename_suffix}.png")
  
  plot_event_stem(
    outcome_series,
    title = plot_title,
    ylab = "Estimate",
    outpath = file.path(plot_dir, plot_filename),
    level = .95
  )
  
  # Return model for further analysis if needed
  return(m_outcome)
}


# Will Loop over all valid outcome variables
valid_outcomes <- c("num_employed", "wage", "jobs_created", "jobs_destroyed", 
                   "num_firms", "wage_per_emp", "num_employed_per_firm")

# Run biasi_estimator for each outcome variable
models <- map(valid_outcomes, function(outcome) {
  message("Running analysis for: ", outcome)
  biasi_estimator(d2, outcome, xvars, FE_main, plot_dir)
})
names(models) <- valid_outcomes

# purrr::walk(models, ~ {
#   print(summary(.x))
# })

#==============================================================================================================#
# By Naics Code Groups
#==============================================================================================================#

d2 <- roads_emp_stacked_by_naics %>%
    group_by(tendigit_fips) %>%
    mutate(pop = max(pop, na.rm = TRUE)
    ) %>%
    filter(pop != -Inf) %>%
    mutate(zero = 0) 

# Prepare model var list present in data
xvars <- vars_exist(d2, model_vars_all)

# All NAICS codes
biasi_estimator(d2, "num_employed", xvars, FE_main, plot_dir, filename_suffix = "_naics")
biasi_estimator(d2, "wage", xvars, FE_main, plot_dir, filename_suffix = "_naics")
biasi_estimator(d2, "wage_per_emp", xvars, FE_main, plot_dir, filename_suffix = "_naics");
biasi_estimator(d2, "jobs_created", xvars, FE_main, plot_dir, filename_suffix = "_naics");
biasi_estimator(d2, "jobs_destroyed", xvars, FE_main, plot_dir, filename_suffix = "_naics");

purrr::walk(models, ~ {
  print(summary(.x))
})

# colnames(d2)

# NAICS lists
naics_groups <- list("construction" = c(23), "transportation" = c(48, 49), "manufacturing" = c(31, 32, 33), "retail" = c(44, 45), "wholesale" = c(42), "real estate" = c(53))

# Loop over NAICS groups
results <- map(names(naics_groups), function(group_name) {
    naics_codes <- naics_groups[[group_name]]
    message("Running analysis for NAICS group: ", group_name)
    
    # Filter data for this NAICS group
    group_data <- d2 %>% filter(naics_2digit %in% naics_codes)
    
    # Run biasi_estimator for each outcome variable
    map(valid_outcomes[1:4], function(outcome) {
        biasi_estimator(group_data, outcome, xvars, FE_main, plot_dir, 
                                     filename_suffix = paste0("_", group_name))
    })  %>% return()
})
names(results) <- names(naics_groups)


# NAICS: 23, 48, 49 (Construction and Transportation/Warehousing)
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 48, 49)), "num_employed", xvars, FE_main, plot_dir, filename_suffix = "_23-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 48, 49)), "wage", xvars, FE_main, plot_dir, filename_suffix = "_23-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 48, 49)), "wage_per_emp", xvars, FE_main, plot_dir, filename_suffix = "_23-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 48, 49)), "jobs_created", xvars, FE_main, plot_dir, filename_suffix = "_23-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 48, 49)), "jobs_destroyed", xvars, FE_main, plot_dir, filename_suffix = "_23-48-49") %>% summary()

# NAICS: 23, 31, 32, 33, 48, 49 (Construction, Manufacturing and Transportation/Warehousing)
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 31, 32, 33, 48, 49)), "num_employed", xvars, FE_main, plot_dir, filename_suffix = "_48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 31, 32, 33, 48, 49)), "wage", xvars, FE_main, plot_dir, filename_suffix = "_48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 31, 32, 33, 48, 49)), "wage_per_emp", xvars, FE_main, plot_dir, filename_suffix = "_48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 31, 32, 33, 48, 49)), "jobs_created", xvars, FE_main, plot_dir, filename_suffix = "_48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(23, 31, 32, 33, 48, 49)), "jobs_destroyed", xvars, FE_main, plot_dir, filename_suffix = "_48-49") %>% summary()

# NAICS: 42, 44, 45, 23, 31, 32, 33, 48, 49 (Retail Trade, Construction, Manufacturing and Transportation/Warehousing)
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45, 23, 31, 32, 33, 48, 49)), "num_employed", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45-23-31-32-33-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45, 23, 31, 32, 33, 48, 49)), "wage", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45-23-31-32-33-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45, 23, 31, 32, 33, 48, 49)), "wage_per_emp", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45-23-31-32-33-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45, 23, 31, 32, 33, 48, 49)), "jobs_created", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45-23-31-32-33-48-49") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45, 23, 31, 32, 33, 48, 49)), "jobs_destroyed", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45-23-31-32-33-48-49") %>% summary()

# NAICS: 42, 44, 45 (Retail Trade)
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45)), "num_employed", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45)), "wage", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45)), "wage_per_emp", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45)), "jobs_created", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45") %>% summary()
biasi_estimator(d2 %>% filter(naics_2digit %in% c(42, 44, 45)), "jobs_destroyed", xvars, FE_main, plot_dir, filename_suffix = "_42-44-45") %>% summary()

# No consistent effect yet.