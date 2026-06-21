suppressPackageStartupMessages({
  library(readr)
  library(haven)
  library(dplyr)
  library(stringr)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

roads <- read_dta(paths$inputs$roads_and_census) %>%
  transmute(
    tendigit_fips = as_fips_chr(TENDIGIT_FIPS),
    year = as.integer(year),
    subdivisionname = str_trim(as.character(subdivisionname)),
    subdivisiontype = str_trim(as.character(subdivisiontype)),
    county = str_trim(as.character(county)),
    taxtype = str_trim(as.character(taxtype)),
    purpose2 = str_trim(as.character(purpose2)),
    description = str_trim(as.character(description)),
    duration = str_trim(as.character(duration)),
    millagepercent = str_trim(as.character(millagepercent)),
    votesfor = as.numeric(votesfor),
    votesagainst = as.numeric(votesagainst),
    votes_pct_for = as.numeric(votes_pct_for),
    votes_pct_against = as.numeric(votes_pct_against)
  ) %>%
  mutate(
    fips_id = fips_id_from_tendigit(tendigit_fips, subdivisiontype),
    margin_against = votes_pct_against - 50,
    failed_levy = as.integer(votes_pct_against > 50),
    close_3 = as.integer(abs(margin_against) <= 3),
    close_5 = as.integer(abs(margin_against) <= 5),
    close_7_5 = as.integer(abs(margin_against) <= 7.5)
  )

annual_controls_raw <- read_dta(paths$inputs$cosub_place_panel)
fips_name <- if ("FIPS_ID" %in% names(annual_controls_raw)) "FIPS_ID" else "Census_FIPS"

annual_controls <- annual_controls_raw %>%
  transmute(
    fips_id = as_fips_chr(.data[[fips_name]]),
    year = as.integer(year),
    pop = as.numeric(pop),
    medfamy = as.numeric(medfamy),
    poverty = as.numeric(poverty),
    unemprate = as.numeric(unemprate),
    pctown = as.numeric(pctown),
    pctrent = as.numeric(pctrent)
  ) %>%
  filter(!is.na(fips_id)) %>%
  filter(year >= 2006, year <= 2021)

write_csv(annual_controls, paths$outputs$annual_controls)
write_csv(roads, paths$outputs$roads_panel)

fars_panel <- read_csv(paths$outputs$fars_panel, show_col_types = FALSE) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    year = as.integer(year)
  )

housing_panel <- read_csv(paths$outputs$housing_panel, show_col_types = FALSE) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    year = as.integer(year)
  )

employment_panel <- read_csv(paths$outputs$employment_panel, show_col_types = FALSE) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    year = as.integer(year)
  )

local_gov_year_panel <- annual_controls %>%
  full_join(fars_panel, by = c("fips_id", "year")) %>%
  full_join(roads %>% filter(!is.na(fips_id)), by = c("fips_id", "year")) %>%
  full_join(
    housing_panel %>%
      select(fips_id, year, n_sales, median_sale_amount, mean_sale_amount, log_median_sale_amount, log_mean_sale_amount),
    by = c("fips_id", "year")
  ) %>%
  full_join(employment_panel, by = c("fips_id", "year")) %>%
  filter(year >= 2006, year <= 2021) %>%
  mutate(
    fatal_crashes = coalesce(fatal_crashes, 0),
    fatalities = coalesce(fatalities, 0),
    persons_involved = coalesce(persons_involved, 0),
    vehicles_involved = coalesce(vehicles_involved, 0),
    any_fatal_crash = coalesce(any_fatal_crash, 0L),
    fatal_crashes_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatal_crashes / pop, NA_real_),
    fatalities_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatalities / pop, NA_real_),
    log_total_wages = if_else(!is.na(total_wages) & total_wages > 0, log(total_wages), NA_real_),
    log_avg_persons = if_else(!is.na(avg_persons) & avg_persons > 0, log(avg_persons), NA_real_),
    log_establishments = if_else(!is.na(establishments) & establishments > 0, log(establishments), NA_real_)
  )

write_csv(local_gov_year_panel, paths$outputs$local_gov_year_panel)

message("Wrote annual controls with FIPS_ID to: ", paths$outputs$annual_controls)
message("Wrote roads panel with FIPS_ID to: ", paths$outputs$roads_panel)
message("Wrote merged local-government-year panel to: ", paths$outputs$local_gov_year_panel)
