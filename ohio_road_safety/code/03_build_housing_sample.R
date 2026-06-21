suppressPackageStartupMessages({
  library(haven)
  library(readr)
  library(dplyr)
  library(stringr)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

roads <- read_dta(paths$inputs$roads_and_census) %>%
  transmute(
    tendigit_fips = as_fips_chr(TENDIGIT_FIPS),
    year = as.integer(year),
    subdivisiontype = str_trim(as.character(subdivisiontype)),
    votes_pct_for = as.numeric(votes_pct_for),
    votes_pct_against = as.numeric(votes_pct_against),
    purpose2 = str_trim(as.character(purpose2)),
    description = str_trim(as.character(description))
  ) %>%
  mutate(
    fips_id = fips_id_from_tendigit(tendigit_fips, subdivisiontype)
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

fars_panel <- read_csv(paths$outputs$fars_panel, show_col_types = FALSE) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    year = as.integer(year)
  )

housing_tx <- read_dta(
  paths$inputs$housesales,
  col_select = c(
    FIPS_ID, SALE_AMOUNT, acres, universal_building_square_feet, year_built, total_rooms,
    total_baths_calculated, year, agehouse, ac, basement,
    cond_exc, cond_fair, cond_good, cond_poor, cond_vgood,
    onestory, condo, lat, lon
  )
) %>%
  transmute(
    fips_id = as_fips_chr(FIPS_ID),
    year = as.integer(year),
    sale_amount = as.numeric(SALE_AMOUNT),
    acres = as.numeric(acres),
    universal_built = as.numeric(universal_building_square_feet),
    year_built = as.numeric(year_built),
    total_rooms = as.numeric(total_rooms),
    total_baths_calculated = as.numeric(total_baths_calculated),
    agehouse = as.numeric(agehouse),
    ac = as.numeric(ac),
    basement = as.numeric(basement),
    cond_exc = as.numeric(cond_exc),
    cond_fair = as.numeric(cond_fair),
    cond_good = as.numeric(cond_good),
    cond_poor = as.numeric(cond_poor),
    cond_vgood = as.numeric(cond_vgood),
    onestory = as.numeric(onestory),
    condo = as.numeric(condo),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  filter(
    year >= 2006,
    year <= 2021,
    !is.na(fips_id),
    !is.na(sale_amount),
    sale_amount > 1000
  ) %>%
  mutate(log_sale_amount = log(sale_amount))

housing_panel <- housing_tx %>%
  group_by(fips_id, year) %>%
  summarise(
    n_sales = n(),
    median_sale_amount = median(sale_amount, na.rm = TRUE),
    mean_sale_amount = mean(sale_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_median_sale_amount = log(median_sale_amount),
    log_mean_sale_amount = log(mean_sale_amount)
  ) %>%
  left_join(fars_panel, by = c("fips_id", "year")) %>%
  left_join(annual_controls, by = c("fips_id", "year")) %>%
  left_join(
    roads %>% select(fips_id, year, votes_pct_for, votes_pct_against, purpose2, description),
    by = c("fips_id", "year")
  ) %>%
  mutate(
    fatal_crashes = coalesce(fatal_crashes, 0),
    fatalities = coalesce(fatalities, 0),
    persons_involved = coalesce(persons_involved, 0),
    vehicles_involved = coalesce(vehicles_involved, 0),
    fatal_crashes_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatal_crashes / pop, NA_real_),
    fatalities_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatalities / pop, NA_real_)
  )

housing_tx_analysis <- housing_tx %>%
  left_join(
    fars_panel %>% select(fips_id, year, fatal_crashes, fatalities, persons_involved, vehicles_involved),
    by = c("fips_id", "year")
  ) %>%
  left_join(
    annual_controls %>% select(fips_id, year, pop, medfamy, poverty, unemprate, pctown, pctrent),
    by = c("fips_id", "year")
  ) %>%
  left_join(
    roads %>% select(fips_id, year, votes_pct_for, votes_pct_against, purpose2, description),
    by = c("fips_id", "year")
  ) %>%
  mutate(
    fatal_crashes = coalesce(fatal_crashes, 0),
    fatalities = coalesce(fatalities, 0),
    fatal_crashes_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatal_crashes / pop, NA_real_),
    fatalities_per_10k = if_else(!is.na(pop) & pop > 0, 10000 * fatalities / pop, NA_real_)
  )

write_csv(housing_tx, paths$outputs$housing_tx)
write_csv(housing_panel, paths$outputs$housing_panel)
write_csv(housing_tx_analysis, paths$outputs$housing_tx_analysis)

message("Wrote housing transactions to: ", paths$outputs$housing_tx)
message("Wrote housing panel to: ", paths$outputs$housing_panel)
message("Wrote housing analysis sample to: ", paths$outputs$housing_tx_analysis)
