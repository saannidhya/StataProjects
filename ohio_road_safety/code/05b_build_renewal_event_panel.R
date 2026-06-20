suppressPackageStartupMessages({
  library(readr)
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

if (!file.exists(paths$outputs$local_gov_year_panel)) {
  stop(
    "Missing annual panel: ", paths$outputs$local_gov_year_panel,
    ". Run code/05_build_analysis_panels.R first."
  )
}

event_time_label <- function(x) {
  ifelse(x < 0, paste0("m", abs(x)), paste0("p", x))
}

row_mean_cols <- function(df, cols) {
  present <- cols[cols %in% names(df)]

  if (length(present) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  mat <- as.matrix(df[, present, drop = FALSE])
  out <- rowMeans(mat, na.rm = TRUE)
  out[rowSums(!is.na(mat)) == 0] <- NA_real_
  out
}

row_nonmissing_cols <- function(df, cols) {
  present <- cols[cols %in% names(df)]

  if (length(present) == 0) {
    return(rep(0L, nrow(df)))
  }

  mat <- as.matrix(df[, present, drop = FALSE])
  rowSums(!is.na(mat))
}

outcome_vars <- c(
  "fatal_crashes",
  "fatalities",
  "any_fatal_crash",
  "fatal_crashes_per_10k",
  "fatalities_per_10k",
  "n_sales",
  "median_sale_amount",
  "mean_sale_amount",
  "log_median_sale_amount",
  "log_mean_sale_amount",
  "total_wages",
  "avg_persons",
  "establishments",
  "entrants",
  "exits",
  "log_total_wages",
  "log_avg_persons",
  "log_establishments"
)

aggregate_vars <- c(
  "fatal_crashes_per_10k",
  "fatalities_per_10k",
  "any_fatal_crash",
  "log_median_sale_amount",
  "n_sales",
  "log_total_wages",
  "log_avg_persons",
  "log_establishments",
  "entrants",
  "exits"
)

local_panel <- read_csv(
  paths$outputs$local_gov_year_panel,
  show_col_types = FALSE,
  col_select = any_of(c("fips_id", "year", outcome_vars))
) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    year = as.integer(year)
  ) %>%
  filter(!is.na(fips_id), !is.na(year)) %>%
  distinct(fips_id, year, .keep_all = TRUE)

renewals <- read_dta(paths$inputs$roads_and_census) %>%
  transmute(
    tendigit_fips = as_fips_chr(TENDIGIT_FIPS),
    election_year = as.integer(year),
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
    votes_pct_against = as.numeric(votes_pct_against),
    pop_election = as.numeric(pop),
    medfamy_election = as.numeric(medfamy),
    poverty_election = as.numeric(poverty),
    unemprate_election = as.numeric(unemprate),
    pctown_election = as.numeric(pctown),
    pctrent_election = as.numeric(pctrent)
  ) %>%
  mutate(
    fips_id = fips_id_from_tendigit(tendigit_fips, subdivisiontype),
    margin_against = votes_pct_against - 50,
    abs_margin_against = abs(margin_against),
    failed_levy = as.integer(votes_pct_against > 50),
    close_3 = as.integer(abs_margin_against <= 3),
    close_5 = as.integer(abs_margin_against <= 5),
    close_7_5 = as.integer(abs_margin_against <= 7.5),
    duration_is_perpetual = as.integer(duration == "1000"),
    duration_is_missing = as.integer(is.na(duration) | duration %in% c("", "-999", "N/A"))
  ) %>%
  filter(
    !is.na(fips_id),
    tolower(purpose2) == "roads",
    description == "R",
    election_year >= 2006,
    election_year <= 2021
  ) %>%
  group_by(fips_id, election_year) %>%
  arrange(abs_margin_against, .by_group = TRUE) %>%
  mutate(renewals_same_year = n()) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(fips_id, election_year) %>%
  group_by(fips_id) %>%
  mutate(
    prev_renewal_year = lag(election_year),
    next_renewal_year = lead(election_year),
    years_since_prev_renewal = election_year - prev_renewal_year,
    years_until_next_renewal = next_renewal_year - election_year,
    isolated_3 = as.integer(
      coalesce(years_since_prev_renewal, 99L) > 3 &
        coalesce(years_until_next_renewal, 99L) > 3
    ),
    isolated_5 = as.integer(
      coalesce(years_since_prev_renewal, 99L) > 5 &
        coalesce(years_until_next_renewal, 99L) > 5
    )
  ) %>%
  ungroup() %>%
  mutate(
    event_id = paste(fips_id, election_year, sep = "_"),
    available_pre_m3_m1 = as.integer(election_year >= 2009),
    available_post_p1_p3 = as.integer(election_year <= 2018),
    available_post_p2_p4 = as.integer(election_year <= 2017),
    available_post_p2_p5 = as.integer(election_year <= 2016),
    available_post_p1_p5 = as.integer(election_year <= 2016)
  )

event_times <- -3:6

event_grid <- renewals %>%
  select(event_id, fips_id, election_year) %>%
  tidyr::crossing(event_time = event_times) %>%
  mutate(
    year = election_year + event_time,
    event_time_label = event_time_label(event_time)
  )

event_outcomes <- event_grid %>%
  left_join(local_panel, by = c("fips_id", "year"))

event_wide <- event_outcomes %>%
  select(event_id, event_time_label, any_of(outcome_vars)) %>%
  pivot_wider(
    names_from = event_time_label,
    values_from = any_of(outcome_vars),
    names_glue = "{.value}_{event_time_label}"
  )

event_panel <- renewals %>%
  left_join(event_wide, by = "event_id")

for (var in aggregate_vars) {
  pre_cols <- paste0(var, "_", c("m3", "m2", "m1"))
  crash_post_cols <- paste0(var, "_", c("p1", "p2", "p3"))
  downstream_mid_cols <- paste0(var, "_", c("p2", "p3", "p4"))
  downstream_post_cols <- paste0(var, "_", c("p2", "p3", "p4", "p5"))

  event_panel[[paste0(var, "_pre_avg_m3_m1")]] <- row_mean_cols(event_panel, pre_cols)
  event_panel[[paste0(var, "_post_avg_p1_p3")]] <- row_mean_cols(event_panel, crash_post_cols)
  event_panel[[paste0(var, "_post_avg_p2_p4")]] <- row_mean_cols(event_panel, downstream_mid_cols)
  event_panel[[paste0(var, "_post_avg_p2_p5")]] <- row_mean_cols(event_panel, downstream_post_cols)
  event_panel[[paste0(var, "_n_pre_m3_m1")]] <- row_nonmissing_cols(event_panel, pre_cols)
  event_panel[[paste0(var, "_n_post_p1_p3")]] <- row_nonmissing_cols(event_panel, crash_post_cols)
  event_panel[[paste0(var, "_n_post_p2_p4")]] <- row_nonmissing_cols(event_panel, downstream_mid_cols)
  event_panel[[paste0(var, "_n_post_p2_p5")]] <- row_nonmissing_cols(event_panel, downstream_post_cols)
  event_panel[[paste0(var, "_diff_p1_p3_vs_pre")]] <-
    event_panel[[paste0(var, "_post_avg_p1_p3")]] - event_panel[[paste0(var, "_pre_avg_m3_m1")]]
  event_panel[[paste0(var, "_diff_p2_p4_vs_pre")]] <-
    event_panel[[paste0(var, "_post_avg_p2_p4")]] - event_panel[[paste0(var, "_pre_avg_m3_m1")]]
  event_panel[[paste0(var, "_diff_p2_p5_vs_pre")]] <-
    event_panel[[paste0(var, "_post_avg_p2_p5")]] - event_panel[[paste0(var, "_pre_avg_m3_m1")]]
}

event_panel <- event_panel %>%
  mutate(
    baseline_close_5 = as.integer(close_5 == 1 & duration_is_perpetual == 0),
    recommended_crash_sample = as.integer(
      close_5 == 1 &
        duration_is_perpetual == 0 &
        available_pre_m3_m1 == 1 &
        available_post_p1_p3 == 1 &
        (is.na(years_until_next_renewal) | years_until_next_renewal > 3)
    ),
    recommended_downstream_sample = as.integer(
      close_5 == 1 &
        duration_is_perpetual == 0 &
        available_pre_m3_m1 == 1 &
        available_post_p2_p4 == 1 &
        (is.na(years_until_next_renewal) | years_until_next_renewal > 4)
    ),
    recommended_downstream_long_sample = as.integer(
      close_5 == 1 &
        duration_is_perpetual == 0 &
        available_pre_m3_m1 == 1 &
        available_post_p2_p5 == 1 &
        (is.na(years_until_next_renewal) | years_until_next_renewal > 5)
    )
  )

write_csv(event_panel, paths$outputs$renewal_event_panel)

message("Wrote renewal event panel to: ", paths$outputs$renewal_event_panel)
message("Renewal elections in 2006-2021: ", nrow(event_panel))
message(
  "Close-5 finite-duration renewals: ",
  sum(event_panel$baseline_close_5, na.rm = TRUE)
)
message(
  "Recommended crash sample: ",
  sum(event_panel$recommended_crash_sample, na.rm = TRUE)
)
message(
  "Recommended downstream sample: ",
  sum(event_panel$recommended_downstream_sample, na.rm = TRUE)
)
message(
  "Recommended downstream long sample: ",
  sum(event_panel$recommended_downstream_long_sample, na.rm = TRUE)
)
