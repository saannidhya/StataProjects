suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(fixest)
  library(tibble)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

event_path <- paths$outputs$renewal_event_panel

if (!file.exists(event_path)) {
  stop(
    "Missing renewal event panel: ", event_path,
    ". Run code/05b_build_renewal_event_panel.R first."
  )
}

controls <- c(
  "pop_election",
  "medfamy_election",
  "poverty_election",
  "unemprate_election",
  "pctown_election"
)

fit_local_linear <- function(data, outcome) {
  needed <- c(outcome, "failed_levy", "margin_against", "county", controls)

  model_data <- data %>%
    filter(if_all(all_of(needed), ~ !is.na(.x)))

  if (nrow(model_data) < 25 || dplyr::n_distinct(model_data$failed_levy) < 2) {
    return(NULL)
  }

  rhs <- c("failed_levy", "margin_against", "failed_levy:margin_against", controls)
  fml <- reformulate(rhs, response = outcome)

  list(
    model = feols(fml, data = model_data, cluster = ~ county),
    data = model_data
  )
}

extract_result <- function(fit, outcome, family, sample_name) {
  ct <- coeftable(fit$model)

  if (!("failed_levy" %in% rownames(ct))) {
    return(NULL)
  }

  tibble(
    family = family,
    sample = sample_name,
    outcome = outcome,
    estimate = unname(ct["failed_levy", "Estimate"]),
    std_error = unname(ct["failed_levy", "Std. Error"]),
    t_value = unname(ct["failed_levy", "t value"]),
    p_value = unname(ct["failed_levy", "Pr(>|t|)"]),
    n_obs = nrow(fit$data),
    n_counties = dplyr::n_distinct(fit$data$county),
    mean_outcome = mean(fit$data[[outcome]], na.rm = TRUE)
  )
}

run_model_family <- function(data, outcomes, family, sample_name) {
  fits <- list()
  rows <- list()

  for (outcome in outcomes) {
    fit <- fit_local_linear(data, outcome)

    if (is.null(fit)) {
      next
    }

    fits[[outcome]] <- fit$model
    rows[[outcome]] <- extract_result(fit, outcome, family, sample_name)
  }

  list(
    models = fits,
    results = bind_rows(rows)
  )
}

write_etable_txt <- function(models, output_path, title) {
  if (length(models) == 0) {
    writeLines(c(title, "", "No estimable models for this family."), output_path)
    return(invisible(NULL))
  }

  table_text <- capture.output(etable(models))
  writeLines(c(title, "", table_text), output_path)
}

event_panel <- read_csv(event_path, show_col_types = FALSE) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    election_year = as.integer(election_year),
    failed_levy = as.integer(failed_levy),
    close_5 = as.integer(close_5),
    duration_is_perpetual = as.integer(duration_is_perpetual),
    baseline_close_5 = as.integer(baseline_close_5),
    recommended_crash_sample = as.integer(recommended_crash_sample),
    recommended_downstream_sample = as.integer(recommended_downstream_sample),
    recommended_downstream_long_sample = as.integer(recommended_downstream_long_sample)
  )

close5_sample <- event_panel %>%
  filter(baseline_close_5 == 1)

crash_sample <- event_panel %>%
  filter(recommended_crash_sample == 1)

downstream_sample <- event_panel %>%
  filter(recommended_downstream_sample == 1)

downstream_long_sample <- event_panel %>%
  filter(recommended_downstream_long_sample == 1)

first_stage_outcomes <- c(
  "fatal_crashes_per_10k_p1",
  "fatal_crashes_per_10k_p2",
  "fatal_crashes_per_10k_p3",
  "fatal_crashes_per_10k_post_avg_p1_p3",
  "fatal_crashes_per_10k_diff_p1_p3_vs_pre",
  "fatalities_per_10k_post_avg_p1_p3",
  "any_fatal_crash_post_avg_p1_p3"
)

housing_outcomes <- c(
  "log_median_sale_amount_post_avg_p2_p4",
  "log_median_sale_amount_diff_p2_p4_vs_pre",
  "n_sales_post_avg_p2_p4",
  "n_sales_diff_p2_p4_vs_pre"
)

housing_long_outcomes <- c(
  "log_median_sale_amount_post_avg_p2_p5",
  "log_median_sale_amount_diff_p2_p5_vs_pre",
  "n_sales_post_avg_p2_p5",
  "n_sales_diff_p2_p5_vs_pre"
)

employment_outcomes <- c(
  "log_total_wages_post_avg_p2_p4",
  "log_total_wages_diff_p2_p4_vs_pre",
  "log_avg_persons_post_avg_p2_p4",
  "log_establishments_post_avg_p2_p4",
  "entrants_post_avg_p2_p4",
  "exits_post_avg_p2_p4"
)

employment_long_outcomes <- c(
  "log_total_wages_post_avg_p2_p5",
  "log_total_wages_diff_p2_p5_vs_pre",
  "log_avg_persons_post_avg_p2_p5",
  "log_establishments_post_avg_p2_p5",
  "entrants_post_avg_p2_p5",
  "exits_post_avg_p2_p5"
)

pretrend_outcomes <- c(
  "fatal_crashes_per_10k_m3",
  "fatal_crashes_per_10k_m2",
  "fatal_crashes_per_10k_m1",
  "log_median_sale_amount_m3",
  "log_median_sale_amount_m2",
  "log_median_sale_amount_m1",
  "log_total_wages_m3",
  "log_total_wages_m2",
  "log_total_wages_m1"
)

first_stage <- run_model_family(
  crash_sample,
  first_stage_outcomes,
  family = "first_stage",
  sample_name = "close5_finite_duration_recommended_crash"
)

housing_rf <- run_model_family(
  downstream_sample,
  housing_outcomes,
  family = "housing_reduced_form",
  sample_name = "close5_finite_duration_recommended_downstream"
)

housing_long_rf <- run_model_family(
  downstream_long_sample,
  housing_long_outcomes,
  family = "housing_reduced_form_long",
  sample_name = "close5_finite_duration_recommended_downstream_long"
)

employment_rf <- run_model_family(
  downstream_sample,
  employment_outcomes,
  family = "employment_reduced_form",
  sample_name = "close5_finite_duration_recommended_downstream"
)

employment_long_rf <- run_model_family(
  downstream_long_sample,
  employment_long_outcomes,
  family = "employment_reduced_form_long",
  sample_name = "close5_finite_duration_recommended_downstream_long"
)

pretrends <- run_model_family(
  close5_sample,
  pretrend_outcomes,
  family = "pretrend_placebo",
  sample_name = "close5_finite_duration"
)

results_tbl <- bind_rows(
  first_stage$results,
  housing_rf$results,
  housing_long_rf$results,
  employment_rf$results,
  employment_long_rf$results,
  pretrends$results
)

summary_lines <- c(
  "Renewal-only lagged design summary",
  "",
  paste("Total road renewals, 2006-2021:", nrow(event_panel)),
  paste("Close-5 finite-duration renewals:", nrow(close5_sample)),
  paste("Close-5 failures:", sum(close5_sample$failed_levy == 1, na.rm = TRUE)),
  paste("Close-5 passes:", sum(close5_sample$failed_levy == 0, na.rm = TRUE)),
  paste("Recommended crash sample:", nrow(crash_sample)),
  paste("Recommended downstream sample:", nrow(downstream_sample)),
  paste("Recommended downstream long sample:", nrow(downstream_long_sample)),
  paste("Counties in close-5 sample:", dplyr::n_distinct(close5_sample$county))
)

writeLines(
  summary_lines,
  file.path(paths$output_tables, "renewal_sample_summary.txt")
)

write_etable_txt(
  first_stage$models,
  file.path(paths$output_tables, "renewal_first_stage.txt"),
  "Close-renewal first stage: failure on lagged crash outcomes"
)

write_etable_txt(
  housing_rf$models,
  file.path(paths$output_tables, "renewal_housing_reduced_form.txt"),
  "Close-renewal reduced form: failure on lagged housing outcomes, t+2 to t+4"
)

write_etable_txt(
  housing_long_rf$models,
  file.path(paths$output_tables, "renewal_housing_reduced_form_long.txt"),
  "Close-renewal reduced form: failure on lagged housing outcomes, t+2 to t+5"
)

write_etable_txt(
  employment_rf$models,
  file.path(paths$output_tables, "renewal_employment_reduced_form.txt"),
  "Close-renewal reduced form: failure on lagged employment outcomes, t+2 to t+4"
)

write_etable_txt(
  employment_long_rf$models,
  file.path(paths$output_tables, "renewal_employment_reduced_form_long.txt"),
  "Close-renewal reduced form: failure on lagged employment outcomes, t+2 to t+5"
)

write_etable_txt(
  pretrends$models,
  file.path(paths$output_tables, "renewal_pretrend_placebos.txt"),
  "Close-renewal placebo checks on pre-election outcomes"
)

write_csv(
  results_tbl,
  file.path(paths$output_tables, "renewal_rd_results.csv")
)

message("Wrote renewal sample summary to: ", file.path(paths$output_tables, "renewal_sample_summary.txt"))
message("Wrote first-stage models to: ", file.path(paths$output_tables, "renewal_first_stage.txt"))
message("Wrote housing reduced-form models to: ", file.path(paths$output_tables, "renewal_housing_reduced_form.txt"))
message("Wrote long housing reduced-form models to: ", file.path(paths$output_tables, "renewal_housing_reduced_form_long.txt"))
message("Wrote employment reduced-form models to: ", file.path(paths$output_tables, "renewal_employment_reduced_form.txt"))
message("Wrote long employment reduced-form models to: ", file.path(paths$output_tables, "renewal_employment_reduced_form_long.txt"))
message("Wrote placebo models to: ", file.path(paths$output_tables, "renewal_pretrend_placebos.txt"))
message("Wrote tidy renewal RD results to: ", file.path(paths$output_tables, "renewal_rd_results.csv"))
