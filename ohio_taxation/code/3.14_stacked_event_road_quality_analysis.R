#================================================================================================================#
# Purpose : Stacked-event RD analysis for road quality using NAIP + OSIP3 predictions
# Name    : Saani Rawat
# Date    : 2026-03-09
# Input   : Harmonized stacked-event datasets from 2.11_merge_satellite_predictions_stacked_event.R
# Output  : Replacement Table 5, dynamic RD figure, and appendix tables for window / balance / bandwidth checks
#================================================================================================================#

library(tidyverse)
library(rdrobust)
library(fixest)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- file.path(root, "data")
roads_dir <- file.path(data, "roads")
stacked_dir <- file.path(roads_dir, "stacked_event_road_quality")
tables <- file.path(data, "outputs", "tables")
plots <- file.path(data, "outputs", "plots")

dir.create(tables, recursive = TRUE, showWarnings = FALSE)
dir.create(plots, recursive = TRUE, showWarnings = FALSE)

MAIN_PRE_WINDOW <- c(-3L, -1L)
MAIN_POST_WINDOW <- c(1L, 3L)
BRIDGE_PRE_WINDOW <- c(-3L, -1L)
BRIDGE_POST_WINDOW <- c(2L, 4L)
TABLE5_PRE_WINDOW <- c(-3L, -1L)
TABLE5_POST_WINDOWS <- list(c(1L, 3L), c(2L, 4L), c(3L, 5L), c(4L, 6L), c(5L, 7L))
PRE_WINDOW_GRID <- list(c(-2L, -1L), c(-3L, -1L), c(-4L, -1L))
POST_WINDOW_GRID <- list(c(1L, 2L), c(1L, 3L), c(1L, 4L), c(2L, 4L), c(3L, 5L), c(4L, 6L))
DYNAMIC_EVENT_TIMES <- c(-3L, -2L, -1L, 1L, 2L, 3L)
DONUT_RADIUS <- 1
BANDWIDTH_MULTIPLIERS <- c(0.5, 0.75, 1.0, 1.25, 1.5)
CUTOFF <- 50

replace_nan_with_na <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

window_label <- function(window_vec) {
  sprintf("t%+d..t%+d", window_vec[1], window_vec[2])
}

window_label_tex <- function(window_vec) {
  fmt_endpoint <- function(k) {
    if (k < 0) {
      paste0("$t\\!-\\!", abs(k), "$")
    } else {
      paste0("$t\\!+\\!", k, "$")
    }
  }

  paste(fmt_endpoint(window_vec[1]), "to", fmt_endpoint(window_vec[2]))
}

window_label_tex_stack <- function(window_vec) {
  fmt_endpoint <- function(k) {
    if (k < 0) {
      paste0("$t\\!-\\!", abs(k), "$")
    } else {
      paste0("$t\\!+\\!", k, "$")
    }
  }

  paste0("\\shortstack{", fmt_endpoint(window_vec[1]), "\\\\to ", fmt_endpoint(window_vec[2]), "}")
}

stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

fmt_est <- function(estimate, pval, digits = 3) {
  ifelse(
    is.na(estimate),
    "",
    paste0(formatC(estimate, format = "f", digits = digits), stars(pval))
  )
}

fmt_est_plain <- function(estimate, digits = 3) {
  ifelse(
    is.na(estimate),
    "",
    formatC(estimate, format = "f", digits = digits)
  )
}

fmt_se <- function(se, digits = 3) {
  ifelse(
    is.na(se),
    "",
    paste0("(", formatC(se, format = "f", digits = digits), ")")
  )
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_int <- function(x) {
  ifelse(is.na(x), "", format(round(x), big.mark = ",", scientific = FALSE))
}

build_event_window_dataset <- function(event_panel, pre_window, post_window) {
  event_panel %>%
    mutate(
      in_pre = within_cycle & dplyr::between(event_time, pre_window[1], pre_window[2]),
      in_post = within_cycle & dplyr::between(event_time, post_window[1], post_window[2])
    ) %>%
    group_by(
      model, cosbidfp, election_year, election_index, subdivision, subdivision_type, county,
      votes_pct_against, treated, pop, prev_election_year, next_election_year
    ) %>%
    summarize(
      has_any_image = as.integer(any(within_cycle)),
      has_pre = as.integer(any(in_pre)),
      has_post = as.integer(any(in_post)),
      pre_z_ev = if (any(in_pre)) mean(z_ev[in_pre], na.rm = TRUE) else NA_real_,
      post_z_ev = if (any(in_post)) mean(z_ev[in_post], na.rm = TRUE) else NA_real_,
      delta_z_ev = post_z_ev - pre_z_ev,
      pre_ev_score = if (any(in_pre)) mean(ev_score[in_pre], na.rm = TRUE) else NA_real_,
      post_ev_score = if (any(in_post)) mean(ev_score[in_post], na.rm = TRUE) else NA_real_,
      delta_ev_score = post_ev_score - pre_ev_score,
      pre_rqr_naip = if (any(in_pre)) mean(mean_pred_id_naip[in_pre], na.rm = TRUE) else NA_real_,
      post_rqr_naip = if (any(in_post)) mean(mean_pred_id_naip[in_post], na.rm = TRUE) else NA_real_,
      delta_rqr_naip = post_rqr_naip - pre_rqr_naip,
      pre_rqs_naip = if (any(in_pre)) mean(mean_rq_score_a_naip[in_pre], na.rm = TRUE) else NA_real_,
      post_rqs_naip = if (any(in_post)) mean(mean_rq_score_a_naip[in_post], na.rm = TRUE) else NA_real_,
      delta_rqs_naip = post_rqs_naip - pre_rqs_naip,
      n_pre_years = sum(in_pre),
      n_post_years = sum(in_post),
      n_pre_images = sum(n_images[in_pre], na.rm = TRUE),
      n_post_images = sum(n_images[in_post], na.rm = TRUE),
      n_pre_naip_bridge_images = sum(n_naip_bridge_images[in_pre], na.rm = TRUE),
      n_post_naip_bridge_images = sum(n_naip_bridge_images[in_post], na.rm = TRUE),
      mean_share_naip_pre = if (any(in_pre)) mean(share_naip[in_pre], na.rm = TRUE) else NA_real_,
      mean_share_naip_post = if (any(in_post)) mean(share_naip[in_post], na.rm = TRUE) else NA_real_,
      mean_share_osip3_pre = if (any(in_pre)) mean(share_osip3[in_pre], na.rm = TRUE) else NA_real_,
      mean_share_osip3_post = if (any(in_post)) mean(share_osip3[in_post], na.rm = TRUE) else NA_real_,
      mean_share_s6in_post = if (any(in_post)) mean(share_s6in[in_post], na.rm = TRUE) else NA_real_,
      mean_share_e6in_post = if (any(in_post)) mean(share_e6in[in_post], na.rm = TRUE) else NA_real_,
      mean_share_e3in_post = if (any(in_post)) mean(share_e3in[in_post], na.rm = TRUE) else NA_real_,
      mean_share_hires_other_post = if (any(in_post)) mean(share_hires_other[in_post], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(
      pre_window_label = window_label(pre_window),
      post_window_label = window_label(post_window)
    ) %>%
    replace_nan_with_na()
}

run_rd_result <- function(df, outcome, covariates = character(), label, pre_window_label, post_window_label) {
  df_use <- df %>%
    filter(!is.na(.data[[outcome]]), !is.na(votes_pct_against), !is.na(cosbidfp))

  if (length(covariates) > 0) {
    df_use <- df_use %>% filter(if_all(all_of(covariates), ~ !is.na(.x)))
  }

  empty_stats <- tibble(
    label = label,
    outcome = outcome,
    pre_window = pre_window_label,
    post_window = post_window_label,
    estimate = NA_real_,
    se = NA_real_,
    pval = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    h = NA_real_,
    b = NA_real_,
    n_eff_left = NA_real_,
    n_eff_right = NA_real_,
    n_total = nrow(df_use),
    sample_n = nrow(df_use),
    covariates = paste(covariates, collapse = " + ")
  )

  if (
    nrow(df_use) < 40 ||
    !any(df_use$votes_pct_against < CUTOFF, na.rm = TRUE) ||
    !any(df_use$votes_pct_against > CUTOFF, na.rm = TRUE)
  ) {
    return(list(stats = empty_stats, rd = NULL, data = df_use, outcome = outcome, covariates = covariates))
  }

  covs_mat <- if (length(covariates) > 0) {
    as.matrix(df_use[, covariates, drop = FALSE])
  } else {
    NULL
  }

  rd_fit <- tryCatch(
    rdrobust(
      y = df_use[[outcome]],
      x = df_use$votes_pct_against,
      c = CUTOFF,
      covs = covs_mat,
      all = TRUE,
      kernel = "tri",
      bwselect = "mserd",
      p = 1,
      q = 2,
      cluster = df_use$cosbidfp
    ),
    error = function(e) NULL
  )

  if (is.null(rd_fit)) {
    return(list(stats = empty_stats, rd = NULL, data = df_use, outcome = outcome, covariates = covariates))
  }

  stats <- tibble(
    label = label,
    outcome = outcome,
    pre_window = pre_window_label,
    post_window = post_window_label,
    estimate = as.numeric(rd_fit$coef[1]),
    se = as.numeric(rd_fit$se[3]),
    pval = as.numeric(rd_fit$pv[3]),
    ci_lower = as.numeric(rd_fit$ci[3, 1]),
    ci_upper = as.numeric(rd_fit$ci[3, 2]),
    h = as.numeric(rd_fit$bws[1, 1]),
    b = as.numeric(rd_fit$bws[2, 1]),
    n_eff_left = as.numeric(rd_fit$N_h[1]),
    n_eff_right = as.numeric(rd_fit$N_h[2]),
    n_total = sum(rd_fit$N),
    sample_n = nrow(df_use),
    covariates = paste(covariates, collapse = " + ")
  )

  list(stats = stats, rd = rd_fit, data = df_use, outcome = outcome, covariates = covariates)
}

run_fixed_bandwidth_rd <- function(df, outcome, covariates, h_value, label, pre_window_label, post_window_label) {
  df_use <- df %>%
    filter(!is.na(.data[[outcome]]), !is.na(votes_pct_against), !is.na(cosbidfp))

  if (length(covariates) > 0) {
    df_use <- df_use %>% filter(if_all(all_of(covariates), ~ !is.na(.x)))
  }

  if (
    is.na(h_value) ||
    nrow(df_use) < 40 ||
    !any(df_use$votes_pct_against < CUTOFF, na.rm = TRUE) ||
    !any(df_use$votes_pct_against > CUTOFF, na.rm = TRUE)
  ) {
    return(tibble(
      label = label,
      outcome = outcome,
      pre_window = pre_window_label,
      post_window = post_window_label,
      estimate = NA_real_,
      se = NA_real_,
      pval = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      h = h_value,
      n_eff_left = NA_real_,
      n_eff_right = NA_real_
    ))
  }

  covs_mat <- if (length(covariates) > 0) {
    as.matrix(df_use[, covariates, drop = FALSE])
  } else {
    NULL
  }

  rd_fit <- tryCatch(
    rdrobust(
      y = df_use[[outcome]],
      x = df_use$votes_pct_against,
      c = CUTOFF,
      covs = covs_mat,
      kernel = "tri",
      p = 1,
      q = 2,
      h = h_value,
      all = TRUE,
      cluster = df_use$cosbidfp
    ),
    error = function(e) NULL
  )

  if (is.null(rd_fit)) {
    return(tibble(
      label = label,
      outcome = outcome,
      pre_window = pre_window_label,
      post_window = post_window_label,
      estimate = NA_real_,
      se = NA_real_,
      pval = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      h = h_value,
      n_eff_left = NA_real_,
      n_eff_right = NA_real_
    ))
  }

  tibble(
    label = label,
    outcome = outcome,
    pre_window = pre_window_label,
    post_window = post_window_label,
    estimate = as.numeric(rd_fit$coef[1]),
    se = as.numeric(rd_fit$se[3]),
    pval = as.numeric(rd_fit$pv[3]),
    ci_lower = as.numeric(rd_fit$ci[3, 1]),
    ci_upper = as.numeric(rd_fit$ci[3, 2]),
    h = h_value,
    n_eff_left = as.numeric(rd_fit$N_h[1]),
    n_eff_right = as.numeric(rd_fit$N_h[2])
  )
}

local_linear_check <- function(df, outcome, covariates = character(), rd_fit) {
  if (is.null(rd_fit)) {
    return(tibble(
      check_estimate = NA_real_,
      check_se = NA_real_,
      check_pval = NA_real_,
      control_mean = NA_real_,
      pct_decline = NA_real_,
      check_n = nrow(df)
    ))
  }

  h_value <- as.numeric(rd_fit$bws[1, 1])
  df_use <- df %>%
    filter(!is.na(.data[[outcome]]), !is.na(votes_pct_against), !is.na(cosbidfp))

  if (length(covariates) > 0) {
    df_use <- df_use %>% filter(if_all(all_of(covariates), ~ !is.na(.x)))
  }

  df_use <- df_use %>%
    mutate(
      running = votes_pct_against - CUTOFF,
      treat = as.integer(votes_pct_against > CUTOFF),
      abs_running = abs(running)
    ) %>%
    filter(abs_running <= h_value) %>%
    mutate(weight = pmax(0, 1 - abs_running / h_value))

  if (nrow(df_use) < 30 || !any(df_use$treat == 0) || !any(df_use$treat == 1)) {
    return(tibble(
      check_estimate = NA_real_,
      check_se = NA_real_,
      check_pval = NA_real_,
      control_mean = NA_real_,
      pct_decline = NA_real_,
      check_n = nrow(df_use)
    ))
  }

  weighted_center <- function(x) {
    stats::weighted.mean(x, w = df_use$weight, na.rm = TRUE)
  }

  if (length(covariates) > 0) {
    for (cov_name in covariates) {
      centered_name <- paste0(cov_name, "_c")
      df_use[[centered_name]] <- df_use[[cov_name]] - weighted_center(df_use[[cov_name]])
    }
  }

  rhs_terms <- c("treat", "running", "treat:running", paste0(covariates, "_c"))
  formula_text <- paste(outcome, "~", paste(rhs_terms, collapse = " + "))

  fit <- feols(
    as.formula(formula_text),
    data = df_use,
    weights = ~weight,
    cluster = ~cosbidfp
  )

  coef_table <- summary(fit)$coeftable
  estimate <- as.numeric(stats::coef(fit)[["treat"]])
  se <- as.numeric(coef_table["treat", "Std. Error"])
  pval <- as.numeric(coef_table["treat", "Pr(>|t|)"])
  control_mean <- as.numeric(stats::coef(fit)[["(Intercept)"]])
  pct_decline <- ifelse(
    !is.na(control_mean) && abs(control_mean) > 1e-8 && estimate < 0,
    abs(estimate / control_mean) * 100,
    NA_real_
  )

  tibble(
    check_estimate = estimate,
    check_se = se,
    check_pval = pval,
    control_mean = control_mean,
    pct_decline = pct_decline,
    check_n = nrow(df_use)
  )
}

estimate_window_bundle <- function(model_panel, pre_window, post_window) {
  pre_label <- window_label(pre_window)
  post_label <- window_label(post_window)
  event_all <- build_event_window_dataset(model_panel, pre_window, post_window)
  event_sample <- event_all %>% filter(has_pre == 1, has_post == 1)

  delta_z <- run_rd_result(event_sample, "delta_z_ev", c("pop"), "Main delta z", pre_label, post_label)
  post_z <- run_rd_result(event_sample, "post_z_ev", c("pop", "pre_z_ev"), "Post z with pre control", pre_label, post_label)
  placebo_z <- run_rd_result(event_sample, "pre_z_ev", c("pop"), "Pre z placebo", pre_label, post_label)
  post_rqr <- run_rd_result(event_sample, "post_rqr_naip", c("pop", "pre_rqr_naip"), "Bridge RQR", pre_label, post_label)
  post_rqs <- run_rd_result(event_sample, "post_rqs_naip", c("pop", "pre_rqs_naip"), "Bridge RQS", pre_label, post_label)
  delta_rqr <- run_rd_result(event_sample, "delta_rqr_naip", c("pop"), "Delta RQR", pre_label, post_label)
  delta_rqs <- run_rd_result(event_sample, "delta_rqs_naip", c("pop"), "Delta RQS", pre_label, post_label)

  post_rqr_check <- local_linear_check(post_rqr$data, "post_rqr_naip", c("pop", "pre_rqr_naip"), post_rqr$rd)
  post_rqs_check <- local_linear_check(post_rqs$data, "post_rqs_naip", c("pop", "pre_rqs_naip"), post_rqs$rd)

  stats_rows <- bind_rows(
    delta_z$stats,
    post_z$stats,
    placebo_z$stats,
    post_rqr$stats %>% bind_cols(post_rqr_check),
    post_rqs$stats %>% bind_cols(post_rqs_check),
    delta_rqr$stats,
    delta_rqs$stats
  ) %>%
    mutate(
      final_event_sample = nrow(event_sample),
      total_event_rows = nrow(event_all)
    )

  list(
    event_all = event_all,
    event_sample = event_sample,
    stats = stats_rows,
    rd_objects = list(
      delta_z = delta_z,
      post_z = post_z,
      placebo_z = placebo_z,
      post_rqr = post_rqr,
      post_rqs = post_rqs,
      delta_rqr = delta_rqr,
      delta_rqs = delta_rqs
    )
  )
}

event_panel <- read_csv(file.path(stacked_dir, "road_quality_event_time_panel.csv"), show_col_types = FALSE)
renewal_elections <- read_csv(file.path(stacked_dir, "road_quality_renewal_elections.csv"), show_col_types = FALSE)
main_event_availability <- read_csv(file.path(stacked_dir, "road_quality_event_availability.csv"), show_col_types = FALSE)

conv_panel <- event_panel %>% filter(model == "convnext_v2")
yolo_panel <- event_panel %>% filter(model == "yolo11")

conv_main_bundle <- estimate_window_bundle(conv_panel, MAIN_PRE_WINDOW, MAIN_POST_WINDOW)
yolo_main_bundle <- estimate_window_bundle(yolo_panel, MAIN_PRE_WINDOW, MAIN_POST_WINDOW)

conv_main_event_all <- conv_main_bundle$event_all
conv_main_event_sample <- conv_main_bundle$event_sample
conv_main_stats <- conv_main_bundle$stats

main_delta_row <- conv_main_stats %>% filter(label == "Main delta z") %>% slice(1)
main_post_row <- conv_main_stats %>% filter(label == "Post z with pre control") %>% slice(1)
main_placebo_row <- conv_main_stats %>% filter(label == "Pre z placebo") %>% slice(1)

window_grid_results <- purrr::map_dfr(PRE_WINDOW_GRID, function(pre_window) {
  purrr::map_dfr(POST_WINDOW_GRID, function(post_window) {
    estimate_window_bundle(conv_panel, pre_window, post_window)$stats
  })
})

window_grid_summary <- window_grid_results %>%
  filter(label %in% c("Main delta z", "Post z with pre control", "Bridge RQR", "Bridge RQS")) %>%
  select(
    pre_window, post_window, label, estimate, se, pval, h, n_eff_left, n_eff_right,
    final_event_sample, sample_n, pct_decline
  )

bridge_choice <- tibble(
  pre_window = window_label(BRIDGE_PRE_WINDOW),
  post_window = window_label(BRIDGE_POST_WINDOW)
)
bridge_pre_window <- BRIDGE_PRE_WINDOW
bridge_post_window <- BRIDGE_POST_WINDOW
bridge_bundle <- estimate_window_bundle(conv_panel, bridge_pre_window, bridge_post_window)
bridge_stats <- bridge_bundle$stats
bridge_rqr_row <- bridge_stats %>% filter(label == "Bridge RQR") %>% slice(1)
bridge_rqs_row <- bridge_stats %>% filter(label == "Bridge RQS") %>% slice(1)

yolo_bridge_bundle <- estimate_window_bundle(yolo_panel, bridge_pre_window, bridge_post_window)
yolo_appendix_rows <- bind_rows(
  yolo_main_bundle$stats %>% filter(label %in% c("Main delta z", "Post z with pre control", "Pre z placebo")),
  yolo_bridge_bundle$stats %>% filter(label %in% c("Bridge RQR", "Bridge RQS"))
)

table5_rolling_results <- purrr::map_dfr(TABLE5_POST_WINDOWS, function(post_window) {
  post_window_label_current <- window_label(post_window)
  estimate_window_bundle(conv_panel, TABLE5_PRE_WINDOW, post_window)$stats %>%
    filter(label %in% c("Bridge RQR", "Bridge RQS")) %>%
    mutate(
      table5_pre_window = window_label(TABLE5_PRE_WINDOW),
      table5_post_window = post_window_label_current
    )
})

table5_placebo_dataset <- build_event_window_dataset(conv_panel, TABLE5_PRE_WINDOW, TABLE5_POST_WINDOWS[[1]])
table5_placebo_results <- bind_rows(
  run_rd_result(
    table5_placebo_dataset,
    outcome = "pre_rqr_naip",
    covariates = c("pop"),
    label = "RQR placebo",
    pre_window_label = window_label(TABLE5_PRE_WINDOW),
    post_window_label = ""
  )$stats,
  run_rd_result(
    table5_placebo_dataset,
    outcome = "pre_rqs_naip",
    covariates = c("pop"),
    label = "RQS placebo",
    pre_window_label = window_label(TABLE5_PRE_WINDOW),
    post_window_label = ""
  )$stats
)

conv_availability_full <- renewal_elections %>%
  left_join(
    main_event_availability %>%
      filter(model == "convnext_v2") %>%
      select(
        cosbidfp, election_year, has_any_image, has_pre, has_post, n_pre_years, n_post_years,
        n_pre_images, n_post_images, mean_share_osip3_pre, mean_share_osip3_post,
        mean_share_hires_other_post
      ),
    by = c("cosbidfp", "election_year")
  ) %>%
  mutate(
    across(c(has_any_image, has_pre, has_post, n_pre_years, n_post_years, n_pre_images, n_post_images), ~ replace_na(.x, 0)),
    across(c(mean_share_osip3_pre, mean_share_osip3_post, mean_share_hires_other_post), ~ replace_na(.x, 0))
  )

sample_flow <- tibble(
  stage = c(
    "All renewal elections",
    "With any matched imagery in levy cycle",
    "With usable pre years in main window",
    "With usable post years in main window",
    "Final stacked-event sample (pre and post)"
  ),
  n = c(
    nrow(renewal_elections),
    sum(conv_availability_full$has_any_image == 1, na.rm = TRUE),
    sum(conv_availability_full$has_pre == 1, na.rm = TRUE),
    sum(conv_availability_full$has_post == 1, na.rm = TRUE),
    nrow(conv_main_event_sample)
  )
)

availability_outcomes <- tribble(
  ~label, ~outcome,
  "Has usable pre data", "has_pre",
  "Has usable post data", "has_post",
  "Number of pre years", "n_pre_years",
  "Number of post years", "n_post_years",
  "Number of pre images", "n_pre_images",
  "Number of post images", "n_post_images",
  "OSIP3 share in pre window", "mean_share_osip3_pre",
  "OSIP3 share in post window", "mean_share_osip3_post",
  "High-res OSIP share in post window", "mean_share_hires_other_post"
)

balance_checks <- purrr::pmap_dfr(
  availability_outcomes,
  function(label, outcome) {
    run_rd_result(
      conv_availability_full,
      outcome = outcome,
      covariates = character(),
      label = label,
      pre_window_label = window_label(MAIN_PRE_WINDOW),
      post_window_label = window_label(MAIN_POST_WINDOW)
    )$stats
  }
)

dynamic_results <- purrr::map_dfr(DYNAMIC_EVENT_TIMES, function(k) {
  panel_k <- conv_panel %>% filter(within_cycle, event_time == k)
  rd_k <- run_rd_result(
    panel_k,
    outcome = "z_ev",
    covariates = c("pop"),
    label = sprintf("Event time %s", ifelse(k > 0, paste0("+", k), as.character(k))),
    pre_window_label = "",
    post_window_label = ""
  )$stats
  rd_k %>% mutate(event_time = k)
})

dynamic_plot <- ggplot(dynamic_results, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray60") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, color = "#2c3e50") +
  geom_point(size = 3, color = "#b03a2e") +
  scale_x_continuous(breaks = DYNAMIC_EVENT_TIMES) +
  labs(
    title = "Dynamic RD Estimates for Predicted Road Quality",
    subtitle = "ConvNeXt pooled standardized score; t+0 omitted",
    x = "Years relative to renewal levy election",
    y = "RD estimate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(
  file.path(plots, "road_quality_dynamic_rd.png"),
  plot = dynamic_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

main_h <- main_delta_row$h[[1]]
bandwidth_grid <- purrr::map_dfr(BANDWIDTH_MULTIPLIERS, function(multiplier) {
  run_fixed_bandwidth_rd(
    conv_main_event_sample,
    outcome = "delta_z_ev",
    covariates = c("pop"),
    h_value = main_h * multiplier,
    label = paste0("Main delta z @ ", multiplier, "h"),
    pre_window_label = window_label(MAIN_PRE_WINDOW),
    post_window_label = window_label(MAIN_POST_WINDOW)
  ) %>%
    mutate(multiplier = multiplier)
})

donut_rows <- bind_rows(
  run_rd_result(
    conv_main_event_sample %>% filter(abs(votes_pct_against - CUTOFF) >= DONUT_RADIUS),
    outcome = "delta_z_ev",
    covariates = c("pop"),
    label = "Main delta z donut",
    pre_window_label = window_label(MAIN_PRE_WINDOW),
    post_window_label = window_label(MAIN_POST_WINDOW)
  )$stats,
  run_rd_result(
    bridge_bundle$event_sample %>% filter(abs(votes_pct_against - CUTOFF) >= DONUT_RADIUS),
    outcome = "post_rqr_naip",
    covariates = c("pop", "pre_rqr_naip"),
    label = "Bridge RQR donut",
    pre_window_label = bridge_choice$pre_window,
    post_window_label = bridge_choice$post_window
  )$stats,
  run_rd_result(
    bridge_bundle$event_sample %>% filter(abs(votes_pct_against - CUTOFF) >= DONUT_RADIUS),
    outcome = "post_rqs_naip",
    covariates = c("pop", "pre_rqs_naip"),
    label = "Bridge RQS donut",
    pre_window_label = bridge_choice$pre_window,
    post_window_label = bridge_choice$post_window
  )$stats
)

write_csv(conv_main_event_sample, file.path(stacked_dir, "road_quality_event_sample_convnext.csv"))
write_csv(yolo_main_bundle$event_sample, file.path(stacked_dir, "road_quality_event_sample_yolo.csv"))
write_csv(dynamic_results, file.path(stacked_dir, "road_quality_dynamic_rd.csv"))
write_csv(window_grid_results, file.path(stacked_dir, "road_quality_window_grid.csv"))
write_csv(balance_checks, file.path(stacked_dir, "road_quality_balance_checks.csv"))
write_csv(sample_flow, file.path(stacked_dir, "road_quality_sample_flow.csv"))
write_csv(bandwidth_grid, file.path(stacked_dir, "road_quality_bandwidth_grid.csv"))
write_csv(donut_rows, file.path(stacked_dir, "road_quality_donut_checks.csv"))
write_csv(yolo_appendix_rows, file.path(stacked_dir, "road_quality_yolo_appendix.csv"))
write_csv(table5_rolling_results, file.path(stacked_dir, "road_quality_table5_rolling_windows.csv"))
write_csv(table5_placebo_results, file.path(stacked_dir, "road_quality_table5_placebo.csv"))

table5_rows <- purrr::map(TABLE5_POST_WINDOWS, function(post_window) {
  post_label <- window_label(post_window)
  window_stats <- table5_rolling_results %>% filter(table5_post_window == post_label)
  list(
    post_label = post_label,
    rqr = window_stats %>% filter(label == "Bridge RQR") %>% slice(1),
    rqs = window_stats %>% filter(label == "Bridge RQS") %>% slice(1)
  )
})

table5_columns <- c(
  list(
    list(
      window_tex = window_label_tex_stack(TABLE5_PRE_WINDOW),
      rqr = table5_placebo_results %>% filter(label == "RQR placebo") %>% slice(1),
      rqs = table5_placebo_results %>% filter(label == "RQS placebo") %>% slice(1)
    )
  ),
  purrr::map(TABLE5_POST_WINDOWS, function(post_window) {
    post_label <- window_label(post_window)
    window_stats <- table5_rolling_results %>% filter(table5_post_window == post_label)
    list(
      window_tex = window_label_tex_stack(post_window),
      rqr = window_stats %>% filter(label == "Bridge RQR") %>% slice(1),
      rqs = window_stats %>% filter(label == "Bridge RQS") %>% slice(1)
    )
  })
)

table5_row_values <- function(table5_cols, outcome_key, value_fn) {
  paste(
    purrr::map_chr(table5_cols, function(col) value_fn(col[[outcome_key]])),
    collapse = " & "
  )
}

table5_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{Road Quality after Failed Renewal Elections}",
  "    \\label{tab:roadquality_estimates}",
  "    \\begin{threeparttable}",
  "        \\small",
  "        \\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}p{3.7cm}cccccc}",
  "            \\toprule",
  "            & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  paste0(
    "            & ",
    paste(purrr::map_chr(table5_columns, "window_tex"), collapse = " & "),
    " \\\\"
  ),
  "            \\midrule",
  "            \\multicolumn{7}{l}{\\textbf{Panel A: Road Quality Rating}} \\\\",
  "            \\midrule",
  paste0(
    "            RQR & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_est_plain(row$estimate)),
    " \\\\"
  ),
  paste0(
    "            & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_se(row$se)),
    " \\\\"
  ),
  paste0(
    "            Eff. bandwidth ($h$), RQR & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_num(row$h)),
    " \\\\"
  ),
  paste0(
    "            Bias bandwidth ($b$), RQR & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_num(row$b)),
    " \\\\"
  ),
  paste0(
    "            Eff. observations, RQR & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_int(row$n_eff_left + row$n_eff_right)),
    " \\\\"
  ),
  paste0(
    "            Total observations, RQR & ",
    table5_row_values(table5_columns, "rqr", function(row) fmt_int(row$n_total)),
    " \\\\"
  ),
  "            \\midrule",
  "            \\multicolumn{7}{l}{\\textbf{Panel B: Road Quality Score}} \\\\",
  "            \\midrule",
  paste0(
    "            RQS & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_est_plain(row$estimate)),
    " \\\\"
  ),
  paste0(
    "            & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_se(row$se)),
    " \\\\"
  ),
  paste0(
    "            Eff. bandwidth ($h$), RQS & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_num(row$h)),
    " \\\\"
  ),
  paste0(
    "            Bias bandwidth ($b$), RQS & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_num(row$b)),
    " \\\\"
  ),
  paste0(
    "            Eff. observations, RQS & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_int(row$n_eff_left + row$n_eff_right)),
    " \\\\"
  ),
  paste0(
    "            Total observations, RQS & ",
    table5_row_values(table5_columns, "rqs", function(row) fmt_int(row$n_total)),
    " \\\\"
  ),
  "            \\bottomrule",
  "        \\end{tabular*}",
  "        \\begin{tablenotes}[flushleft]",
  "        \\small",
  paste0(
    "        \\item \\textit{Notes:} Entries are bias-corrected sharp RD estimates at the 50\\% vote-share cutoff. ",
    "Column (1) is a placebo RD using average pre-election road quality over ", window_label_tex(TABLE5_PRE_WINDOW), ". ",
    "Columns (2)--(6) use rolling 3-year post-election windows, exclude $t+0$, and control for population and the corresponding pre-election outcome. ",
    "Standard errors clustered by ten-digit FIPS code are in parentheses."
  ),
  "        \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{table}"
)
writeLines(table5_lines, file.path(tables, "road_quality_rd_table5.tex"))

grid_tex_rows <- window_grid_summary %>%
  mutate(
    label_key = recode(
      label,
      "Main delta z" = "main_delta_z",
      "Post z with pre control" = "post_z",
      "Bridge RQR" = "bridge_rqr",
      "Bridge RQS" = "bridge_rqs"
    ),
    estimate_str = paste0(fmt_est(estimate, pval), " ", fmt_se(se)),
    pct_str = if_else(is.na(pct_decline), "", paste0(" [", fmt_num(pct_decline), "\\%]"))
  ) %>%
  select(pre_window, post_window, label_key, estimate_str, pct_str) %>%
  pivot_wider(
    names_from = label_key,
    values_from = c(estimate_str, pct_str),
    values_fill = ""
  )

grid_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{Window Grid for Stacked-Event Road Quality RD}",
  "    \\label{tab:roadquality_window_grid}",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{llcccc}",
  "        \\toprule",
  "        Pre window & Post window & Main $\\Delta z_{ev}$ & Post $z_{ev}$ & Bridge RQR & Bridge RQS \\\\",
  "        \\midrule",
  purrr::pmap_chr(
    grid_tex_rows,
    function(pre_window, post_window, estimate_str_main_delta_z, pct_str_main_delta_z,
             estimate_str_post_z, pct_str_post_z,
             estimate_str_bridge_rqr, pct_str_bridge_rqr,
             estimate_str_bridge_rqs, pct_str_bridge_rqs) {
      paste0(
        "        ", pre_window, " & ", post_window, " & ",
        estimate_str_main_delta_z, " & ",
        estimate_str_post_z, " & ",
        estimate_str_bridge_rqr, pct_str_bridge_rqr, " & ",
        estimate_str_bridge_rqs, pct_str_bridge_rqs, " \\\\"
      )
    }
  ),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "    \\small",
  "    \\item \\textit{Notes:} Each cell reports the RD estimate with robust standard error in parentheses. Bracketed entries for the bridge outcomes report the implied percentage decline relative to the pass-side local linear counterfactual at the cutoff.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{table}"
)
writeLines(grid_lines, file.path(tables, "road_quality_window_grid.tex"))

balance_checks_print <- balance_checks %>%
  filter(!is.na(estimate))

balance_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{Continuity Checks for Road-Quality Data Availability}",
  "    \\label{tab:roadquality_balance_checks}",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{lcccc}",
  "        \\toprule",
  "        Outcome & RD estimate & Robust SE & $p$-value & Eff.\\ Obs (L/R) \\\\",
  "        \\midrule",
  purrr::pmap_chr(
    balance_checks_print %>% select(label, estimate, se, pval, n_eff_left, n_eff_right),
    function(label, estimate, se, pval, n_eff_left, n_eff_right) {
      paste0(
        "        ", label, " & ",
        fmt_est(estimate, pval), " & ",
        fmt_se(se), " & ",
        fmt_num(pval, 3), " & ",
        fmt_int(n_eff_left), " / ", fmt_int(n_eff_right), " \\\\"
      )
    }
  ),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "    \\small",
  "    \\item \\textit{Notes:} Outcomes are measured for the main stacked-event window with pre period t-3 to t-1 and post period t+1 to t+3. Estimates use the same sharp RD design as the main analysis, without additional covariates.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{table}"
)
writeLines(balance_lines, file.path(tables, "road_quality_balance_checks.tex"))

sample_flow_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{Sample Flow for the Stacked-Event Road Quality RD}",
  "    \\label{tab:roadquality_sample_flow}",
  "    \\begin{tabular}{lc}",
  "        \\toprule",
  "        Stage & Elections \\\\",
  "        \\midrule",
  purrr::pmap_chr(sample_flow, function(stage, n) {
    paste0("        ", stage, " & ", fmt_int(n), " \\\\")
  }),
  "        \\bottomrule",
  "    \\end{tabular}",
  "\\end{table}"
)
writeLines(sample_flow_lines, file.path(tables, "road_quality_sample_flow.tex"))

bandwidth_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{Bandwidth Sensitivity for the Main Stacked-Event Change RD}",
  "    \\label{tab:roadquality_bw_sensitivity}",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{cccccc}",
  "        \\toprule",
  "        Multiplier & Bandwidth ($h$) & RD estimate & Robust SE & $p$-value & Eff.\\ Obs (L/R) \\\\",
  "        \\midrule",
  purrr::pmap_chr(
    bandwidth_grid %>% select(multiplier, h, estimate, se, pval, n_eff_left, n_eff_right),
    function(multiplier, h, estimate, se, pval, n_eff_left, n_eff_right) {
      paste0(
        "        ", multiplier, "h & ",
        fmt_num(h), " & ",
        fmt_est(estimate, pval), " & ",
        fmt_se(se), " & ",
        fmt_num(pval, 3), " & ",
        fmt_int(n_eff_left), " / ", fmt_int(n_eff_right), " \\\\"
      )
    }
  ),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "    \\small",
  "    \\item \\textit{Notes:} The outcome is the main pooled standardized change in predicted road quality. All specifications use the main stacked-event sample and fixed bandwidths equal to the reported multiple of the MSE-optimal bandwidth.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{table}"
)
writeLines(bandwidth_lines, file.path(tables, "road_quality_bw_sensitivity.tex"))

yolo_lines <- c(
  "\\begin{table}[ht]",
  "    \\centering",
  "    \\caption{YOLO Appendix: Stacked-Event Road Quality RD}",
  "    \\label{tab:roadquality_yolo_appendix}",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{lcccc}",
  "        \\toprule",
  "        Outcome & RD estimate & Robust SE & $p$-value & Window \\\\",
  "        \\midrule",
  purrr::pmap_chr(
    yolo_appendix_rows %>% select(label, estimate, se, pval, pre_window, post_window),
    function(label, estimate, se, pval, pre_window, post_window) {
      window_text <- ifelse(post_window == "", pre_window, paste0(pre_window, " / ", post_window))
      paste0(
        "        ", label, " & ",
        fmt_est(estimate, pval), " & ",
        fmt_se(se), " & ",
        fmt_num(pval, 3), " & ",
        window_text, " \\\\"
      )
    }
  ),
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "    \\small",
  "    \\item \\textit{Notes:} YOLO estimates use the identical stacked-event samples and windows as the ConvNeXt main and bridge specifications.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{table}"
)
writeLines(yolo_lines, file.path(tables, "road_quality_yolo_appendix.tex"))

cat("Main ConvNeXt event sample:", nrow(conv_main_event_sample), "events\n")
cat("Bridge window fixed at:", bridge_choice$pre_window, "to", bridge_choice$post_window, "\n")
cat("Replacement Table 5 written to:", file.path(tables, "road_quality_rd_table5.tex"), "\n")
cat("Dynamic RD plot written to:", file.path(plots, "road_quality_dynamic_rd.png"), "\n")
