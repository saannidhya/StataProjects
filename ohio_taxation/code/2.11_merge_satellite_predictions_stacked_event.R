#================================================================================================================#
# Purpose : Harmonize NAIP and OSIP3 road-quality predictions into stacked-event RD datasets
# Name    : Saani Rawat
# Date    : 2026-03-09
# Input   : NAIP / OSIP3 prediction CSVs, roads_and_census.dta
# Output  : Harmonized image-level, subdivision-year, event-time, and main-window event datasets
# Note    : The script does not modify any source prediction files.
#================================================================================================================#

library(tidyverse)
library(haven)
library(janitor)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- file.path(root, "data")
roads_dir <- file.path(data, "roads")
naip_dir <- file.path(roads_dir, "satellite_images")
osip_dir <- file.path(roads_dir, "satellite_images_osip3")
out_dir <- file.path(roads_dir, "stacked_event_road_quality")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

MAIN_PRE_WINDOW <- c(-3L, -1L)
MAIN_POST_WINDOW <- c(1L, 3L)
DYNAMIC_EVENT_TIMES <- c(-3L, -2L, -1L, 1L, 2L, 3L)

replace_nan_with_na <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

score_predictions <- function(df) {
  df %>%
    mutate(
      cosbidfp = as.numeric(cosbidfp),
      year = as.integer(year),
      pred_id = as.numeric(pred_id),
      max_prob = as.numeric(max_prob),
      p0 = as.numeric(p0),
      p1 = as.numeric(p1),
      p2 = as.numeric(p2),
      ev_score = p1 + 2 * p2,
      rq_score_a = case_when(
        pred_id == 0 ~ 1 + (1 - 0.5 * max_prob) * 99 / 3,
        TRUE ~ 1 + pred_id * 99 / 3 + 0.5 * max_prob * 99 / 3
      ),
      product_group = case_when(
        source == "NAIP" ~ "NAIP",
        product %in% c("S6IN", "E6IN", "E3IN") ~ product,
        product %in% c("E1FT", "E1_5IN") ~ "HIRES_OTHER",
        TRUE ~ "OTHER"
      )
    )
}

standardize_scores <- function(df) {
  df %>%
    group_by(model, source, year, product_group) %>%
    mutate(
      cell_mean_ev = mean(ev_score, na.rm = TRUE),
      cell_sd_ev = sd(ev_score, na.rm = TRUE),
      z_ev = ifelse(is.na(cell_sd_ev) | cell_sd_ev <= 0, 0, (ev_score - cell_mean_ev) / cell_sd_ev)
    ) %>%
    ungroup()
}

load_prediction_file <- function(path, model_name, source_name) {
  if (!file.exists(path)) {
    stop("Prediction file not found: ", path)
  }

  df <- readr::read_csv(path, show_col_types = FALSE)

  if (!"product" %in% names(df)) {
    df$product <- ifelse(source_name == "NAIP", "NAIP", NA_character_)
  }
  if (!"namelsad" %in% names(df)) {
    df$namelsad <- NA_character_
  }
  if (!"folder" %in% names(df)) {
    df$folder <- NA_character_
  }
  if (!"tile" %in% names(df)) {
    df$tile <- NA_character_
  }
  if (!"sha256" %in% names(df)) {
    df$sha256 <- NA_character_
  }
  if (!"source_url" %in% names(df)) {
    df$source_url <- NA_character_
  }

  df %>%
    mutate(
      model = model_name,
      source = source_name
    ) %>%
    select(
      model, source, filename, cosbidfp, year, lat, lon, roadname, namelsad,
      product, folder, tile, sha256, pred_id, pred_label, max_prob, p0, p1, p2, source_url
    )
}

annualize_predictions <- function(df) {
  df %>%
    group_by(model, cosbidfp, year) %>%
    summarize(
      z_ev = mean(z_ev, na.rm = TRUE),
      ev_score = mean(ev_score, na.rm = TRUE),
      mean_pred_id = mean(pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(rq_score_a, na.rm = TRUE),
      n_images = n(),
      n_naip_images = sum(source == "NAIP"),
      n_osip3_images = sum(source == "OSIP3"),
      share_naip = mean(source == "NAIP"),
      share_osip3 = mean(source == "OSIP3"),
      share_s6in = mean(product_group == "S6IN"),
      share_e6in = mean(product_group == "E6IN"),
      share_e3in = mean(product_group == "E3IN"),
      share_hires_other = mean(product_group == "HIRES_OTHER"),
      mean_pred_id_naip = if (any(source == "NAIP")) mean(pred_id[source == "NAIP"], na.rm = TRUE) else NA_real_,
      mean_rq_score_a_naip = if (any(source == "NAIP")) mean(rq_score_a[source == "NAIP"], na.rm = TRUE) else NA_real_,
      n_naip_bridge_images = sum(source == "NAIP"),
      .groups = "drop"
    ) %>%
    replace_nan_with_na()
}

load_election_panel <- function() {
  read_dta(file.path(data, "roads_and_census.dta")) %>%
    janitor::clean_names() %>%
    filter(description == "R", duration != "1000") %>%
    transmute(
      cosbidfp = tendigit_fips,
      election_year = year,
      subdivision = subdivisionname,
      subdivision_type = subdivisiontype,
      county,
      pop,
      votes_pct_against,
      treated = as.integer(votes_pct_against > 50)
    ) %>%
    arrange(cosbidfp, election_year) %>%
    group_by(cosbidfp) %>%
    mutate(
      election_index = row_number(),
      prev_election_year = lag(election_year),
      next_election_year = lead(election_year)
    ) %>%
    ungroup()
}

build_event_time_panel <- function(annual_df, elections_df) {
  annual_df %>%
    inner_join(elections_df, by = "cosbidfp", relationship = "many-to-many") %>%
    mutate(
      event_time = year - election_year,
      within_cycle = year > coalesce(prev_election_year, -Inf) &
        year < coalesce(next_election_year, Inf),
      is_t0 = event_time == 0,
      in_pre_main = within_cycle &
        dplyr::between(event_time, MAIN_PRE_WINDOW[1], MAIN_PRE_WINDOW[2]),
      in_post_main = within_cycle &
        dplyr::between(event_time, MAIN_POST_WINDOW[1], MAIN_POST_WINDOW[2]),
      in_dynamic_window = within_cycle & event_time %in% DYNAMIC_EVENT_TIMES
    ) %>%
    arrange(model, cosbidfp, election_year, year)
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
      pre_window_label = sprintf("t%+d..t%+d", pre_window[1], pre_window[2]),
      post_window_label = sprintf("t%+d..t%+d", post_window[1], post_window[2])
    ) %>%
    replace_nan_with_na()
}

convnext_preds <- load_prediction_file(
  file.path(naip_dir, "naip_preds_convnext.csv"),
  model_name = "convnext_v2",
  source_name = "NAIP"
)
yolo_preds <- load_prediction_file(
  file.path(naip_dir, "naip_preds_yolo.csv"),
  model_name = "yolo11",
  source_name = "NAIP"
)
convnext_osip_preds <- load_prediction_file(
  file.path(osip_dir, "osip3_preds_convnext.csv"),
  model_name = "convnext_v2",
  source_name = "OSIP3"
)
yolo_osip_preds <- load_prediction_file(
  file.path(osip_dir, "osip3_preds_yolo.csv"),
  model_name = "yolo11",
  source_name = "OSIP3"
)

image_predictions <- bind_rows(
  convnext_preds,
  yolo_preds,
  convnext_osip_preds,
  yolo_osip_preds
) %>%
  score_predictions() %>%
  standardize_scores() %>%
  arrange(model, source, cosbidfp, year, filename)

subdivision_year_panel <- annualize_predictions(image_predictions) %>%
  arrange(model, cosbidfp, year)

renewal_elections <- load_election_panel()
max(renewal_elections$election_year)

event_time_panel <- build_event_time_panel(subdivision_year_panel, renewal_elections)
main_event_availability <- build_event_window_dataset(
  event_time_panel,
  pre_window = MAIN_PRE_WINDOW,
  post_window = MAIN_POST_WINDOW
)
main_event_sample <- main_event_availability %>%
  filter(has_pre == 1, has_post == 1)
dynamic_event_sample <- event_time_panel %>%
  filter(in_dynamic_window) %>%
  replace_nan_with_na()

write_csv(image_predictions, file.path(out_dir, "road_quality_image_predictions.csv"))
write_csv(subdivision_year_panel, file.path(out_dir, "road_quality_subdivision_year_panel.csv"))
write_csv(renewal_elections, file.path(out_dir, "road_quality_renewal_elections.csv"))
write_csv(event_time_panel, file.path(out_dir, "road_quality_event_time_panel.csv"))
write_csv(main_event_availability, file.path(out_dir, "road_quality_event_availability.csv"))
write_csv(main_event_sample, file.path(out_dir, "road_quality_event_sample.csv"))
write_csv(dynamic_event_sample, file.path(out_dir, "road_quality_dynamic_event_panel.csv"))

write_csv(
  main_event_sample %>% filter(model == "convnext_v2"),
  file.path(out_dir, "road_quality_event_sample_convnext.csv")
)
write_csv(
  main_event_sample %>% filter(model == "yolo11"),
  file.path(out_dir, "road_quality_event_sample_yolo.csv")
)
write_csv(
  dynamic_event_sample %>% filter(model == "convnext_v2") %>% View(),
  file.path(out_dir, "road_quality_event_time_convnext.csv")
)
write_csv(
  dynamic_event_sample %>% filter(model == "yolo11"),
  file.path(out_dir, "road_quality_event_time_yolo.csv")
)

cat("Harmonized image predictions:", nrow(image_predictions), "rows\n")
cat("Subdivision-year panel:", nrow(subdivision_year_panel), "rows\n")
cat("Renewal elections:", nrow(renewal_elections), "rows\n")
cat("Event-time panel:", nrow(event_time_panel), "rows\n")
cat("Main-window event availability:", nrow(main_event_availability), "rows\n")
cat("Main-window final event sample:", nrow(main_event_sample), "rows\n")
cat("Outputs written to:", out_dir, "\n")
