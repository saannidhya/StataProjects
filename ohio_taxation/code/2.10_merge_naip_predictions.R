#================================================================================================================#
# Purpose : Merge NAIP satellite image predictions with RDD data to create analysis-ready panel
# Name    : Saani Rawat
# Date    : 2026-02-22
# Input   : naip_preds_convnext.csv, naip_preds_yolo.csv, roads_and_census.dta
# Output  : naip_road_quality_panel_convnext.csv, naip_road_quality_panel_yolo.csv,
#           naip_road_quality_collapsed.csv
# Note    : No original data files are modified.
#================================================================================================================#

library(tidyverse)
library(haven)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root, "/data")
sat_dir <- paste0(data, "/roads/satellite_images")

#==========================================================================================================#
# 1. Load data
#==========================================================================================================#

cutoff <- 50  # RDD cutoff: 50% votes against

# Load the RDD dataset (read-only)
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>% 
  dplyr::select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != "1000") %>%
  janitor::clean_names() %>%
  mutate(votes_pct_against = 100 - votes_pct_for) %>%
  mutate(treated = if_else(votes_pct_against > cutoff, 1, 0))      %>% 
    dplyr::select(tendigit_fips, year, votes_pct_against, treated, pop) %>%
  rename(election_year = year)


# Load prediction CSVs
preds_convnext <- read_csv(paste0(sat_dir, "/naip_preds_convnext.csv"))
preds_yolo <- read_csv(paste0(sat_dir, "/naip_preds_yolo.csv"))

cat("ConvNeXt predictions:", nrow(preds_convnext), "rows\n")
cat("YOLO predictions:", nrow(preds_yolo), "rows\n")
cat("RDD dataset:", nrow(roads_and_census), "rows\n")

#==========================================================================================================#
# 2. Compute road quality scores (both methods, applied to each model)
#==========================================================================================================#

compute_scores <- function(df) {
  df %>%
    mutate(
      # Method A: road_quality_score2 definition
      rq_score_a = case_when(
        pred_id == 0 ~ 1 + (1 - 0.5 * max_prob) * 99 / 3,
        TRUE ~ 1 + pred_id * 99 / 3 + 0.5 * max_prob * 99 / 3
      ) %>% round(2),
      # Method B: expected value using full probability distribution
      rq_score_ev = round(p0 * 0 + p1 * 1 + p2 * 2, 4),
      # Ensure cosbidfp is numeric for merge
      cosbidfp = as.numeric(cosbidfp),
      year = as.integer(year)
    )
}

preds_convnext <- compute_scores(preds_convnext)
preds_yolo <- compute_scores(preds_yolo)

#==========================================================================================================#
# 3. Aggregate to subdivision x NAIP year
#==========================================================================================================#

aggregate_to_panel <- function(df, model_name) {
  df %>%
    group_by(cosbidfp, year) %>%
    summarize(
      n_images = n(),
      mean_pred_id = mean(pred_id, na.rm = TRUE),
      median_pred_id = median(pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(rq_score_a, na.rm = TRUE),
      median_rq_score_a = median(rq_score_a, na.rm = TRUE),
      mean_rq_score_ev = mean(rq_score_ev, na.rm = TRUE),
      median_rq_score_ev = median(rq_score_ev, na.rm = TRUE),
      mean_max_prob = mean(max_prob, na.rm = TRUE),
      mean_p0 = mean(p0, na.rm = TRUE),
      mean_p1 = mean(p1, na.rm = TRUE),
      mean_p2 = mean(p2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model = model_name)
}

panel_convnext <- aggregate_to_panel(preds_convnext, "convnext_v2")
panel_yolo <- aggregate_to_panel(preds_yolo, "yolo11")

cat("ConvNeXt panel:", nrow(panel_convnext), "subdivision-year obs\n")
cat("YOLO panel:", nrow(panel_yolo), "subdivision-year obs\n")

#==========================================================================================================#
# 4. Merge with RDD data
#==========================================================================================================#

merge_with_rdd <- function(panel_df) {
  # Note: inner_join may produce multiple rows per subdivision if a subdivision
  # has multiple elections in roads_and_census. This is intentional -- each
  # election is treated as a separate event with its own pre/post period.
  panel_df %>%
    rename(naip_year = year) %>%
    inner_join(roads_and_census, by = c("cosbidfp" = "tendigit_fips")) %>%
    mutate(
      post_election_flag = as.integer(naip_year > election_year),
      post_delayed_3 = as.integer(naip_year >= election_year + 3),
      post_delayed_5 = as.integer(naip_year >= election_year + 5),
      event_time = naip_year - election_year,
      did = post_election_flag * treated
    ) %>%
    # Keep only subdivisions with at least 1 pre and 1 post-election image
    group_by(cosbidfp, election_year) %>%
    filter(any(post_election_flag == 0) & any(post_election_flag == 1)) %>%
    ungroup() %>%
    arrange(cosbidfp, election_year, naip_year)
}

panel_convnext_rdd <- merge_with_rdd(panel_convnext)
panel_yolo_rdd <- merge_with_rdd(panel_yolo)

cat("ConvNeXt panel (merged, filtered):", nrow(panel_convnext_rdd), "obs\n")
cat("YOLO panel (merged, filtered):", nrow(panel_yolo_rdd), "obs\n")
cat("Unique subdivisions (ConvNeXt):", n_distinct(panel_convnext_rdd$cosbidfp), "\n")

#==========================================================================================================#
# 5. Create collapsed pre/post dataset
#==========================================================================================================#

collapse_pre_post <- function(panel_df, model_name) {
  panel_df %>%
    group_by(cosbidfp, election_year, treated, votes_pct_against, pop, post_election_flag) %>%
    summarize(
      n_images = sum(n_images),
      n_years = n(),
      mean_pred_id = mean(mean_pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(mean_rq_score_a, na.rm = TRUE),
      mean_rq_score_ev = mean(mean_rq_score_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      did = post_election_flag * treated,
      model = model_name
    )
}

# Collapsed dataset for delayed-post specifications (3+ and 5+ years after election)
collapse_delayed <- function(panel_df, model_name, delay_years) {
  delay_col <- paste0("post_delayed_", delay_years)
  panel_df %>%
    group_by(cosbidfp, election_year, treated, votes_pct_against, pop,
             post_delayed = .data[[delay_col]]) %>%
    summarize(
      n_images = sum(n_images),
      n_years = n(),
      mean_pred_id = mean(mean_pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(mean_rq_score_a, na.rm = TRUE),
      mean_rq_score_ev = mean(mean_rq_score_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      did = post_delayed * treated,
      model = model_name,
      delay = delay_years
    )
}

collapsed_convnext <- collapse_pre_post(panel_convnext_rdd, "convnext_v2")
collapsed_yolo <- collapse_pre_post(panel_yolo_rdd, "yolo11")
collapsed_all <- bind_rows(collapsed_convnext, collapsed_yolo)

# Delayed-post collapsed datasets
collapsed_delayed_all <- bind_rows(
  collapse_delayed(panel_convnext_rdd, "convnext_v2", 3),
  collapse_delayed(panel_convnext_rdd, "convnext_v2", 5),
  collapse_delayed(panel_yolo_rdd, "yolo11", 3),
  collapse_delayed(panel_yolo_rdd, "yolo11", 5)
)

#==========================================================================================================#
# 6. Save outputs
#==========================================================================================================#

write_csv(panel_convnext_rdd, paste0(sat_dir, "/naip_road_quality_panel_convnext.csv"))
write_csv(panel_yolo_rdd, paste0(sat_dir, "/naip_road_quality_panel_yolo.csv"))
write_csv(collapsed_all, paste0(sat_dir, "/naip_road_quality_collapsed.csv"))
write_csv(collapsed_delayed_all, paste0(sat_dir, "/naip_road_quality_collapsed_delayed.csv"))

cat("\nOutputs saved to", sat_dir, ":\n")
cat("  naip_road_quality_panel_convnext.csv\n")
cat("  naip_road_quality_panel_yolo.csv\n")
cat("  naip_road_quality_collapsed.csv\n")
cat("  naip_road_quality_collapsed_delayed.csv\n")
