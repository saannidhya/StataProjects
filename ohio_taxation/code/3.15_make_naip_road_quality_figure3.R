#================================================================================================================#
# Purpose : Create JPAM Figure 3 from NAIP ConvNeXt road-quality predictions
# Name    : Saani Rawat
# Date    : 2026-06-13
# Input   : data/roads/stacked_event_road_quality/road_quality_event_time_panel.csv
# Output  : data/outputs/plots/rd_plot_road_quality_naip_t2_t4.png
#           docs/JPAM_draft/images/rd_plot_road_quality_naip_t2_t4.png
# Note    : Uses the Table 5 t+2 to t+4 post-election window and the corresponding
#           RQR effective bandwidth from road_quality_table5_rolling_windows.csv.
#================================================================================================================#

library(tidyverse)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
stacked_dir <- file.path(root, "data", "roads", "stacked_event_road_quality")
plots_dir <- file.path(root, "data", "outputs", "plots")
jpam_img_dir <- file.path(root, "docs", "JPAM_draft", "images")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(jpam_img_dir, recursive = TRUE, showWarnings = FALSE)

cutoff <- 50
pre_window <- c(-3L, -1L)
post_window_vec <- c(2L, 4L)

window_label <- function(window_vec) {
  sprintf("t%+d..t%+d", window_vec[1], window_vec[2])
}

table5_windows <- read_csv(
  file.path(stacked_dir, "road_quality_table5_rolling_windows.csv"),
  show_col_types = FALSE
)

h_rqr <- table5_windows %>%
  filter(label == "Bridge RQR", table5_post_window == window_label(post_window_vec)) %>%
  slice(1) %>%
  pull(h)

if (length(h_rqr) != 1 || is.na(h_rqr)) {
  stop("Could not recover Table 5 RQR bandwidth for post window ", window_label(post_window_vec))
}

event_panel <- read_csv(
  file.path(stacked_dir, "road_quality_event_time_panel.csv"),
  show_col_types = FALSE
) %>%
  filter(model == "convnext_v2")

event_window <- event_panel %>%
  mutate(
    in_pre = within_cycle & between(event_time, pre_window[1], pre_window[2]),
    in_post = within_cycle & between(event_time, post_window_vec[1], post_window_vec[2])
  ) %>%
  group_by(
    model, cosbidfp, election_year, election_index, subdivision, subdivision_type,
    county, votes_pct_against, treated, pop, prev_election_year, next_election_year
  ) %>%
  summarize(
    has_pre = any(in_pre),
    has_post = any(in_post),
    post_rqr_naip = if (any(in_post)) mean(mean_pred_id_naip[in_post], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(has_pre, has_post, !is.na(post_rqr_naip)) %>%
  mutate(running = votes_pct_against - cutoff) %>%
  filter(abs(running) <= h_rqr)

make_bins <- function(df, h, bins_each_side = 4) {
  left_breaks <- seq(-h, 0, length.out = bins_each_side + 1)
  right_breaks <- seq(0, h, length.out = bins_each_side + 1)

  bind_rows(
    df %>%
      filter(running < 0) %>%
      mutate(bin = cut(running, breaks = left_breaks, include.lowest = TRUE, right = FALSE)),
    df %>%
      filter(running >= 0) %>%
      mutate(bin = cut(running, breaks = right_breaks, include.lowest = TRUE, right = TRUE))
  ) %>%
    group_by(bin, treated) %>%
    summarize(
      x = mean(votes_pct_against, na.rm = TRUE),
      y = mean(post_rqr_naip, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

make_lines <- function(df, h) {
  make_side <- function(side_name) {
    side <- if (side_name == "left") {
      df %>% filter(running < 0)
    } else {
      df %>% filter(running >= 0)
    }

    fit <- lm(post_rqr_naip ~ running, data = side)
    xs <- if (side_name == "left") {
      seq(-h, 0, length.out = 50)
    } else {
      seq(0, h, length.out = 50)
    }

    tibble(
      running = xs,
      votes_pct_against = xs + cutoff,
      y = predict(fit, newdata = tibble(running = xs)),
      side = side_name
    )
  }

  bind_rows(make_side("left"), make_side("right"))
}

bins <- make_bins(event_window, h_rqr)
fit_lines <- make_lines(event_window, h_rqr)

figure <- ggplot() +
  geom_vline(xintercept = cutoff, color = "black", linewidth = 0.7) +
  geom_line(data = fit_lines, aes(x = votes_pct_against, y = y), color = "red", linewidth = 0.9) +
  geom_point(data = bins, aes(x = x, y = y), color = "navy", size = 2.4) +
  coord_cartesian(ylim = c(1.1, 2.0)) +
  labs(
    title = "Road Quality vs Vote Share Against Tax Renewal",
    x = "Vote Share Against Tax Renewal (%)",
    y = "Road Quality Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_line(color = "grey92"),
    panel.grid.major = element_line(color = "grey88")
  )

plot_path <- file.path(plots_dir, "rd_plot_road_quality_naip_t2_t4.png")
jpam_path <- file.path(jpam_img_dir, "rd_plot_road_quality_naip_t2_t4.png")

ggsave(plot_path, plot = figure, width = 10, height = 6, dpi = 300)
file.copy(plot_path, jpam_path, overwrite = TRUE)

cat("Figure 3 NAIP sample rows:", nrow(event_window), "\n")
cat("Table 5 RQR bandwidth:", h_rqr, "\n")
cat("Wrote:", plot_path, "\n")
cat("Copied:", jpam_path, "\n")
