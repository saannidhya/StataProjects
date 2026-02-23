#================================================================================================================#
# Purpose : Road Quality Analysis using NAIP satellite images + AI vision model predictions
# Name    : Saani Rawat
# Date    : 2026-02-22
# Input   : naip_road_quality_panel_{convnext,yolo}.csv, naip_road_quality_collapsed.csv
# Output  : Tables (LaTeX) and plots (PNG) to data/outputs/{tables,plots}/
#================================================================================================================#

library(fixest)
library(rdrobust)
library(tidyverse)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root, "/data")
sat_dir <- paste0(data, "/roads/satellite_images")
tables <- paste0(data, "/outputs/tables")
plots <- paste0(data, "/outputs/plots")

cutoff <- 50  # RDD cutoff: 50% votes against

# Load panel data
panel_cx <- read_csv(paste0(sat_dir, "/naip_road_quality_panel_convnext.csv"))
panel_yl <- read_csv(paste0(sat_dir, "/naip_road_quality_panel_yolo.csv"))
collapsed <- read_csv(paste0(sat_dir, "/naip_road_quality_collapsed.csv"))

cat("ConvNeXt panel:", nrow(panel_cx), "obs,", n_distinct(panel_cx$cosbidfp), "subdivisions\n")
cat("YOLO panel:", nrow(panel_yl), "obs,", n_distinct(panel_yl$cosbidfp), "subdivisions\n")

#==========================================================================================================#
# 1. Summary Statistics: Treatment x Pre/Post
#==========================================================================================================#

summary_table <- function(df, model_name) {
  df %>%
    group_by(treated, post_election_flag) %>%
    summarize(
      n = n(),
      mean_pred_id = mean(mean_pred_id, na.rm = TRUE),
      sd_pred_id = sd(mean_pred_id, na.rm = TRUE),
      mean_rq_a = mean(mean_rq_score_a, na.rm = TRUE),
      sd_rq_a = sd(mean_rq_score_a, na.rm = TRUE),
      mean_rq_ev = mean(mean_rq_score_ev, na.rm = TRUE),
      sd_rq_ev = sd(mean_rq_score_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model = model_name)
}

summary_cx <- summary_table(panel_cx, "ConvNeXt v2")
summary_yl <- summary_table(panel_yl, "YOLOv11")
print(bind_rows(summary_cx, summary_yl), width = Inf)

#==========================================================================================================#
# 2. Difference-in-Differences (collapsed pre/post)
#==========================================================================================================#

# ConvNeXt v2
collapsed_cx <- collapsed %>% filter(model == "convnext_v2")
collapsed_yl <- collapsed %>% filter(model == "yolo11")

did_cx_pred <- feols(mean_pred_id ~ post_election_flag * treated,
                     data = collapsed_cx, cluster = ~cosbidfp)
did_cx_ev <- feols(mean_rq_score_ev ~ post_election_flag * treated,
                   data = collapsed_cx, cluster = ~cosbidfp)

did_yl_pred <- feols(mean_pred_id ~ post_election_flag * treated,
                     data = collapsed_yl, cluster = ~cosbidfp)
did_yl_ev <- feols(mean_rq_score_ev ~ post_election_flag * treated,
                   data = collapsed_yl, cluster = ~cosbidfp)

# Combined DID table
etable(did_cx_pred, did_cx_ev, did_yl_pred, did_yl_ev,
       headers = c("CX: pred_id", "CX: EV score", "YOLO: pred_id", "YOLO: EV score"),
       tex = TRUE,
       file = paste0(tables, "/naip_road_quality_did.tex"))

etable(did_cx_pred, did_cx_ev, did_yl_pred, did_yl_ev,
       headers = c("CX: pred_id", "CX: EV score", "YOLO: pred_id", "YOLO: EV score"))

#==========================================================================================================#
# 3. Event Study (panel: subdivision x NAIP year)
#==========================================================================================================#

# ConvNeXt v2 event study
es_cx <- feols(mean_rq_score_ev ~ i(event_time, treated, ref = -1) | cosbidfp,
               data = panel_cx, cluster = ~cosbidfp)

png(paste0(plots, "/naip_event_study_road_quality.png"), width = 10, height = 6, units = "in", res = 300)
iplot(es_cx,
      main = "Event Study: Road Quality Around Tax Levy Elections (ConvNeXt v2)",
      xlab = "Years Relative to Election",
      ylab = "Road Quality Score (Expected Value)")
abline(v = 0, lty = 2, col = "gray50")
dev.off()

# YOLO event study
es_yl <- feols(mean_rq_score_ev ~ i(event_time, treated, ref = -1) | cosbidfp,
               data = panel_yl, cluster = ~cosbidfp)

png(paste0(plots, "/naip_event_study_road_quality_yolo.png"), width = 10, height = 6, units = "in", res = 300)
iplot(es_yl,
      main = "Event Study: Road Quality Around Tax Levy Elections (YOLOv11)",
      xlab = "Years Relative to Election",
      ylab = "Road Quality Score (Expected Value)")
abline(v = 0, lty = 2, col = "gray50")
dev.off()

summary(es_cx)
summary(es_yl)

#==========================================================================================================#
# 4. Regression Discontinuity (post-election observations only)
#==========================================================================================================#

panel_cx_post <- panel_cx %>% filter(post_election_flag == 1)
panel_yl_post <- panel_yl %>% filter(post_election_flag == 1)

# ConvNeXt v2 RDD
rd_cx <- rdrobust(
  y = panel_cx_post$mean_rq_score_ev,
  x = panel_cx_post$votes_pct_against,
  c = cutoff,
  covs = panel_cx_post$pop,
  all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2,
  h = max(abs(panel_cx_post$votes_pct_against - cutoff), na.rm = TRUE),
  cluster = panel_cx_post$cosbidfp
)
summary(rd_cx)

# YOLOv11 RDD
rd_yl <- rdrobust(
  y = panel_yl_post$mean_rq_score_ev,
  x = panel_yl_post$votes_pct_against,
  c = cutoff,
  covs = panel_yl_post$pop,
  all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2,
  h = max(abs(panel_yl_post$votes_pct_against - cutoff), na.rm = TRUE),
  cluster = panel_yl_post$cosbidfp
)
summary(rd_yl)

# RD Plot (ConvNeXt v2)
rd_plot_cx <- rdplot(
  y = panel_cx_post$mean_rq_score_ev,
  x = panel_cx_post$votes_pct_against,
  c = cutoff,
  p = 1, kernel = "uniform",
  h = max(abs(panel_cx_post$votes_pct_against - cutoff), na.rm = TRUE),
  nbins = 4, binselect = "esmv",
  title = "Road Quality vs Vote Share Against Tax Renewal (NAIP + ConvNeXt v2)",
  x.label = "Vote Share Against Tax Renewal (%)",
  y.label = "Road Quality Score (Expected Value)"
)
ggsave(paste0(plots, "/naip_rd_plot_road_quality.png"),
       plot = rd_plot_cx$rdplot, width = 10, height = 6, dpi = 300)

# RD Plot (YOLOv11)
rd_plot_yl <- rdplot(
  y = panel_yl_post$mean_rq_score_ev,
  x = panel_yl_post$votes_pct_against,
  c = cutoff,
  p = 1, kernel = "uniform",
  h = max(abs(panel_yl_post$votes_pct_against - cutoff), na.rm = TRUE),
  nbins = 4, binselect = "esmv",
  title = "Road Quality vs Vote Share Against Tax Renewal (NAIP + YOLOv11)",
  x.label = "Vote Share Against Tax Renewal (%)",
  y.label = "Road Quality Score (Expected Value)"
)
ggsave(paste0(plots, "/naip_rd_plot_road_quality_yolo.png"),
       plot = rd_plot_yl$rdplot, width = 10, height = 6, dpi = 300)

#==========================================================================================================#
# 5. Visualization: Line plot and bar plot (matching 3.11 style)
#==========================================================================================================#

# Prepare summary for plotting
plot_summary <- collapsed %>%
  group_by(model, treated, post_election_flag) %>%
  summarize(mean_rq = mean(mean_rq_score_ev, na.rm = TRUE),
            sd_rq = sd(mean_rq_score_ev, na.rm = TRUE),
            n = n(), .groups = "drop")

# Line plot: Both models (faceted)
lp <- ggplot(plot_summary, aes(x = post_election_flag, y = mean_rq, color = factor(treated))) +
  geom_line(aes(group = treated), linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  facet_wrap(~model) +
  scale_color_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                     labels = c("Control", "Treated"), name = "Group") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre-Election", "Post-Election")) +
  labs(x = "Election Period", y = "Mean Road Quality Score (EV)",
       title = "NAIP Road Quality Before and After Elections",
       subtitle = "Treatment vs Control Groups") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"))
ggsave(paste0(plots, "/naip_road_quality_lineplot.png"), plot = lp, width = 12, height = 6, dpi = 300)

# Bar plot: Both models (faceted)
bp <- ggplot(plot_summary, aes(x = factor(treated), y = mean_rq, fill = factor(post_election_flag))) +
  geom_col(position = "dodge", alpha = 0.8) +
  facet_wrap(~model) +
  scale_fill_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                    labels = c("Pre-Election", "Post-Election"), name = "Election Period") +
  scale_x_discrete(labels = c("0" = "Control", "1" = "Treated")) +
  labs(x = "Group", y = "Mean Road Quality Score (EV)",
       title = "NAIP Road Quality by Treatment and Election Period") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(paste0(plots, "/naip_road_quality_barplot.png"), plot = bp, width = 12, height = 6, dpi = 300)

cat("\nAll outputs saved.\n")
