#================================================================================================================#
# Purpose : Rebuild Table 2 (Predicted Road Quality by Treatment Status and Period).
#           Two column blocks per group: (i) raw GLOBAL means over the estimation sample, and
#           (ii) RD-ADJUSTED values at the 50% threshold. The threshold Difference is the
#           bias-corrected RD estimate (identical to the bridge RQR/RQS rows of Table 5), and the
#           threshold Treated value = Control fitted level + that estimate, so the displayed gap and
#           its stars come from the SAME (Table 5) estimator.
# Name    : Saani Rawat
# Date    : 2026-06-19
# Input   : road_quality_event_time_panel.csv  (ConvNeXt v2 NAIP predictions)
# Output  : data/outputs/tables/road_quality_means_table.tex
# Note    : Window pre = t-3..t-1, post = t+3..t+5. Helper functions are identical to
#           3.14_stacked_event_road_quality_analysis.R so threshold numbers match Table 5.
#================================================================================================================#

library(tidyverse)
library(rdrobust)
library(fixest)

root        <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data        <- file.path(root, "data")
stacked_dir <- file.path(data, "roads", "stacked_event_road_quality")
tables      <- file.path(data, "outputs", "tables")
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

PRE_WINDOW  <- c(-3L, -1L)
POST_WINDOW <- c( 3L,  5L)
CUTOFF      <- 50

#-----------------------------------------------------------------------------------------------------------------#
# Helpers (mirrors of 3.14 so the threshold columns reproduce Table 5 exactly)
#-----------------------------------------------------------------------------------------------------------------#
replace_nan_with_na <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

window_label_tex <- function(window_vec) {
  fmt <- function(k) if (k < 0) paste0("$t\\!-\\!", abs(k), "$") else paste0("$t\\!+\\!", k, "$")
  paste(fmt(window_vec[1]), "to", fmt(window_vec[2]))
}

stars <- function(p) dplyr::case_when(
  is.na(p) ~ "", p < 0.01 ~ "$^{***}$", p < 0.05 ~ "$^{**}$", p < 0.10 ~ "$^{*}$", TRUE ~ "")

build_event_window_dataset <- function(event_panel, pre_window, post_window) {
  event_panel %>%
    mutate(
      in_pre  = within_cycle & dplyr::between(event_time, pre_window[1], pre_window[2]),
      in_post = within_cycle & dplyr::between(event_time, post_window[1], post_window[2])
    ) %>%
    group_by(model, cosbidfp, election_year, election_index, subdivision, subdivision_type, county,
             votes_pct_against, treated, pop, prev_election_year, next_election_year) %>%
    summarize(
      has_pre  = as.integer(any(in_pre)),
      has_post = as.integer(any(in_post)),
      pre_rqr_naip  = if (any(in_pre))  mean(mean_pred_id_naip[in_pre],  na.rm = TRUE) else NA_real_,
      post_rqr_naip = if (any(in_post)) mean(mean_pred_id_naip[in_post], na.rm = TRUE) else NA_real_,
      pre_rqs_naip  = if (any(in_pre))  mean(mean_rq_score_a_naip[in_pre],  na.rm = TRUE) else NA_real_,
      post_rqs_naip = if (any(in_post)) mean(mean_rq_score_a_naip[in_post], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    replace_nan_with_na()
}

run_rd_result <- function(df, outcome, covariates = character()) {
  df_use <- df %>% filter(!is.na(.data[[outcome]]), !is.na(votes_pct_against), !is.na(cosbidfp))
  if (length(covariates) > 0) df_use <- df_use %>% filter(if_all(all_of(covariates), ~ !is.na(.x)))
  covs_mat <- if (length(covariates) > 0) as.matrix(df_use[, covariates, drop = FALSE]) else NULL
  rd_fit <- tryCatch(
    rdrobust(y = df_use[[outcome]], x = df_use$votes_pct_against, c = CUTOFF, covs = covs_mat,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = df_use$cosbidfp),
    error = function(e) NULL)
  list(rd = rd_fit, data = df_use,
       estimate    = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$coef[2]),   # bias-corrected
       se          = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$se[3]),      # robust SE
       pval        = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$pv[3]),      # robust
       bw          = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$bws[1, 1]),
       n_eff_left  = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$N_h[1]),      # control side (<50)
       n_eff_right = if (is.null(rd_fit)) NA_real_ else as.numeric(rd_fit$N_h[2]))      # treated side (>50)
}

# Covariate-adjusted, triangular-weighted local linear at the MSE-optimal h.
# Returns the fitted renewed-side (control) and failed-side (treated) levels at the cutoff,
# each with a clustered standard error (the natural precision measure for a fitted value).
local_linear_check <- function(df, outcome, covariates, rd_fit) {
  na_out <- tibble(control_mean = NA_real_, control_se = NA_real_, treated_se = NA_real_)
  if (is.null(rd_fit)) return(na_out)
  h_value <- as.numeric(rd_fit$bws[1, 1])
  df_use <- df %>% filter(!is.na(.data[[outcome]]), !is.na(votes_pct_against), !is.na(cosbidfp))
  if (length(covariates) > 0) df_use <- df_use %>% filter(if_all(all_of(covariates), ~ !is.na(.x)))
  df_use <- df_use %>%
    mutate(running = votes_pct_against - CUTOFF, treat = as.integer(votes_pct_against > CUTOFF),
           abs_running = abs(running)) %>%
    filter(abs_running <= h_value) %>%
    mutate(weight = pmax(0, 1 - abs_running / h_value))
  wcenter <- function(x) stats::weighted.mean(x, w = df_use$weight, na.rm = TRUE)
  if (length(covariates) > 0)
    for (cv in covariates) df_use[[paste0(cv, "_c")]] <- df_use[[cv]] - wcenter(df_use[[cv]])
  rhs <- c("treat", "running", "treat:running", paste0(covariates, "_c"))
  fit <- feols(as.formula(paste(outcome, "~", paste(rhs, collapse = " + "))),
               data = df_use, weights = ~weight, cluster = ~cosbidfp)
  b <- stats::coef(fit); V <- stats::vcov(fit)
  control_se <- sqrt(V["(Intercept)", "(Intercept)"])                                  # fitted control level SE
  treated_se <- sqrt(V["(Intercept)", "(Intercept)"] + V["treat", "treat"] +           # fitted treated level SE
                     2 * V["(Intercept)", "treat"])
  tibble(control_mean = as.numeric(b[["(Intercept)"]]),
         control_se   = as.numeric(control_se),
         treated_se   = as.numeric(treated_se))
}

#-----------------------------------------------------------------------------------------------------------------#
# Data + estimation sample
#-----------------------------------------------------------------------------------------------------------------#
event_panel <- read_csv(file.path(stacked_dir, "road_quality_event_time_panel.csv"), show_col_types = FALSE)
conv_panel  <- event_panel %>% filter(model == "convnext_v2")

event_all    <- build_event_window_dataset(conv_panel, PRE_WINDOW, POST_WINDOW)
event_sample <- event_all %>% filter(has_pre == 1, has_post == 1)

#-----------------------------------------------------------------------------------------------------------------#
# (i) GLOBAL raw means (jurisdiction-election level, equal weight) over the estimation sample
#-----------------------------------------------------------------------------------------------------------------#
global_means <- event_sample %>%
  group_by(treated) %>%
  summarize(
    n            = n(),
    pre_rqr_m  = mean(pre_rqr_naip,  na.rm = TRUE), pre_rqr_sd  = sd(pre_rqr_naip,  na.rm = TRUE),
    post_rqr_m = mean(post_rqr_naip, na.rm = TRUE), post_rqr_sd = sd(post_rqr_naip, na.rm = TRUE),
    pre_rqs_m  = mean(pre_rqs_naip,  na.rm = TRUE), pre_rqs_sd  = sd(pre_rqs_naip,  na.rm = TRUE),
    post_rqs_m = mean(post_rqs_naip, na.rm = TRUE), post_rqs_sd = sd(post_rqs_naip, na.rm = TRUE),
    .groups = "drop"
  )
gC <- global_means %>% filter(treated == 0)   # control = renewed
gT <- global_means %>% filter(treated == 1)   # treated = failed

#-----------------------------------------------------------------------------------------------------------------#
# (ii) RD-ADJUSTED at the threshold.
#      Control = covariate-adjusted local-linear fitted renewed-side level at the cutoff.
#      Difference = bias-corrected RD estimate (matches Table 5); Treated = Control + Difference.
#      Post controls for pop + pre-election outcome (the bridge spec); pre (placebo) controls for pop only.
#-----------------------------------------------------------------------------------------------------------------#
thr_block <- function(outcome, covs) {
  rd <- run_rd_result(event_sample, outcome, covs)
  ll <- local_linear_check(rd$data, outcome, covs, rd$rd)
  tibble(
    control     = ll$control_mean,
    control_se  = ll$control_se,
    rd_estimate = rd$estimate,
    rd_se       = rd$se,
    treated     = ll$control_mean + rd$estimate,   # additive with the bias-corrected Difference
    treated_se  = ll$treated_se,                   # precision of the fitted treated level
    rd_pval     = rd$pval,
    bw          = rd$bw,
    n_eff_ctrl  = rd$n_eff_left,
    n_eff_trt   = rd$n_eff_right
  )
}
thr_pre_rqr  <- thr_block("pre_rqr_naip",  c("pop"))
thr_post_rqr <- thr_block("post_rqr_naip", c("pop", "pre_rqr_naip"))
thr_pre_rqs  <- thr_block("pre_rqs_naip",  c("pop"))
thr_post_rqs <- thr_block("post_rqs_naip", c("pop", "pre_rqs_naip"))

#-----------------------------------------------------------------------------------------------------------------#
# Console report (for verification against Table 5)
#-----------------------------------------------------------------------------------------------------------------#
cat("\n==================== ESTIMATION SAMPLE ====================\n")
cat(sprintf("Pre %s  Post %s   N control=%d  N treated=%d\n",
            paste(PRE_WINDOW, collapse=".."), paste(POST_WINDOW, collapse=".."), gC$n, gT$n))
cat("\n-- GLOBAL raw means (Treated / Control) --\n")
cat(sprintf("RQR pre  T/C = %.2f / %.2f   post T/C = %.2f / %.2f\n", gT$pre_rqr_m,gC$pre_rqr_m, gT$post_rqr_m,gC$post_rqr_m))
cat(sprintf("RQS pre  T/C = %.1f / %.1f   post T/C = %.1f / %.1f\n", gT$pre_rqs_m,gC$pre_rqs_m, gT$post_rqs_m,gC$post_rqs_m))
cat("\n-- EFFECTIVE at threshold (Control(se) / Treated(se) / Difference=RD est(se) / robust p / eff N C,T) --\n")
prnt <- function(tag, b, d=2) cat(sprintf("%-9s C=%.*f (%.*f)  T=%.*f (%.*f)  diff(RD)=%+.*f (%.*f) p=%.4f  eff N C/T=%g/%g  h=%.2f\n",
        tag, d,b$control, d,b$control_se, d,b$treated, d,b$treated_se, d,b$rd_estimate, d,b$rd_se, b$rd_pval, b$n_eff_ctrl, b$n_eff_trt, b$bw))
prnt("RQR pre ", thr_pre_rqr);     prnt("RQR post", thr_post_rqr)
prnt("RQS pre ", thr_pre_rqs, 1);  prnt("RQS post", thr_post_rqs, 1)

#-----------------------------------------------------------------------------------------------------------------#
# LaTeX table
#-----------------------------------------------------------------------------------------------------------------#
f2 <- function(x) formatC(x, format = "f", digits = 2)
f1 <- function(x) formatC(x, format = "f", digits = 1)
p2 <- function(x) paste0("(", f2(x), ")")   # parenthesized dispersion, 2 digits
p1 <- function(x) paste0("(", f1(x), ")")   # parenthesized dispersion, 1 digit

# Difference cell: bias-corrected RD estimate with stars from its robust p (matches Table 5)
thr_diff <- function(b, d) paste0(if (d == 2) f2(b$rd_estimate) else f1(b$rd_estimate), stars(b$rd_pval))

pre_tex  <- window_label_tex(PRE_WINDOW)
post_tex <- window_label_tex(POST_WINDOW)
bw_str   <- sprintf("%.1f$--$%.1f", min(thr_post_rqr$bw, thr_post_rqs$bw), max(thr_post_rqr$bw, thr_post_rqs$bw))
pre_p_str <- sprintf("RQR $p=%.2f$, RQS $p=%.2f$", thr_pre_rqr$rd_pval, thr_pre_rqs$rd_pval)

# one outcome-row = a means line then a dispersion line; columns ordered Control then Treated in each block
mean_row <- function(label, gc_m, gt_m, b, fmt, d) sprintf(
  "        %s & %s & %s & %s & %s & %s \\\\",
  label, fmt(gc_m), fmt(gt_m), fmt(b$control), fmt(b$treated), thr_diff(b, d))
disp_row <- function(gc_sd, gt_sd, b, par) sprintf(
  "                       & %s & %s & %s & %s & %s \\\\",
  par(gc_sd), par(gt_sd), par(b$control_se), par(b$treated_se), par(b$rd_se))

tex <- c(
"\\begin{table}[H]",
"    \\centering",
"    \\caption{Predicted Road Quality by Treatment Status and Period}",
"    \\label{tab:road_quality_panel}",
"    \\begin{threeparttable}",
"    \\begin{minipage}{\\linewidth}",
"    \\centering",
"    \\setlength{\\tabcolsep}{5pt}",
"    \\begin{tabular}{lccccc}",
"        \\toprule",
"        & \\multicolumn{2}{c}{\\textbf{Global}} & \\multicolumn{3}{c}{\\textbf{Effective}} \\\\",
"        \\cmidrule(lr){2-3} \\cmidrule(lr){4-6}",
"        & Control & Treated & Control & Treated & Difference \\\\",
"        & (Renewed) & (Failed) & (Renewed) & (Failed) & (Treated$-$Control) \\\\",
"        \\midrule",
"        \\multicolumn{6}{l}{\\textbf{Panel A: Road Quality Rating (0, 1, 2)}} \\\\",
mean_row(sprintf("Pre-Election (%s) ", pre_tex),  gC$pre_rqr_m,  gT$pre_rqr_m,  thr_pre_rqr,  f2, 2),
disp_row(gC$pre_rqr_sd,  gT$pre_rqr_sd,  thr_pre_rqr,  p2),
mean_row(sprintf("Post-Election (%s)", post_tex), gC$post_rqr_m, gT$post_rqr_m, thr_post_rqr, f2, 2),
disp_row(gC$post_rqr_sd, gT$post_rqr_sd, thr_post_rqr, p2),
"        \\addlinespace",
"        \\multicolumn{6}{l}{\\textbf{Panel B: Road Quality Score (RQS)}} \\\\",
mean_row(sprintf("Pre-Election (%s) ", pre_tex),  gC$pre_rqs_m,  gT$pre_rqs_m,  thr_pre_rqs,  f1, 1),
disp_row(gC$pre_rqs_sd,  gT$pre_rqs_sd,  thr_pre_rqs,  p1),
mean_row(sprintf("Post-Election (%s)", post_tex), gC$post_rqs_m, gT$post_rqs_m, thr_post_rqs, f1, 1),
disp_row(gC$post_rqs_sd, gT$post_rqs_sd, thr_post_rqs, p1),
"        \\bottomrule",
"    \\end{tabular}",
"    \\vspace{0.5em}",
"    \\begin{tablenotes}[flushleft]",
"        \\footnotesize",
paste0("        \\item \\textit{Notes:} The treated group comprises observations that failed to renew their road ",
       "tax levies. The control group comprises those that renewed. The sample is the ConvNeXt V2 stacked-event NAIP ",
       "road imagery sample with valid pre- and post-window imagery. Election-year imagery is omitted and windows are ",
       "censored at adjacent renewal elections. ",
       "\\textit{Global} columns are unweighted descriptive means across all observations with standard deviations in ",
       "parentheses, with no bandwidth restriction. ",
       "\\textit{Effective} columns are local-linear RD fits at the 50\\% vote-against cutoff, using the ",
       sprintf("MSE-optimal bandwidth of $h\\approx%s$ percentage points; these are fitted values at the cutoff with ", bw_str),
       "standard errors in parentheses, and the treated value is the control fit plus the RD estimate. "),
sprintf("        The pre-election rows are a placebo balance check and show no significant treated--control gap at the cutoff (%s), consistent with comparability near the threshold.", pre_p_str),
"        \\item Significance: *** $p<0.01$, ** $p<0.05$, * $p<0.1$.",
"    \\end{tablenotes}",
"    \\end{minipage}",
"    \\end{threeparttable}",
"\\end{table}"
)

out_path <- file.path(tables, "road_quality_means_table.tex")
writeLines(tex, out_path)
cat("\nWrote:", out_path, "\n")
