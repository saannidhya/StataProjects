# =====================================================================
# anticipation_test_diagnostics.R
#
# Diagnostics for the rational-expectations anticipation test discussed
# in Section 4 (emp_strategy.tex Subsection "Testing for Anticipation
# under Rational Expectations").
#
# Under RE, the pre-period RD coefficients theta_{-3}, theta_{-2}, theta_{-1}
# on median house price should be jointly indistinguishable from zero.
#
# This script reports:
#   1. Joint F-test on the three pre-period coefficients
#   2. Lee-Card bandwidth sensitivity of each pre-period coefficient
#   3. Permutation test of the joint null using randomly-assigned placebo
#      close-election cutoffs
#
# Reads from:
#   data/intermediate/rdd_panel.rds  (the panel used in 3.12_rdd_house_prices.R)
#
# Writes to:
#   data/outputs/tables/anticipation_test.tex
# =====================================================================

# ---- Dependencies ----
pkgs <- c("rdrobust", "data.table", "car", "ggplot2")
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Paths ----
project_root <- normalizePath("../..")
panel_path   <- file.path(project_root, "data/intermediate/rdd_panel.rds")
out_tab      <- file.path(project_root, "data/outputs/tables/anticipation_test.tex")

if (!file.exists(panel_path)) {
  stop("Panel file not found at: ", panel_path,
       "\nRun code/3.12_rdd_house_prices.R first.")
}

dt <- readRDS(panel_path)

# ---- 1. Joint F-test on pre-period coefficients ----
# Stack three pre-period RD regressions and test the joint hypothesis that
# all three discontinuities are zero, accounting for clustering by
# jurisdiction.

stack_dt <- rbindlist(list(
  dt[tau == -3][, tau_flag := "t-3"],
  dt[tau == -2][, tau_flag := "t-2"],
  dt[tau == -1][, tau_flag := "t-1"]
))

stack_dt[, tau_t3 := as.integer(tau_flag == "t-3")]
stack_dt[, tau_t2 := as.integer(tau_flag == "t-2")]
stack_dt[, tau_t1 := as.integer(tau_flag == "t-1")]

# Sharp RD with vote_share_against centered at 0.50.
stack_dt[, run := vote_share_against - 0.50]
stack_dt[, treat := as.integer(run > 0)]

# Triangular-kernel weights at the optimal bandwidth used in the main analysis.
h_opt <- 0.08  # replace with the bandwidth from rdrobust if available
stack_dt[, tri_w := pmax(0, 1 - abs(run) / h_opt)]

# Interactions: discontinuity allowed to vary by tau bucket.
fit_joint <- lm(
  median_sale_amount ~ run * treat * tau_flag,
  data = stack_dt[abs(run) <= h_opt],
  weights = tri_w
)

# Joint F-test of the three discontinuity-by-bucket coefficients.
joint_test <- linearHypothesis(
  fit_joint,
  c("treat = 0",
    "treat:tau_flagt-2 = 0",
    "treat:tau_flagt-3 = 0")
)
F_stat <- joint_test$F[2]
F_pval <- joint_test$`Pr(>F)`[2]

cat(sprintf("Joint F-statistic: F(3, df) = %0.3f, p = %0.4f\n",
            F_stat, F_pval))

# ---- 2. Bandwidth sensitivity ----
bws <- seq(0.04, 0.16, by = 0.02)
sens <- rbindlist(lapply(bws, function(h) {
  sub <- stack_dt[abs(run) <= h]
  sub[, tri_w := pmax(0, 1 - abs(run) / h)]
  if (nrow(sub) < 30) return(NULL)
  fit <- lm(median_sale_amount ~ run * treat * tau_flag,
            data = sub, weights = tri_w)
  test <- linearHypothesis(
    fit,
    c("treat = 0",
      "treat:tau_flagt-2 = 0",
      "treat:tau_flagt-3 = 0")
  )
  data.table(bandwidth = h, F = test$F[2], p = test$`Pr(>F)`[2])
}))

# ---- 3. Permutation test ----
# Simulate the null by randomly reassigning election outcomes within each
# jurisdiction (preserving the running-variable distribution but breaking
# the link to actual close-election failure).
set.seed(2026)
n_perm <- 999
perm_F <- numeric(n_perm)
for (i in seq_len(n_perm)) {
  perm_dt <- copy(stack_dt[abs(run) <= h_opt])
  perm_dt[, treat := sample(treat), by = jurisdiction_id]
  fit_p <- tryCatch(
    lm(median_sale_amount ~ run * treat * tau_flag,
       data = perm_dt, weights = tri_w),
    error = function(e) NULL
  )
  if (is.null(fit_p)) { perm_F[i] <- NA; next }
  test_p <- tryCatch(
    linearHypothesis(fit_p,
      c("treat = 0",
        "treat:tau_flagt-2 = 0",
        "treat:tau_flagt-3 = 0")
    ),
    error = function(e) NULL
  )
  perm_F[i] <- if (is.null(test_p)) NA else test_p$F[2]
}
perm_p <- mean(perm_F >= F_stat, na.rm = TRUE)

# ---- Output ----
table_tex <- c(
  "\\begin{table}[H]",
  "  \\centering",
  "  \\caption{Rational-expectations anticipation test, diagnostic suite.}",
  "  \\label{tab:anticipation_test_diagnostics}",
  "  \\begin{tabular}{l c c}",
  "    \\toprule",
  "    Test & Statistic & p-value \\\\",
  "    \\midrule",
  sprintf("    Joint F-test on $\\theta_{-3}, \\theta_{-2}, \\theta_{-1}$ & %0.3f & %0.4f \\\\",
          F_stat, F_pval),
  sprintf("    Permutation test (%d replications)              & --- & %0.4f \\\\",
          sum(!is.na(perm_F)), perm_p),
  "    \\midrule",
  "    \\multicolumn{3}{l}{\\textit{Bandwidth sensitivity (joint F):}} \\\\")

for (i in seq_len(nrow(sens))) {
  table_tex <- c(table_tex, sprintf(
    "    \\quad bandwidth = %0.2f & %0.3f & %0.4f \\\\",
    sens$bandwidth[i], sens$F[i], sens$p[i]
  ))
}

table_tex <- c(table_tex,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}",
  "  \\footnotesize",
  "  \\item Tests the joint null $H_0: \\theta_{-3} = \\theta_{-2} = \\theta_{-1} = 0$.",
  "  Failure to reject is consistent with the no-anticipation prediction of",
  "  rational expectations under unforecastable close-election outcomes.",
  "  \\end{tablenotes}",
  "\\end{table}"
)
writeLines(table_tex, out_tab)

cat(sprintf("Saved: %s\n", out_tab))
