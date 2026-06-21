# =====================================================================
# placebo_non_renewal_rd.R
#
# Placebo RD on non-renewal local elections in the same jurisdictions.
#
# Purpose: rule out the possibility that the post-period house-price
# response documented in Section 5 is driven by generic close-margin
# local-election dynamics rather than by the road-maintenance shock.
#
# The placebo sample is restricted to close-margin (effective bandwidth
# +/- 8 percentage points) elections of these types in the same townships
# during overlapping years:
#   1. School-bond renewal referendums
#   2. General-purpose-levy referendums
#   3. Police or fire-protection levy referendums (excluding fire-protection
#      which Cassidy et al. 2024 documents has its own capitalization effect)
#
# Under the road-specific mechanism, the placebo coefficients should be
# small and statistically indistinguishable from zero.
#
# Reads from:
#   data/inputs/voting_data/non_renewal_elections.csv   (TODO: build)
#   data/inputs/housing/median_sale_amount_panel.csv
#
# Writes to:
#   data/outputs/tables/placebo_non_renewal_rd.tex
#   data/outputs/figures/placebo_non_renewal_rd.pdf
# =====================================================================

# ---- Dependencies ----
pkgs <- c("rdrobust", "data.table", "ggplot2", "stargazer", "fixest")
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Paths ----
project_root <- normalizePath("../..")
votes_path   <- file.path(project_root, "data/inputs/voting_data/non_renewal_elections.csv")
prices_path  <- file.path(project_root, "data/inputs/housing/median_sale_amount_panel.csv")
out_tab      <- file.path(project_root, "data/outputs/tables/placebo_non_renewal_rd.tex")
out_fig      <- file.path(project_root, "data/outputs/figures/placebo_non_renewal_rd.pdf")

# ---- Load ----
# Placeholder load: the user should replace with the actual vote-file path
# once the non-renewal election sample is constructed from the Ohio
# Secretary of State raw files.
if (!file.exists(votes_path)) {
  stop("Non-renewal elections file not found at: ", votes_path,
       "\nBuild this file from raw Ohio SoS vote data before running.")
}

votes  <- fread(votes_path)
prices <- fread(prices_path)

# ---- Sample restriction: close-margin elections in our event-time window ----
votes <- votes[election_type %in% c("school_bond_renewal",
                                    "general_purpose_levy",
                                    "police_levy")]
votes <- votes[abs(vote_share_against - 0.50) <= 0.08]

# ---- Merge: vote outcome + dynamic house price by event time ----
# Construct a panel of {jurisdiction, election_year, tau} for tau in -3..10.
dt <- merge(prices, votes, by = c("jurisdiction_id", "year"),
            all.x = FALSE, all.y = TRUE)
dt[, tau := year_obs - election_year]
dt <- dt[tau >= -3 & tau <= 10]
dt[, failed := as.integer(vote_share_against > 0.50)]

# ---- Estimate dynamic RD coefficient for each tau ----
taus <- -3:10
results <- data.table(tau = integer(), estimate = double(),
                      se = double(), p = double(), n = integer())

for (tau_val in taus) {
  sub <- dt[tau == tau_val & !is.na(median_sale_amount)]
  if (nrow(sub) < 50) next

  fit <- tryCatch(
    rdrobust(y = sub$median_sale_amount,
             x = sub$vote_share_against,
             c = 0.50,
             cluster = sub$jurisdiction_id),
    error = function(e) NULL
  )
  if (is.null(fit)) next

  results <- rbind(results, data.table(
    tau = tau_val,
    estimate = fit$coef[1],   # conventional point estimate
    se = fit$se[3],            # robust-bias-corrected SE
    p = fit$pv[3],
    n = sum(fit$N[3:4])
  ))
}

# ---- Output table ----
results[, sig_stars := fcase(
  p < 0.01,  "***",
  p < 0.05,  "**",
  p < 0.10,  "*",
  default   = ""
)]
fwrite(results, gsub("\\.tex$", ".csv", out_tab))

# Write a LaTeX-formatted table that can be \input{} into appendix.tex.
table_tex <- c(
  "\\begin{table}[H]",
  "  \\centering",
  "  \\caption{Placebo RD: non-renewal local elections, same townships.}",
  "  \\label{tab:placebo_non_renewal_rd}",
  "  \\begin{tabular}{l c c c c}",
  "    \\toprule",
  "    Event time $\\tau$ & Estimate (\\$) & Robust SE & p-value & $N$ \\\\",
  "    \\midrule"
)
for (i in seq_len(nrow(results))) {
  row <- results[i]
  table_tex <- c(table_tex, sprintf(
    "    %d & %0.0f%s & %0.0f & %0.3f & %d \\\\",
    row$tau, row$estimate, row$sig_stars, row$se, row$p, row$n
  ))
}
table_tex <- c(table_tex,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}",
  "  \\footnotesize",
  "  \\item Estimates from sharp regression-discontinuity design on close-margin",
  "  non-road-maintenance elections in the same townships as the main sample.",
  "  Significance levels: * p < 0.10, ** p < 0.05, *** p < 0.01.",
  "  \\end{tablenotes}",
  "\\end{table}"
)
writeLines(table_tex, out_tab)

# ---- Output figure ----
g <- ggplot(results, aes(x = tau, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se,
                      ymax = estimate + 1.96 * se), size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  labs(x = "Years from referendum (tau)",
       y = "House price RD coefficient ($)",
       title = "Placebo: close-margin non-renewal local elections") +
  theme_minimal(base_size = 11)
ggsave(out_fig, g, width = 7.5, height = 4.5)

cat(sprintf("Saved: %s\nSaved: %s\n", out_tab, out_fig))
