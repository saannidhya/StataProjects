housing_roads_census_t_plus_10_matches


colnames(dfs_agg_covs$housing_roads_census_t_plus_5_matches)

summary(dfs_agg_covs$housing_roads_census_t_plus_5_matches$votes_pct_against)


for (gap in 1:20){
  close_means <- dfs_agg_covs$housing_roads_census_t_plus_5_matches %>%
    mutate(treated = if_else(votes_pct_against > 50, 1, 0 ),
           votes_pct_against_center = votes_pct_against-50) %>%
    filter(between(votes_pct_against_center, -gap, gap)) %>%
    group_by(treated) %>%
    summarize(
      n = n(),
      mean_median_sale_amount = mean(median_sale_amount, na.rm = TRUE),
      sd_median_sale_amount = sd(median_sale_amount, na.rm = TRUE)
    )
  
}


dfs_agg_covs$housing_roads_census_t_plus_4_matches %>%
    mutate(treated = if_else(votes_pct_against > 50, 1, 0 ),
                     votes_pct_against_center = votes_pct_against-50) %>%
     filter(between(votes_pct_against_center, -5, 5)) %>%
     group_by(treated) %>%
     summarize(
         n = n(),
         mean_median_sale_amount = mean(median_sale_amount, na.rm = TRUE),
         sd_median_sale_amount = sd(median_sale_amount, na.rm = TRUE)
       )



#===================================================================================#

# install.packages(c("tidyverse", "scales")) # if needed
library(tidyverse)
library(scales)

# Generic helper: mean-diff by RD bandwidths
rd_mean_diff <- function(data, outcome, score,
                         cutoff = 50, bws = 1:20) {
  outcome <- rlang::enquo(outcome)
  score   <- rlang::enquo(score)
  
  base <- data %>%
    mutate(
      treated = if_else( (!!score) > cutoff, 1L, 0L ),
      score_center = (!!score) - cutoff
    )
  
  purrr::map_dfr(bws, function(bw) {
    base %>%
      filter(between(score_center, -bw, bw)) %>%
      group_by(treated) %>%
      summarise(
        n = sum(!is.na(!!outcome)),
        mean_outcome = mean(!!outcome, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # ensure both sides exist even if one is empty at small bw
      tidyr::complete(treated = c(0L, 1L),
                      fill = list(n = 0L, mean_outcome = NA_real_)) %>%
      tidyr::pivot_wider(
        names_from = treated,
        values_from = c(n, mean_outcome),
        names_prefix = "treated_"
      ) %>%
      transmute(
        bandwidth = bw,
        n0   = n_treated_0 ,
        n1   = n_treated_1 ,
        mean0 = mean_outcome_treated_0 ,
        mean1 = mean_outcome_treated_1,
        diff = dplyr::if_else(n0 > 0 & n1 > 0, mean1 - mean0, NA_real_)
      )
  })
}

# Run on your data
te_by_bw_3 <- rd_mean_diff(
  data   = dfs_agg_covs$housing_roads_census_t_plus_3_matches,
  outcome = median_sale_amount,
  score   = votes_pct_against,
  cutoff  = 50,
  bws     = 1:20
)
te_by_bw_4 <- rd_mean_diff(
  data   = dfs_agg_covs$housing_roads_census_t_plus_4_matches,
  outcome = median_sale_amount,
  score   = votes_pct_against,
  cutoff  = 50,
  bws     = 1:20
)
te_by_bw_5 <- rd_mean_diff(
  data   = dfs_agg_covs$housing_roads_census_t_plus_5_matches,
  outcome = median_sale_amount,
  score   = votes_pct_against,
  cutoff  = 50,
  bws     = 1:20
)
te_by_bw_6 <- rd_mean_diff(
  data   = dfs_agg_covs$housing_roads_census_t_plus_6_matches,
  outcome = median_sale_amount,
  score   = votes_pct_against,
  cutoff  = 50,
  bws     = 1:20
)

# --- Build results for horizons t+3 through t+8 ---
horizons <- 3:6
bws      <- 1:12

te_all <- purrr::map_dfr(horizons, function(h) {
  # Pull each t+X df from your list by name
  df_name <- sprintf("housing_roads_census_t_plus_%d_matches", h)
  dat     <- dfs_agg_covs[[df_name]]
  
  # Skip gracefully if the df is missing
  if (is.null(dat)) return(tibble())
  
  rd_mean_diff(
    data    = dat,
    outcome = median_sale_amount,
    score   = votes_pct_against,
    cutoff  = 50,
    bws     = bws
  ) %>%
    mutate(horizon = paste0("t+", h))
})

# --- Plot: treatment-effect (treated - control) vs bandwidth, all horizons ---
ggplot(te_all, aes(x = bandwidth, y = diff, color = horizon)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "RD bandwidth sensitivity: difference in means across horizons",
    subtitle = "Outcome: median_sale_amount; Treated = votes_pct_against > 50",
    x = "RD Bandwidth",
    y = "Mean Difference in House Price: Cut − Renewed (USD)",
    color = "Horizon"
  ) +
  scale_y_continuous(labels = label_dollar(accuracy = 1)) +
  scale_x_continuous(breaks = bws) +
  theme_minimal()
