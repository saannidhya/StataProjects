#================================================================================================================#
# Purpose : Use median sale amount as outcome of interest for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 02/01/2024
# Log     : 
#       02/01/2024: finished the program. Replicated in Stata as well. See road_spending_reg_discontinuity_agg.do
#================================================================================================================#


# Load the packages
library(openxlsx)
library(xlsx)

# placebo cutoffs
placebo_cutoffs <- c(30, 40, 60, 70)

gs_pb <- map(placebo_cutoffs , ~ map2(covs_final, dfs_agg_covs, .f = function(x,y){
                              rdrobust(  y = y$median_sale_amount,
                                         x = y$votes_pct_against,
                                         c = .x,
                                         covs = y %>%
                                           select(x) ,
                                         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
}) )
names(gs_pb) <- placebo_cutoffs

tes_g_pb <- map2(gs_pb, placebo_cutoffs, ~ te_tables(.x) %>% mutate(cutoff = .y) ) %>% bind_rows
tes_g_pb$pval %>% sort

# tes_g_p[grepl("plus", tes_g_p$dataset),"bias_corrected_coef"] %>% pull %>% mean
# plot_te(tes_g_p[[4]], title = "Visualization of Treatment Effects", subtitle = "With covariates")

# output to csv
write.csv(tes_g_pb, paste0(tables, "/tes_gs_placebos.csv"), row.names = FALSE)


#==============================================#
# Adding Time Fixed Effects
#==============================================#

gs_reg_pb <- map(placebo_cutoffs , ~ map2(covs_final_w_tfe, dfs_agg_covs_w_tfe, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = .x,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
}) )
names(gs_reg_pb) <- placebo_cutoffs

tes_reg_g_pb <- map2(gs_reg_pb, placebo_cutoffs, ~ te_tables(.x) %>% mutate(cutoff = .y) ) %>% bind_rows
tes_reg_g_pb$pval %>% sort

tes_reg_g_pb %>% print(., n = 56)
