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
install.packages("xlsx")

# placebo cutoffs
placebo_cutoffs <- c(30, 40, 60, 70)

map(dfs_agg_covs, )

gs_p <- map(placebo_cutoffs , ~ map2(covs_final, dfs_agg_covs, .f = function(x,y){
                              rdrobust(  y = y$median_sale_amount,
                                         x = y$votes_pct_for,
                                         c = .x,
                                         covs = y %>%
                                           select(x) ,
                                         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
}) )
names(gs_p) <- placebo_cutoffs

tes_g_p <- map(gs_p, ~ te_tables(.x))

# tes_g_p[grepl("plus", tes_g_p$dataset),"bias_corrected_coef"] %>% pull %>% mean
# plot_te(tes_g_p[[4]], title = "Visualization of Treatment Effects", subtitle = "With covariates")

# output to excel sheet tes_gs_placebos.xlsx
for (i in placebo_cutoffs){
  i = as.character(i)
  if (i == "30") {
    write.xlsx(tes_g_p[i], file = paste0(tables, "/tes_gs_placebos.xlsx"), sheetName = i, row.names = FALSE)
  }
  else write.xlsx(tes_g_p[i], file = paste0(tables, "/tes_gs_placebos.xlsx"), sheetName = i, append = TRUE, row.names = FALSE)
}
