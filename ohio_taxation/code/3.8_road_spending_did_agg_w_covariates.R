




#======================================================================#
# Comparing Diff-in-Diff with Regression Discontinuity
#======================================================================#

# global did estimation
did_g <- lm(data = mgd_gr, 
            formula = median_sale_price ~ treated + time + did)
summary(did_g)

#======================================================================#
# Running Global DID regressions  out each post-treatment years ----
#======================================================================#

# no covariates
did_regs <- purrr::map(did_dfs, ~ lm(data = .x, 
                                    formula = median_sale_price ~ treated + time + did))

map(did_regs, summary)



# global regression discontinuity estimation
dfs_agg


