#==========================================================================================================#
# Purpose : Set-up RDD loop for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 07/12/2022
# Log     : 07/12/2022: finished RD Plots section
#           07/16/2022: separated aggregate section. this code contains non-aggregate only.
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))

#==========================================================================================================#
#                                     Non-Aggregated Results (using raw sale_amount) ----
#==========================================================================================================#


#=================================#
# Data Summary
#=================================#
# overall summary
purrr::map(dfs, ~.x %>% 
                 select(sale_amount, ln_sale_amount, votes_pct_for,treated) %>%
                 summary()
             )
# grouped by treatment
for (i in c("sale_amount","ln_sale_amount","votes_pct_for")){
  tapply(df[[i]], df$treated, summary)
  # print(i)
}
df_dt <- df%>% 
          select(c("sale_amount","ln_sale_amount","votes_pct_for","treated")) %>%
          setDT()

df_dt[, as.list(summary(df_dt)), by = treated]

#=====================================#
# Manipulation test (X variable) ----
#=====================================#
# housing t+10 example rdd density plot
rddensity::rdplotdensity(rdd = rddensity::rddensity(X = dfs$housing_roads_census_t_plus_10_matches$votes_pct_for, c = 50),
                         X = dfs$housing_roads_census_t_plus_10_matches$votes_pct_for)
# failed Mccrary test 1+10 example
ggplot(data = dfs$housing_roads_census_t_plus_10_matches) +
  geom_histogram(aes(x = votes_pct_for), bins = 200)

### Density/Manipulation test
# failed
dens_test_1 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_1_matches$votes_pct_for, c = 50)
summary(dens_test_1)
# failed
dens_test_2 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_2_matches$votes_pct_for, c = 50)
summary(dens_test_2)
# failed
dens_test_3 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_3_matches$votes_pct_for, c = 50)
summary(dens_test_3)
# failed
dens_test_4 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_4_matches$votes_pct_for, c = 50)
summary(dens_test_4)
# borderline failed
dens_test_5 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_5_matches$votes_pct_for, c = 50)
summary(dens_test_5)
# failed
dens_test_6 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_6_matches$votes_pct_for, c = 50)
summary(dens_test_6)
# passed
dens_test_7 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_7_matches$votes_pct_for, c = 50)
summary(dens_test_7)
# failed
dens_test_8 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_8_matches$votes_pct_for, c = 50)
summary(dens_test_8)
# failed
dens_test_9 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_9_matches$votes_pct_for, c = 50)
summary(dens_test_9)
# failed
dens_test_10 <- rddensity::rddensity(X = dfs$housing_roads_census_t_plus_10_matches$votes_pct_for, c = 50)
summary(dens_test_10)

#=====================================#
# RD plots -----
#=====================================#

# t+10 as an example (14 is the best)
for (i in 10:30){
  rdrobust::rdplot(y = dfs$housing_roads_census_t_plus_10_matches$sale_amount, 
                   x = dfs$housing_roads_census_t_plus_10_matches$votes_pct_for, 
                   c = 50, p = 2, nbins = c(i,i), title = paste0(as.character(i)), h = c(5,5))
}

#===========================================#
# Running regressions (non-aggregate) ----
#===========================================#

# run_rdd takes df and vars to give out all possible regressions based on specified options
run_rdd <- function(df,
                    Y,
                    X,
                    Z = NULL,
                    cutoff = 50,
                    kernel_list = c("uniform","triangular","epanechnikov"),
                    bw_list = c("mserd","msesum","cerrd","cersum"),
                    p = 1,
                    q = 1
){
  regs <- list()
  Y = deparse(substitute(Y))
  X = deparse(substitute(X))
  counter <- 1
  for (kernel in 1:length(kernel_list)){
    for (bw in 1:length(bw_list)){
      print(paste0("Your Kernal is ",kernel_list[kernel]," and your Bandwidth is ", bw_list[bw]))
      reg <- rdrobust::rdrobust(y = df[[Y]], x = df[[X]], kernel = kernel_list[kernel], bwselect = bw_list[bw], c = cutoff)
      regs[[counter]] <- reg
      counter <- counter + 1
    }
  }
  return(regs)
}
# regressions <- run_rdd(df = dfs$housing_roads_census_t_plus_10_matches,
#                     Y = sale_amount,  
#                     X = votes_pct_for)

# store_rdd_results takes a list containing regression results and converts it into a dataframe
store_rdd_results <- function(regressions){
  tbl <- NULL
  map(regressions, function(x){
    coefs <-  as_tibble(t(x$coef)) %>% dplyr::rename_with(~paste0("coef_",.x))
    pvals <- as_tibble(t(x$pv)) %>% dplyr::rename_with(~paste0("pval_",.x))
    bws <- as_tibble(plyr::round_any(x$bws, 0.01)) %>% unite(bandwidth, c("left", "right"), sep = ",") %>% t() %>% as_tibble() %>% rename(h = names(.)[1], b = names(.)[2])
    cis <- as_tibble(round(x$ci)) %>% unite(ci, c("CI Lower", "CI Upper"), sep = ",") %>% t() %>% as_tibble() %>% rename(ci_conventional = names(.)[1], ci_bc = names(.)[2], ci_robust = names(.)[3])
    kernel <- as_tibble(x$kernel) %>% rename(kernel = value)
    bw_type <- as_tibble(x$bwselect) %>% rename(bwselect = value)
    N <- as_tibble(paste0(as.character(x$N), collapse=", ")) %>% rename(N = value)
    N_h <- as_tibble(paste0(as.character(x$N_h), collapse=", ")) %>% rename(N_h = value)
    N_b <- as_tibble(paste0(as.character(x$N_b), collapse=", ")) %>% rename(N_b = value)
    all_stats <- bind_cols(coefs, pvals, bws, cis, kernel, bw_type, N, N_h, N_b)
    rbind(tbl, all_stats)
  })
}
# rdd_results <- store_rdd_results(regressions) %>% bind_rows()


# running run_rdd() and store_rdd_results() for all housing dfs from plus1 to plus10
start_time <- Sys.time()
all_rdd_results <- purrr::map(dfs[3:length(dfs)],
                         function(x){
                          # dataset <- deparse(substitute(x))
                          # print(paste0("Your dataset is ",dataset))
                          regressions <- run_rdd(df = x,
                                                 Y = sale_amount,
                                                 X = votes_pct_for,
                                                 # kernel_list = "uniform",
                                                 # bw_list = "mserd"
                                                 )
                          rdd_results <- store_rdd_results(regressions) %>% bind_rows()
                          # rdd_results$dataset <- dataset
                          rdd_results
                    })
end_time <- Sys.time()
print(paste0("Run time was ",end_time - start_time))

# deparse(substitute(dfs[[3]]))
# names(dfs[3:length(dfs)])
# stringr::str_detect(names(dfs[3:length(dfs)]), )

# storing dataset names with the results
nm_results <- names(all_rdd_results)
all_rdd_results_ <- all_rdd_results
for (i in nm_results){
  all_rdd_results[[i]]$dataset <- i
}

all_rdd_results <- all_rdd_results %>% dplyr::bind_rows() %>% dplyr::relocate(dataset)



