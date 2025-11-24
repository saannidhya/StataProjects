#================================================================================================================#
# Purpose : Road Quality Analysis
# Name    : Saani Rawat
# Log     : 1. 1/12/2025: made a more formal update to code. Added Qs that each snippet answers.
#           2. 1/16/2025: results from test run of fine-tuned gpt-4 mode
#           3. 2/26/2025: Added regression analysis for Road quality
#           4. 9/30/2025: Added analysis using predictions from convnext v2 model fine-tuned on HF
#================================================================================================================#

library(fixest)
library(MASS) 
library(tidyverse)

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

#==========================================================================================================#
#  Q. Does road quality actually change after the tax cuts?
#     Results show they do.
#==========================================================================================================#

# Regression analysis #
above_roads <- readr::read_csv(paste0(data,"/roads/ohio/above/above_predictions_with_flag.csv")) %>% 
  mutate(treat_flag = 1, # Above means these areas are above the cutoff for % votes against i.e. cut their renewal taxes 
         road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
           TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
         ) |> round(1) ,
         road_quality_score3 = case_when(
           predicted_label == 0 ~ (1 - 0.8*confidence) * 99 / 3 ,
           TRUE                 ~ predicted_label * 99 / 3  + 0.8*confidence * 99 / 3         ) |> round(1) 
  )  %>%
  mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
         year = ifelse(nchar(year) == 2, paste0("20", year), year),
         year = as.integer(year))

below_roads <- readr::read_csv(paste0(data,"/roads/ohio/below/below_predictions_with_flag.csv")) %>% 
  mutate(treat_flag = 0,
         road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
           TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
         ) |> round(1),
         road_quality_score3 = case_when(
           predicted_label == 0 ~ (1 - 0.8*confidence) * 99 / 3 ,
           TRUE                 ~ predicted_label * 99 / 3  + 0.8*confidence * 99 / 3         ) |> round(1) 
  ) %>%
  mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
         year = ifelse(nchar(year) == 2, paste0("20", year), year),
         year = as.integer(year))

roads_close <- bind_rows(above_roads, below_roads) %>% mutate(did = post_election_flag*treat_flag)


# group means 
roads_close %>%
  group_by(treat_flag, post_election_flag) %>%
  summarize(mean = mean(predicted_label), sd = sd(predicted_label) ,
            mean_score = mean(road_quality_score), sd_score = sd(road_quality_score) ,
            mean_score2 = mean(road_quality_score2), sd_score2 = sd(road_quality_score2) ,
            mean_score3 = mean(road_quality_score3), sd_score3 = sd(road_quality_score3) ,
            # ,n = n()
  ) 
# select(treat_flag, post_election_flag, mean_score3, sd_score3)

# before and after: treatment group
road_above_lm <- lm(data = above_roads, formula = predicted_label ~ post_election_flag)
summary(road_above_lm)
# before and after: control group
road_below_lm <- lm(data = below_roads, formula = predicted_label ~ post_election_flag)
summary(road_below_lm)

# before and after: treatment group
road_above_rqs_lm <- lm(data = above_roads, formula = road_quality_score2 ~ post_election_flag)
summary(road_above_rqs_lm)
# before and after: control group
road_below_rqs_lm <- lm(data = below_roads, formula = road_quality_score2 ~ post_election_flag)
summary(road_below_rqs_lm)



#==========================================================================================================#
#                   Regression Discontinuity Analysis
#==========================================================================================================#

# Need to add running variable to roads_close
colnames(roads_close)
nrow(roads_close)

roads_close2 <- roads_close %>% 
    left_join(roads_and_census %>% dplyr::select(tendigit_fips, year, votes_pct_against) %>% rename(election_year = year), 
    by = c("tendigit_fips", "election_year")) 

View(roads_close2)

roads_close3 <- roads_close2 %>% filter(post_election_flag == 1)

rdrobust_results <- rdrobust(y = roads_close3$predicted_label,
       x = roads_close3$votes_pct_against,
       c = cutoff,
       all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2
       )
summary(rdrobust_results)

rdrobust_results1 <- rdrobust(y = roads_close3$road_quality_score,
       x = roads_close3$votes_pct_against,
       c = cutoff,
      #  covs = roads_close2 %>%
      #    dplyr::select(x) ,
       all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2
      #  h = max(abs(roads_close3$votes_pct_against - cutoff), na.rm = TRUE)
       )
summary(rdrobust_results1)


rdrobust_results2 <- rdrobust(y = roads_close3$road_quality_score2,
       x = roads_close3$votes_pct_against,
       c = cutoff,
      #  covs = roads_close2 %>%
      #    dplyr::select(x) ,
       all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2
      #  h = max(abs(roads_close3$votes_pct_against - cutoff), na.rm = TRUE)
       )
summary(rdrobust_results2)

rdrobust_results3 <- rdrobust(y = roads_close3$road_quality_score3,
       x = roads_close3$votes_pct_against,
       c = cutoff,
      #  covs = roads_close2 %>%
      #    dplyr::select(x) ,
       all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2
      #  h = max(abs(roads_close3$votes_pct_against - cutoff), na.rm = TRUE)
       )
summary(rdrobust_results3)

# opp <- roads_and_census %>% dplyr::select(tendigit_fips, year, votes_pct_against)  %>% rename(election_year = year)  %>% 
#     inner_join(roads_close, 
#     by = c("tendigit_fips", "election_year")) 
# View(roads_close2)


#==========================================================================================================#
#                   Predictions from convnext v2 model
#==========================================================================================================#

# importing subdivision and county name
cty_sub_names <- readxl::read_excel(paste0(data,"/ohio-only-all-geocodes-2016.xlsx")) %>% janitor::clean_names() %>% 
                    dplyr::select(dplyr::all_of(c("tendigit_fips", "name_note_if_split_between_two_counties", "county_name", "split_flag"))) %>% 
                    rename(subdivision = name_note_if_split_between_two_counties, county = county_name) %>%
                    mutate(subdivision = if_else(split_flag == 1,
                               trimws(str_replace(subdivision, "(village|city).*", "\\1")),
                               subdivision)) 

# above_roads_hf$image_path
# Import the new prediction files
above_roads_hf <- readr::read_csv(paste0(data,"/roads/hf_finetuned_convnextv2/ohio_preds/ohio_above_preds.csv"))  %>%
       mutate(
              image = basename(image_path),
              year = as.integer(str_extract(image, "\\d{2}(?=\\.jpg$)") %>% paste0("20", .)),
              tendigit_fips = as.numeric(str_extract(image, "\\d{10}"))
       )   %>% 
  mutate(road_quality_score = round(((pred_label + max_prob) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           pred_label == 0 ~ 1 + (1 - 0.5*max_prob) * 99 / 3 ,
           TRUE                 ~ 1 + pred_label * 99 / 3  + 0.5*max_prob * 99 / 3 
         ) |> round(1) ,
         road_quality_score3 = case_when(
           pred_label == 0 ~ (1 - 0.8*max_prob) * 99 / 3 ,
           TRUE                 ~ pred_label * 99 / 3  + 0.8*max_prob * 99 / 3         ) |> round(1) 
  ) 
       # left_join(cty_sub_names %>% distinct(tendigit_fips, .keep_all = TRUE), by = "tendigit_fips")
# print(above_roads_hf, width=Inf)

# Find the county subdivisions with the closest votes 
closest_votes <- roads_and_census %>%
                    # filter(between(votes_pct_against, cutoff - tes_gs_bw, cutoff + tes_gs_bw)) %>%
                    filter(between(votes_pct_against, cutoff - mean_eff_bw, cutoff + mean_eff_bw)) %>%
                    arrange(treated, desc(votes_pct_against)) %>% 
                    dplyr::select(tendigit_fips, year, votes_pct_against, treated, pop) %>%
                    left_join(cty_sub_names, by = "tendigit_fips") %>%
                    relocate(c(subdivision, county) , .after = tendigit_fips) %>%
                    filter(year >= 2010) # because we need before-after data

closest_votes_a <- closest_votes %>% filter(treated == 1) %>% arrange(tendigit_fips, votes_pct_against) %>% distinct(tendigit_fips, .keep_all = TRUE) %>% 
       rename(election_year=year)

above_roads_hf_a <- above_roads_hf %>% left_join(closest_votes_a, by = "tendigit_fips") %>% arrange(tendigit_fips, year) %>%
                     mutate(post_election_flag = ifelse(year > election_year, 1, 0)) %>%
       group_by(tendigit_fips, election_year) %>%
       filter(any(post_election_flag == 0) & any(post_election_flag == 1)) %>% # atleast have one pre and one post image
       ungroup() %>%
       relocate(post_election_flag, .after = pred_label)

View(above_roads_hf_a)

above_roads_hf_a %>% 
   group_by(post_election_flag) %>%
    summarize(n = n(), 
    mean_rq = mean(pred_id), sd_rq = sd(pred_id),
    mean_rqs = mean(road_quality_score2), sd_rqs = sd(road_quality_score2))   

mod_a <- feols(data = above_roads_hf_a, fml = pred_id ~ post_election_flag, cluster = ~tendigit_fips)
summary(mod_a)
 

# below_roads_hf_b %>% 
#    group_by(post_election_flag, pred_id) %>%
#     summarize(n = n())    

       # mutate(treat_flag = 1, # Above means these areas are above the cutoff for % votes against i.e. cut their renewal taxes 
       #                       road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
       #                       road_quality_score2 = case_when(
       #                              predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
       #                              TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
       #                       ) |> round(1) ,
       #                       road_quality_score3 = case_when(
       #                              predicted_label == 0 ~ (1 - 0.8*confidence) * 99 / 3 ,
       #                              TRUE                 ~ predicted_label * 99 / 3  + 0.8*confidence * 99 / 3         ) |> round(1) 
       # )  %>%
       # mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
       #                       year = ifelse(nchar(year) == 2, paste0("20", year), year),
       #                       year = as.integer(year))

below_roads_hf <- readr::read_csv(paste0(data,"/roads/hf_finetuned_convnextv2/ohio_preds/ohio_below_preds.csv"))  %>%
       mutate(
              image = basename(image_path),
              year = as.integer(str_extract(image, "\\d{2}(?=\\.jpg$)") %>% paste0("20", .)),
              tendigit_fips = as.numeric(str_extract(image, "\\d{10}"))
       ) %>% 
  mutate(road_quality_score = round(((pred_label + max_prob) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           pred_label == 0 ~ 1 + (1 - 0.5*max_prob) * 99 / 3 ,
           TRUE                 ~ 1 + pred_label * 99 / 3  + 0.5*max_prob * 99 / 3 
         ) |> round(1) ,
         road_quality_score3 = case_when(
           pred_label == 0 ~ (1 - 0.8*max_prob) * 99 / 3 ,
           TRUE                 ~ pred_label * 99 / 3  + 0.8*max_prob * 99 / 3         ) |> round(1) 
  ) 
       # mutate(treat_flag = 0,
       #                       road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
       #                       road_quality_score2 = case_when(
       #                              predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
       #                              TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
       #                       ) |> round(1),
       #                       road_quality_score3 = case_when(
       #                              predicted_label == 0 ~ (1 - 0.8*confidence) * 99 / 3 ,
       #                              TRUE                 ~ predicted_label * 99 / 3  + 0.8*confidence * 99 / 3         ) |> round(1) 
       # ) %>%
       # mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
       #                       year = ifelse(nchar(year) == 2, paste0("20", year), year),
       #                       year = as.integer(year))

closest_votes_b <- closest_votes %>% filter(treated == 0) %>% arrange(tendigit_fips, votes_pct_against) %>% distinct(tendigit_fips, .keep_all = TRUE) %>% 
       rename(election_year=year)

below_roads_hf_b <- below_roads_hf %>% left_join(closest_votes_b, by = "tendigit_fips") %>% arrange(tendigit_fips, year) %>%
                     mutate(post_election_flag = ifelse(year > election_year, 1, 0)) %>%
       group_by(tendigit_fips, election_year) %>%
       filter(any(post_election_flag == 0) & any(post_election_flag == 1)) %>% # atleast have one pre and one post image
       ungroup()


# roads_close_hf <- bind_rows(above_roads_hf_a, below_roads_hf_b %>% filter(max_prob > 0.5)) %>% mutate(did = post_election_flag*treated)
roads_close_hf <- bind_rows(above_roads_hf_a, below_roads_hf_b ) %>% mutate(did = post_election_flag*treated)

# View(roads_close_hf)

rq_summary <- roads_close_hf %>% group_by(treated, post_election_flag) %>%
    summarize(n = n(), mean_rq = mean(pred_id), sd_rq = sd(pred_id), 
              mean_rqs1 = mean(road_quality_score), sd_rqs1 = sd(road_quality_score),
              mean_rqs2 = mean(road_quality_score2), sd_rqs2 = sd(road_quality_score2),
              mean_rqs3 = mean(road_quality_score3), sd_rqs3 = sd(road_quality_score3)
              )
# print(rq_summary, width=Inf)

# Create the plot
sp <- ggplot(rq_summary, aes(x = post_election_flag, y = mean_rq, color = factor(treated))) +
       geom_line(aes(group = treated), size = 1.2) +
       geom_point(size = 3) +
       geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
       scale_color_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                                                                       labels = c("Control", "Treated"),
                                                                       name = "Group") +
       scale_x_continuous(breaks = c(0, 1), labels = c("Pre-Election", "Post-Election")) +
       labs(x = "Election Period",
                      y = "Mean Road Quality Score",
                      title = "Road Quality Before and After Elections",
                      subtitle = "Treatment vs Control Groups") +
       theme_minimal() +
       theme(panel.grid.minor = element_blank(),
                            panel.grid.major.x = element_blank(),
                            legend.position = "bottom",
                            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
                            axis.title = element_text(size = 11),
                            legend.title = element_text(size = 10)) +
       ylim(c(1.4, 1.65))


ggsave(paste0(plots, "/road_quality_lineplot.png"), plot = sp, width = 8, height = 6, dpi = 300)


# Create bar plot
bp <- ggplot(rq_summary, aes(x = factor(treated), y = mean_rq, fill = factor(post_election_flag))) +
       geom_col(position = "dodge", alpha = 0.8) +
       scale_fill_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                                                                      labels = c("0" = "Pre-Election", "1" = "Post-Election"),
                                                                      name = "Election Period") +
       scale_x_discrete(labels = c("0" = "Control", "1" = "Treated")) +
       labs(x = "Group",
                            y = "Mean Road Quality Score",
                            title = "Mean Road Quality by Treatment and Election Period") +
       theme_minimal() +
       theme(legend.position = "bottom",
                            plot.title = element_text(hjust = 0.5, face = "bold")) +
                            ylim(c(0.0, 3.0))
print(bp)
ggsave(paste0(plots, "/road_quality_barplot.png"), plot = bp, width = 8, height = 6, dpi = 300)


#===================================#
## RD analysis of roads ##
#===================================#

roads_close_hf2 <- roads_close_hf %>% filter(post_election_flag == 1)
# mean(roads_close_hf$pred_id)
# roads_close_hf %>%
#    group_by(treated) %>%
#     summarize(n = n(), mean = mean(pred_id), sd = sd(pred_id))

rdrobust_results_hf <- rdrobust(y = roads_close_hf2$pred_id,
       x = roads_close_hf2$votes_pct_against,
       c = cutoff,
       covs = roads_close_hf2$pop,
       all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2, # uniform weights because these are already close elections i.e. randomization has already happened.
       h = max(abs(roads_close_hf2$votes_pct_against - cutoff), na.rm = TRUE), # full because these are already close elections
       cluster=roads_close_hf2$tendigit_fips
       )

summary(rdrobust_results_hf)

# rdrobust_results_hf %>% 

# Create RD plot
rdplot_hf <- rdplot(y = roads_close_hf2$pred_id,
                               x = roads_close_hf2$votes_pct_against,
                               c = cutoff,
                               p = 1,
                               kernel = "uniform",
                               h = max(abs(roads_close_hf2$votes_pct_against - cutoff), na.rm = TRUE),
                               # nbins = c(10, 10),  # specify number of bins on left and right of cutoff
                               nbins = 4,
                               binselect = "esmv",  # binning method: "es", "espr", "esmv", "qs", "qspr", "qsmv"
                               title = "Road Quality vs Vote Share Against Tax Renewal",
                               x.label = "Vote Share Against Tax Renewal (%)",
                               y.label = "Road Quality Rating",
                               y.lim = c(1.0, max(roads_close_hf2$pred_id, na.rm = TRUE)))

# Save the plot
ggsave(paste0(plots, "/rd_plot_road_quality_hf4.png"), plot = rdplot_hf$rdplot, 
          width = 10, height = 6, dpi = 300)


# RD regressions: Road Quality vs Vote Share Against Tax Renewal
rdrobust_results_hf <- rdrobust(y = roads_close_hf2$pred_id,
       x = roads_close_hf2$votes_pct_against,
       c = cutoff,
       covs = roads_close_hf2$pop,
       all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2, # uniform weights because these are already close elections i.e. randomization has already happened.
       h = max(abs(roads_close_hf2$votes_pct_against - cutoff), na.rm = TRUE), # full because these are already close elections
       cluster=roads_close_hf2$tendigit_fips
       )
summary(rdrobust_results_hf)
mean(roads_close_hf[roads_close_hf$treated == 1 & roads_close_hf$post_election_flag == 0,]$pred_id)

rdrobust_results_hf2 <- rdrobust(y = roads_close_hf2$road_quality_score3,
       x = roads_close_hf2$votes_pct_against,
       c = cutoff,
       covs = roads_close_hf2$pop,
       all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2, # uniform weights because these are already close elections i.e. randomization has already happened.
       h = max(abs(roads_close_hf2$votes_pct_against - cutoff), na.rm = TRUE), # full because these are already close elections
       cluster=roads_close_hf2$tendigit_fips
       )
summary(rdrobust_results_hf2)
mean(roads_close_hf[roads_close_hf$treated == 1 & roads_close_hf$post_election_flag == 0,]$road_quality_score3)

