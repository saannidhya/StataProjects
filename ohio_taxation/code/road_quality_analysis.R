#================================================================================================================#
# Purpose : Road Quality Analysis
# Name    : Saani Rawat
# Log     : 1. 1/12/2025: made a more formal update to code. Added Qs that each snippet answers.
#           2. 1/16/2025: results from test run of fine-tuned gpt-4 mode
#           3. 2/26/2025: Added regression analysis for Road quality
#================================================================================================================#

library(fixest)
library(MASS) 
library(tidyverse)

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")


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

# above_roads_hf$image_path
# Import the new prediction files
above_roads_hf <- readr::read_csv(paste0(data,"/roads/hf_finetuned_convnextv2/ohio_preds/ohio_above_preds.csv"))  
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

below_roads_hf <- readr::read_csv(paste0(data,"/roads/hf_finetuned_convnextv2/ohio_preds/ohio_below_preds.csv")) 
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

roads_close_hf <- bind_rows(above_roads_hf, below_roads_hf) %>% mutate(did = post_election_flag*treat_flag)

# Group means 
roads_close_hf %>%
       group_by(treat_flag, post_election_flag) %>%
       summarize(mean = mean(predicted_label), sd = sd(predicted_label) ,
                                          mean_score = mean(road_quality_score), sd_score = sd(road_quality_score) ,
                                          mean_score2 = mean(road_quality_score2), sd_score2 = sd(road_quality_score2) ,
                                          mean_score3 = mean(road_quality_score3), sd_score3 = sd(road_quality_score3) ,
                                          # ,n = n()
       ) 

# Before and after: treatment group
road_above_hf_lm <- lm(data = above_roads_hf, formula = predicted_label ~ post_election_flag)
summary(road_above_hf_lm)

# Before and after: control group
road_below_hf_lm <- lm(data = below_roads_hf, formula = predicted_label ~ post_election_flag)
summary(road_below_hf_lm)

# Before and after: treatment group (road quality score2)
road_above_rqs_hf_lm <- lm(data = above_roads_hf, formula = road_quality_score2 ~ post_election_flag)
summary(road_above_rqs_hf_lm)

# Before and after: control group (road quality score2)
road_below_rqs_hf_lm <- lm(data = below_roads_hf, formula = road_quality_score2 ~ post_election_flag)
summary(road_below_rqs_hf_lm)


# print(above_roads, width = Inf)

above_roads_hf <- above_roads_hf %>%
       # mutate(image = tools::file_path_sans_ext(basename(image_path))) %>%
       mutate(image = basename(image_path)) %>%
       left_join(above_roads %>% dplyr::select(image, tendigit_fips, year, election_year, post_election_flag), by = "image") %>%
       filter(!is.na(tendigit_fips))

# View(above_roads_hf)
# colnames(above_roads)
# above_roads$image

below_roads_hf <- below_roads_hf %>%
       # mutate(image = tools::file_path_sans_ext(basename(image_path))) %>%
       mutate(image = basename(image_path)) %>%
       left_join(below_roads %>% dplyr::select(image, tendigit_fips, year, election_year, post_election_flag), by = "image") %>%
       filter(!is.na(tendigit_fips))

# before and after: treatment group
road_above_lm_hf <- lm(data = above_roads_hf, formula = pred_label ~ post_election_flag)
summary(road_above_lm_hf)
# before and after: control group
road_below_lm_hf <- lm(data = below_roads_hf, formula = pred_label ~ post_election_flag)
summary(road_below_lm_hf)

