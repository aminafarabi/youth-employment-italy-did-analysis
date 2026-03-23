library(fixest)
library(modelsummary)
library(tidyverse)

rm(list=ls())
all_years_did_data <- read_csv("output/italy_lfs_did_analysis_data.csv")

did_data <- all_years_did_data %>%
  filter(
    year %in% c(2006,2007,2009,2010)
  )
#-------------------------------------------------------------------------------
# basic did regression model
#-------------------------------------------------------------------------------
model_basic <- feols(employed ~ treat + post + did, 
                     data = did_data,
                     weights = ~weight_norm,
                     vcov = "hetero")

#-------------------------------------------------------------------------------
# did regression model with demographics control
#-------------------------------------------------------------------------------
model_demo <- feols(employed ~ treat + post + did + female + age,
                    data = did_data,
                    weights = ~weight_norm,
                    vcov = "hetero")

# ------------------------------------------------------------------------------
# alternative outcomes
# ------------------------------------------------------------------------------

employed_data <- did_data %>% 
  filter(employed == 1, !is.na(hwusual))

# working hours
model_hours <- feols(hwusual ~ treat + post + did + female + age + part_time,
                     data = employed_data,  # fixed: was employed_df
                     weights = ~weight_norm,
                     vcov = "hetero")

# part-time
model_parttime <- feols(part_time ~ treat + post + did + female + age,
                        data = employed_data,
                        weights = ~weight_norm,
                        vcov = "hetero")

# ------------------------------------------------------------------------------
# triple difference (ddd)
# ------------------------------------------------------------------------------

model_ddd <- feols(employed ~ treat * post * female + age,
                   data = did_data,
                   weights = ~weight_norm,
                   vcov = "hetero")

# ------------------------------------------------------------------------------
# placebo test
# ------------------------------------------------------------------------------

placebo_data <- all_years_did_data %>%
  filter(year %in% c(2004, 2005, 2006, 2007)) %>%
  mutate(
    # we pretend that the crisis happened in 2006 (~-.-)~
    placebo_post = if_else(year >= 2006, 1, 0),
    placebo_did = treat * placebo_post
  )

model_placebo <- feols(employed ~ treat + placebo_post + placebo_did + 
                         female + age,
                       data = placebo_data,
                       weights = ~weight_norm,
                       vcov = "hetero")

# ------------------------------------------------------------------------------
# create grouped summary table
# ------------------------------------------------------------------------------

models_list <- list(
  "(1) Basic DID" = model_basic,
  "(2) + Demographics" = model_demo,
  "(3) Hours Worked" = model_hours,
  "(4) Part-time" = model_parttime,
  "(5) Triple Diff (DDD)" = model_ddd,
  "(6) Placebo Test" = model_placebo
)

modelsummary(
  models_list,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_rename = c(
    "treat" = "Young (15-24)",
    "post" = "Post-crisis",
    "did" = "Young × Post-crisis",
    "female" = "Female",
    "age" = "Age",
    "part_time" = "Part-time",
    "placebo_post" = "Placebo Post",
    "placebo_did" = "Young × Placebo",
    "treat:post" = "Young × Post-crisis",
    "treat:female" = "Young × Female",
    "post:female" = "Post × Female",
    "treat:post:female" = "Young × Post × Female"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Table 1: Effect of the 2008 Crisis on Youth Employment in Italy",
  notes = "Dependent variable: employed (0/1) for models 1-2, 5-6; usual hours worked for model 3; part-time indicator for model 4. Heteroskedasticity-robust standard errors. Sampling weights applied. *** p<0.01, ** p<0.05, * p<0.1."
)


#-------------------------------------------------------------------------------
# some other models to check the pre-trend better
#-------------------------------------------------------------------------------
did_data_2007 <- did_data %>%
  filter(year %in% c(2007, 2009, 2010)) %>%
  mutate(
    post = if_else(year >= 2009, 1, 0),
    did = treat * post
  )

model_2007_baseline <- feols(employed ~ treat + post + did + female + age,
                             data = did_data_2007,
                             weights = ~weight_norm,
                             vcov = "hetero")

modelsummary(
  list("Original (2006-07)" = model_demo,
       "2007 Baseline" = model_2007_baseline,
       "Model with trend" = model_with_trend
       ),
  stars = TRUE,
  coef_rename = c("did" = "Young × Post-crisis")
)

did_data <- did_data %>%
  mutate(time_trend = year - 2004)

model_with_trend <- feols(employed ~ treat + post + did + 
                            treat * time_trend +
                            female + age,
                          data = did_data,
                          weights = ~weight_norm,
                          vcov = "hetero")

modelsummary(
  list("Model with trend" = model_with_trend),
  stars = TRUE,
  coef_rename = c("did" = "Young × Post-crisis")
)
