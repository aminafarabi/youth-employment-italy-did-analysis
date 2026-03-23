library(fixest)
library(tidyverse)
library(broom)

rm(list=ls())
did_data <- read_csv("output/italy_lfs_did_analysis_data.csv")

# ------------------------------------------------------------------------------
# create event study data
# ------------------------------------------------------------------------------

event_data <- did_data %>%
  filter(year %in% c(2004, 2005, 2006, 2007, 2008, 2009, 2010)) %>%
  mutate(
    event_time = case_when(
      year == 2004 ~ -4,
      year == 2005 ~ -3,
      year == 2006 ~ -2,
      year == 2007 ~ -1,
      year == 2008 ~ 0,# reference year
      year == 2009 ~ 1,
      year == 2010 ~ 2
    ),
    event_factor = factor(event_time)
  )

model_event <- feols(employed ~ treat + i(event_time, treat, ref = 0) + 
                       female + age,
                     data = event_data,
                     weights = ~weight_norm,
                     vcov = "hetero")

summary(model_event)


png("output/event_study_plot.png", 
    width = 8, 
    height = 5, 
    units = "in", 
    res = 300)

iplot(model_event,
      ci_level = 0.95,
      pt.join = TRUE,
      ref.line = 0,
      main = "",
      xlab = "Years Relative to Crisis (2008)",
      ylab = "Difference (Young - Prime-age)")

dev.off()





