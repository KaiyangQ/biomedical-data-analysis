# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW8")

#1a
library(tidyverse)
library(tidyr)
library(dplyr)
library(geepack) 

data(seizure)
seizure_long = seizure %>%
  mutate(id = row_number(), # data is in wide format and we don't have an id variable
         trt = factor(trt, levels = 0:1, labels = c("placebo", "progabide"))) %>%
  pivot_longer(
    cols = y1:y4,
    names_to = "time",
    values_to = "num_seizures"
  ) %>%
  mutate(time = as.numeric(str_remove(time, "y")))
length(unique(seizure_long$id)) # number of subjects
table(seizure_long$time) # number of subjects per visit


#1b
seizure_long %>%
  ggplot(aes(time, log(num_seizures))) +
  geom_line(alpha = 0.3, aes(group = id) )+
  facet_wrap(~ trt) +
  geom_smooth(method = "lm", se = FALSE)


seizure_long %>%
  ggplot(aes(log(base), log(num_seizures))) +
  geom_point() +
  facet_wrap(~time, nrow = 1)


#1c
library(GGally)

seizure_residuals = seizure_long %>%
  group_by(time) %>%
  mutate(mean_over_time = mean(log(num_seizures + 1))) %>%
  ungroup() %>%
  mutate(residuals = log(num_seizures + 1) - mean_over_time) %>%
  select(id, time, residuals)
seizure_residuals_wide = seizure_residuals %>%
  pivot_wider(names_from = time,
              names_glue = paste0("time_", "{time}"),
              values_from = residuals) %>%
  select(-id)
ggpairs(seizure_residuals_wide)