# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW8")

install.packages("geepack")  # Install geepack package
library(geepack)             # Load geepack package

data("seizure", package = "geepack")  # Load the seizure dataset
data("dietox", package = "geepack")  # Load the seizure dataset
dietox
seizure
#1a
library(tidyverse)
library(tidyr)
library(dplyr)

# Transform the data to a long format
seizure_long <- seizure %>% 
  pivot_longer(
    cols = starts_with("y"),
    names_to = "time",
    names_prefix = "y",
    values_to = "seizures"
  )

# Add an identifier for each subject assuming each has exactly 4 records
seizure_long <- seizure_long %>%
  group_by(base, trt, age) %>%  # Assuming base, trt, and age uniquely identify a subject
  mutate(subject_id = cur_group_id()) %>%
  ungroup()

# Print the long-format dataset to confirm the structure
print(seizure_long, n = 236)

# Examine the structure of the long-format dataset
str(seizure_long)

# Look at the first few rows to ensure the transformation was successful
head(seizure_long)


table(seizure_long$trt)
table(seizure_long$time)

# Assuming your data frame is named seizure_long
write.csv(seizure_long, "seizure_long.csv", row.names = FALSE)


#1b
# Calculate average number of seizures at each time point, grouped by treatment
average_seizures_time_trt <- seizure_long %>%
  group_by(time, trt) %>%
  summarise(mean_seizures = mean(seizures, na.rm = TRUE), .groups = 'drop')
# Plot
ggplot(average_seizures_time_trt, aes(x = time, y = mean_seizures, group = trt, color = as.factor(trt))) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Seizures Over Time by Treatment",
       x = "Time Point",
       y = "Average Seizures",
       color = "Treatment") +
  theme_minimal()

ggplot(seizure_long, aes(x = age, y = seizures, color = as.factor(trt))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Seizures by Age and Treatment", x = "Age", y = "Seizures", color = "Treatment") +
    theme_minimal()

seizure_long %>%
  ggplot(aes(time, seizures)) +
  geom_line(alpha = 0.3, aes(group = subject_id)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ trt) +
  labs(x = "Time", y = "Seizures")




#1c
library(GGally)
seizure_residuals <- seizure_long %>%
  group_by(time) %>%
  mutate(weekly_mean = mean(seizures)) %>%
  ungroup() %>%
  mutate(residuals = seizures - weekly_mean) %>%
  select(subject_id, time, residuals)

# Pivot the data to wide format for visualization
seizure_residuals_wide <- seizure_residuals %>%
  pivot_wider(
    names_from = time,
    names_glue = paste0("time point","{time}"),
    values_from = residuals) %>%
  select(-subject_id)

ggpairs(seizure_residuals_wide[, 1:4])




#2b
library(geepack)

model <- geeglm(seizures ~ trt + age + time, 
                family = poisson, 
                data = seizure_long, 
                id = subject_id,
                corstr = "exchangeable")
summary(model)


#2d
library(tidyverse)

tidy_output <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

print(tidy_output)


#3b
library(lme4)
poisson_glmm <- glmer(seizures ~ trt + age + time + (1 | subject_id), 
                      family = poisson, 
                      data = seizure_long)
summary(poisson_glmm)


#3c

fixef_poisson_glmm <- fixef(poisson_glmm)

exp_fixef_poisson_glmm <- exp(fixef_poisson_glmm)

print(exp_fixef_poisson_glmm)

conf_int_log <- confint(poisson_glmm)

conf_int_exp <- exp(conf_int_log)

print(conf_int_exp)


#3d
library(performance)
overdispersion_result <- check_overdispersion(poisson_glmm)
print(overdispersion_result)