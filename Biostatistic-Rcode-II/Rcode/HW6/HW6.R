# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW6")

# Load the data
data <- read.csv("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW6/skincancer-3.csv")

# Create an indicator variable for Dallas
data$Dallas <- ifelse(data$city == "Dallas", 1, 0)

model <- glm(cases ~ Dallas + offset(log(py1000)), family = poisson, data = data)

summary(model)
confint(model)

library(tidyverse)

tidy_output <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

print(tidy_output)


#2a
data$age.group <- as.factor(data$age.group)

model_age_adjusted <- glm(cases ~ Dallas + age.group + offset(log(py1000)), 
                          family = poisson, data = data)

summary(model_age_adjusted)

tidy_model_age_adjusted <- broom::tidy(model_age_adjusted, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model_age_adjusted)

#3a
load("/Users/kaiyangqian/Downloads/myomectomy-1.Rdata")
ls()
# myo_data
# write.csv(myo_data, "myo_data.csv", row.names = FALSE)
library(ggplot2)

# Load the data
#myo_data <- read.csv("path/to/your/myo_data.csv")

# Plot the distribution of operation times by surgical approach
ggplot(myo_data, aes(x = optime, fill = approach)) +
  geom_histogram(alpha = 0.6, position = 'identity', binwidth = 5) +
  labs(title = "Distribution of Operation Times by Surgical Approach",
       x = "Operation Time (minutes)",
       y = "Count") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Abdominal", "Laparoscopic"),
                    name = "Surgical Approach") +
  theme_minimal()

#3d
myo_data$racegrp <- factor(myo_data$racegrp)
myo_data$racegrp <- relevel(myo_data$racegrp, ref = "other")
poisson_model <- glm(optime ~ age + bmi + approach + racegrp, family = poisson, data = myo_data)

summary(poisson_model)
tidy_output <- broom::tidy(poisson_model, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

print(tidy_output)


#3e
new_data <- data.frame(age = 36,
                       bmi = 27,
                       approach = factor("Abdominal"),
                       racegrp = factor("other"))

log_expected_time <- predict(poisson_model, newdata = new_data, type = "response")

expected_time <- log_expected_time

print(expected_time)


#3f
#install.packages("performance")
library(performance)
overdispersion_result <- check_overdispersion(poisson_model)
print(overdispersion_result)


# Assuming your data is in a dataframe called myo_data
# and the outcome variable is optime

# Load necessary package
library(dplyr)
summary_table <- myo_data %>%
  group_by(approach,racegrp) %>% 
  summarise(
    Mean = mean(optime, na.rm = TRUE),
    Variance = var(optime, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(approach) %>%
  head() %>%
  knitr::kable(digits = 2)

print(summary_table)


#3g
library(MASS)

nb_model <- glm.nb(optime ~ age + bmi + approach + racegrp, data = myo_data)

summary(nb_model)

tidy_output_new <- broom::tidy(nb_model, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

print(tidy_output_new)

anova(poisson_model, nb_model, test = "LRT")