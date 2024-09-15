getwd()
setwd("D:/Homework")

install.packages('readr')
install.packages("Matrix")
install.packages("lme4")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the data
data <- read_csv("Final_Project_Data.csv")


# Part 0 : Data Exploration
age_summary <- summary(data$fam_age)

# Frequency counts for categorical variables
hispanic_summary <- table(data$fam_hispanic)
race_summary <- table(data$fam_race)
female_summary <- table(data$fam_female)

# Print summaries
print(age_summary)
print(hispanic_summary)
print(race_summary)
print(female_summary)



# Drop NA values for IES for plotting
data_filtered  <- na.omit(data[c("IES", "followup","fam_female")])

# Load necessary libraries
library(ggplot2)

# Plotting distribution of IES scores by follow-up time using separate plots for each time with fixed scales
p1 <- ggplot(data, aes(x = IES, fill = factor(followup))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_brewer(palette = "Set1", name = "Follow-up Time (months)") +
  labs(title = "IES Score Distribution by Follow-up Time",
       x = "IES Score", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~followup, scales = "fixed")  # Use fixed scales for consistent y-axis across facets

# Print the plot
print(p1)



average_IES <- data %>%
  group_by(followup) %>%
  summarise(mean_IES = mean(IES, na.rm = TRUE))

p2 <- ggplot(average_IES, aes(x = followup, y = mean_IES, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(3, 6, 12)) +
  labs(title = "Average IES Scores Over Time",
       x = "Follow-up Time (months)",
       y = "Average IES Score") +
  theme_minimal()

# Print the plot
print(p2)



# Part 1 : Research Question 1


install.packages("Matrix", dependencies = TRUE)

library(lme4)
# Assuming 'IES' as the response variable and 'followup' as a fixed effect, and 'id' as a random effect
model_lmm <- lmer(IES ~ followup + (1|id), data = data)
summary(model_lmm)


update.packages(checkBuilt = TRUE, ask = FALSE)


version
# Part 2 : Research Question 2

