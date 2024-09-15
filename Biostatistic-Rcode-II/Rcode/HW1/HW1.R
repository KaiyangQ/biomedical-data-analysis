# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW1")

#1a
library(ggplot2)

# Read the dataset
data <- read.csv("lane_departures.csv")

# Relationship between condition and departures
ggplot(data, aes(x = condition, y = departures)) +
  geom_boxplot() +
  labs(title = "Lane Departures by Marijuana Use Group",
       x = "Condition",
       y = "Number of Lane Departures") 

# Plot for relationship between initial speed and departures
ggplot(data, aes(x = init_spd, y = departures)) +
  geom_point() +
  labs(title = "Lane Departures vs. Initial Speed",
       x = "Initial Speed (MPH)",
       y = "Number of Lane Departures")


model <- lm(departures ~ condition + init_spd, data = data)

summary(model)