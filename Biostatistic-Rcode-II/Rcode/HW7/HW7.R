# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW7")
#write.csv(data, "exercise_therapy.csv", row.names = FALSE)

library(foreign)
data <- read.dta("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW7/exercise_therapy.dta")
#1a
# Spaghetti plot for Treatment 1 
ggplot(data[data$trt == 1, ], aes(x = time, y = y, group = id, color = as.factor(id))) +
  geom_line() + 
  geom_point() +
  labs(title = "Treatment 1: Increased Repetitions", x = "Time (days)", y = "Strength") +
  theme_minimal() +
  scale_color_viridis_d(name = "Subject ID") 

# Spaghetti plot for Treatment 2 
ggplot(data[data$trt == 2, ], aes(x = time, y = y, group = id, color = as.factor(id))) +
  geom_line() + 
  geom_point() +
  labs(title = "Treatment 2: Increased Weight", x = "Time (days)", y = "Strength") +
  theme_minimal() +
  scale_color_viridis_d(name = "Subject ID") 

#1b
library(ggplot2)

# Plot of smoothed means for the two groups
ggplot(data, aes(x = time, y = y, color = as.factor(trt))) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Smoothed Means of Strength Over Time by Treatment Group",
       x = "Time (days)",
       y = "Strength",
       color = "Treatment") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Program 1 (Increased Repetitions)", "Program 2 (Increased Weight)")) +
  theme_minimal()


#1c
ggplot(data, aes(x = as.factor(time), y = y, fill = as.factor(trt))) +
  geom_boxplot() +
  labs(title = "Distribution of Strength Outcomes Over Time by Treatment Group",
       x = "Time (days)",
       y = "Strength",
       fill = "Treatment") +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("Program 1 (Increased Repetitions)", "Program 2 (Increased Weight)")) +
  theme_minimal() 


#2c
if(!require(lme4)) install.packages("lme4")
library(lme4)

model <- lmer(y ~ time * trt + (1 | id), data = data)

summary(model)

#2d
a <- 10.678
b <- 1.212
ICC <- a/(a+b)
ICC

if(!require(lmerTest)) install.packages("lmerTest")
library(lmerTest)

model <- lmer(y ~ time * trt + (1 | id), data = data)
summary(model)

#3a
model2 <- lmer(y ~ time + trt + (time | id), data = data)

summary(model2)


#3b
