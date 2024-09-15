# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW5")

library(broom)

# Load the data
data <- read.csv("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW5/breastcancer.csv")

library(nnet)
data$age <- as.factor(data$age)
data$arm <- as.factor(data$arm)
data$response <- as.factor(data$response)

model <- multinom(response ~ arm + age, data = data)
summary(model)

exp_coefficients_arm2 <- exp(-0.6626597)
exp_coefficients_arm3 <- exp(-0.8866734)
exp_coefficients_arm4 <- exp(-1.3551808)
exp_coefficients_arm2
exp_coefficients_arm3
exp_coefficients_arm4

#1d
tidy_summary <- tidy(model)
print(tidy_summary)

#1f
data$response <- relevel(data$response, ref = "3")  

modelf <- multinom(response ~ arm + age, data = data)

tidy_summary1 <- tidy(modelf, exp = TRUE, conf.int = TRUE)
print(tidy_summary1)

#1e
MASS::dropterm(modelf, test = "Chisq")

#2c
if (!require(MASS)) install.packages("MASS")
library(MASS)

data$response <- factor(data$response, levels = c(1, 2, 3, 4), ordered = TRUE)
data$arm <- factor(data$arm)
data$clinical_stage <- factor(data$clinical_stage)

modelc <- polr(response ~ arm + clinical_stage, data = data, Hess = TRUE)

summary(modelc)

#2d
if (!require(brant)) install.packages("brant")
library(brant)
brant(modelc)

#2e
coef_summary <- summary(modelc)$coefficients
odds_ratios <- exp(coef_summary[,1])  
odds_ratios

p_value <- (1 - pnorm(abs(summary(model)$coefficients[, "t value"]), 0, 1)) * 2
p_value

conf_int <- confint(model)
conf_int