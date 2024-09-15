# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW3")


byssinosis_data <- read.csv("byssinosis.csv")

model_unadjusted <- glm(as.factor(byss) ~ as.factor(smoke), data = byssinosis_data, family = "binomial")
summary(model_unadjusted)

confint(model_unadjusted, level = 0.95)


# b
model_adjusted <- glm(as.factor(byss) ~ as.factor(smoke) + as.factor(expose), data = byssinosis_data, family = "binomial")
summary(model_adjusted)

confint(model_adjusted, level = 0.95)

#c
model_covariate <- glm(as.factor(expose) ~ as.factor(smoke), data = byssinosis_data, family = "binomial")
summary(model_covariate)

#e
# Logistic regression with interaction term, correctly specified
model_interaction <- glm(as.factor(byss) ~ smoke + expose + smoke:expose, 
                         data = byssinosis_data, family = "binomial")
summary(model_interaction)


odds_ratio_smokers_unexposed <- exp(coef(model_interaction)["smokeSmoker"])

print(odds_ratio_smokers_unexposed)

conf_int_smokers_unexposed <- exp(confint(model_interaction)["smokeSmoker", ])

print(conf_int_smokers_unexposed)


#exposed
combined_odds_ratio_smokers_exposed <- exp(coef(model_interaction)["smokeSmoker"] + coef(model_interaction)["smokeSmoker:exposeYes"])
print(combined_odds_ratio_smokers_exposed)

conf_int_smokers_exposed <- exp(confint(model_interaction))
print(conf_int_smokers_exposed)



#question2
# Assuming 'byssinosis_data' is your data frame and 'dbyss' is the outcome variable

# Load necessary package
library(stats)

# 1. Intercept Only Model
model_intercept <- glm(dbyss ~ 1, family = binomial(), data = byssinosis_data)
minus2_log_likelihood_intercept <- -2 * logLik(model_intercept)
print(paste("-2 Log Likelihood for Intercept Only Model:", minus2_log_likelihood_intercept))

# 2. Model with Smoking Status as the Only Predictor
model_smoking <- glm(dbyss ~ smoker, family = binomial(), data = byssinosis_data)
minus2_log_likelihood_smoking <- -2 * logLik(model_smoking)
print(paste("-2 Log Likelihood for Model with Smoking Status:", minus2_log_likelihood_smoking))


# 3. Model with Smoking Status and Dust Exposure as the Predictors
model_smoke_dust <- glm(dbyss ~ smoker + dust, family = binomial(), data = byssinosis_data)
minus2_log_likelihood_smoke_dust <- -2 * logLik(model_smoke_dust)
print(paste("-2 Log Likelihood for Model with Smoking Status and Dust Exposure:", minus2_log_likelihood_smoke_dust))


#4
lrt_statistic <- -2 * (logLik(model_smoking) - logLik(model_smoke_dust))

df <- length(coef(model_smoke_dust)) - length(coef(model_smoking))

p_value <- pchisq(lrt_statistic, df, lower.tail = FALSE)

print(paste("Likelihood Ratio Test Statistic:", lrt_statistic))
print(paste("P-value:", p_value))

#d
anova(model_smoking,model_smoke_dust,test = 'LRT')

#e
anova(model_intercept,model_smoke_dust,test = 'LRT')


#f
lrt_statistic <- 1213.318 - 1210.808

p_value <- pchisq(lrt_statistic, df = 1, lower.tail = FALSE)

print(paste("LRT Statistic:", lrt_statistic))
print(paste("P-value:", p_value))

