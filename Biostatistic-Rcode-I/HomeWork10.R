getwd()
setwd("/Users/kaiyangqian/Rcode")
lead <- read.csv('lead2.csv')

# 1a
model <- lm(iq ~ expose, data = lead)
summary(model)
confint(model)


#1b
# Model adjusted
model_adjusted <- lm(iq ~ expose + sex, data=lead)

# Summary
summary(model_adjusted)

# %95 CI
confint(model_adjusted)

# Define the crude and adjusted coefficients for lead exposure
beta_crude <- -7.77
beta_adj <- -7.916

# Calculate the percent change using the method favored by biostatisticians
percent_change_biostatisticians <- ((beta_crude - beta_adj) / beta_crude) * 100

# Calculate the percent change using the method favored by epidemiologists
percent_change_epidemiologists <- ((beta_crude - beta_adj) / beta_adj) * 100

# Print the results
print(paste("Percent change (Biostatisticians):", percent_change_biostatisticians))
print(paste("Percent change (Epidemiologists):", percent_change_epidemiologists))



#2a
# Model adjusted
adjusted <- lm(iq ~ expose + resdur, data=lead)

#Summary
summary(adjusted)


# Model Covariate
adjusted_covariate <- lm(resdur ~ expose, data=lead)

#Summary
summary(adjusted_covariate)

#2b
proportion <- (-7.770 -(-7.6358)) / (-7.770) 
proportion


#2c
# Define the coefficients and standard errors
gamma_X <- 0.1683
beta_M <- -0.7994
se_gamma_X <- 0.6455
se_beta_M <- 0.4021

# Calculate the Sobel test statistic
sobel_test_statistic <- (gamma_X * beta_M) / sqrt((se_gamma_X^2 * beta_M^2) + (gamma_X^2 * se_beta_M^2))

# Calculate the p-value from the Sobel test statistic
p_value <- 2 * pnorm(sobel_test_statistic)

# Calculate the 95% CI for the Sobel test statistic
# Assuming a normal distribution, the 95% CI would be the statistic plus or minus 1.96 times its standard error
ci_lower <- sobel_test_statistic - (1.96 * sqrt((se_gamma_X^2 * beta_M^2) + (gamma_X^2 * se_beta_M^2)))
ci_upper <- sobel_test_statistic + (1.96 * sqrt((se_gamma_X^2 * beta_M^2) + (gamma_X^2 * se_beta_M^2)))

# Print the results
print(paste("Sobel test statistic:", sobel_test_statistic))
print(paste("P-value for the Sobel test:", p_value))
print(paste("95% CI lower bound:", ci_lower))
print(paste("95% CI upper bound:", ci_upper))


#2d
# Samples I want to take
bootstrap_samples <- 1000
bootstrap_estimates <- numeric(bootstrap_samples)

for(i in 1:bootstrap_samples) {
  sample_indices <- sample(1:nrow(lead), replace = TRUE)
  bootstrap_sample <- lead[sample_indices, ]
  
  # Fit your mediator and outcome models to the bootstrap sample
  mediator_model <- lm(resdur ~ expose, data = bootstrap_sample)
  outcome_model <- lm(iq ~ expose + resdur, data = bootstrap_sample)
  
  # Compute the indirect effect for the bootstrap sample and store it
  bootstrap_estimates[i] <- summary(mediator_model)$coefficients["expose", "Estimate"] * summary(outcome_model)$coefficients["resdur", "Estimate"]
}

# Compute the 95% confidence interval from the bootstrap estimates
ci_lower <- quantile(bootstrap_estimates, probs = 0.025)
ci_upper <- quantile(bootstrap_estimates, probs = 0.975)

print(ci_lower)
print(ci_upper)



#3a
# Run the linear regression
model <- lm(iq ~ miles + first2y + miles*first2y, data=lead)

# Display the summary of the model
summary(model)


#3e
library(ggplot2)

# Create a scatterplot with different symbols and regression lines
ggplot(lead, aes(x = miles, y = iq, color = factor(first2y), shape = factor(first2y))) +
  geom_point(alpha=0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatterplot of IQ Score vs. Distance from Smelter",
       x = "Distance from Smelter (Miles)",
       y = "IQ Score",
       color = "First 2 Years",
       shape = "First 2 Years") +
  theme_minimal()


#3i
# Fit the simpler model
model_reduce <- lm(iq ~ miles, data = lead)

# Get the ANOVA table for the model
anova_result1 <- anova(model_reduce)

# Get the ANOVA table for the model
anova_result2 <- anova(model)

# Print the ANOVA table
print(anova_result1)

print(anova_result2)


# Given values
F_statistic <- 5.634  
df1 <- 2              
df2 <- 120            

# Calculate the p-value
p_value <- 1 - pf(F_statistic, df1, df2)

# Print the p-value
print(p_value)

a <- anova(model_reduce, model)
summary(a)