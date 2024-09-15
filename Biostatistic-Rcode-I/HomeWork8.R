amniotic <- data.frame(
  cells = c(1.13,1.20,1.00,0.91,1.05,1.75,1.45,1.55,1.64,1.60,
            2.30,2.15,2.25,2.40,2.49,3.18,3.10,3.28,3.35,3.12),
  temp = c(rep(40,5), rep(60,5), rep(80,5), rep(100,5)))
amniotic$ln_cells <- log(amniotic$cells) # calculate log(cells)


#2a
# Perform the linear regression
model <- glm(ln_cells ~ temp, data = amniotic)

# Summary of the model to obtain coefficients
model_summary <- summary(model)
print(model_summary)

# Predicted regression equation for the outcome on the log scale
intercept <- coef(model)["(Intercept)"]
slope <- coef(model)["temp"]
#cat("Predicted regression equation (log scale): ln(cells) =", intercept, "+", slope, "* temp\n")

# Interpretation of the coefficients
#cat("Intercept interpretation: At 0 degrees temperature, the natural log of the cell count is", intercept, "\n")
#cat("Slope interpretation: For each additional degree of temperature, the natural log of the cell count increases by", slope, "\n")

# Transform the estimate of the slope and its 95% confidence interval to the original (not logged) scale
conf_int <- confint(model, level = 0.95)
slope_antilog <- exp(slope)
lower <- exp(conf_int[2,1])
upper <- exp(conf_int[2,2])

# Print
cat("Original scale slope:",slope_antilog, "\n")
cat("Original scale 95% CI:",lower, "to", upper, "\n")

# Interpretation of the transformed coefficients
cat("Transformed slope estimate: For each additional degree of temperature, the cell count is multiplied by", slope_antilog, "\n")
cat("95% confidence interval of the transformed slope:", lower, "to", upper, "\n")





#2b
# Set the graphics layout to a 2x2 grid
par(mfrow=c(2,2), mar=c(4.1, 4.1, 3.1, 2.1))

# Scatterplot of the observed data
plot(amniotic$temp, amniotic$ln_cells, xlab='Temperature', ylab='Log of Cell Count', main='Scatterplot', cex=0.7)
abline(model, col='blue')

# Residual plot
plot(amniotic$temp, rstudent(model), xlab='Temperature', ylab='Jackknife Residual', main='Residual Plot', cex=0.7)
abline(h=0, lty=2, col='gray65')

# Histogram of the residuals
hist(rstudent(model), xlab='Jackknife Residual', main='Histogram of Residuals', freq=F, breaks=seq(-4,10,0.25))
curve(dnorm(x, mean=0, sd=1), lwd=2, col='blue', add=T)

# Normal Probability Plot
plot(ppoints(length(rstudent(model))), sort(rstudent(model)),
     xlab='Observed Cumulative Probability',
     ylab='Expected Cumulative Probability',
     main='Normal Probability Plot', cex=0.7)
abline(a=0, b=1, col='gray65', lwd=1)

# Reset graphics layout
par(mfrow=c(1,1))



getwd()
setwd("/Users/kaiyangqian/Rcode")

# Exercise 3
# Load the data
fram_dat <- read.csv('frmgham2_baseline_subset.csv')

# Store the initial number of cases
initial_cases <- nrow(fram_dat)

# Define the columns to check for NA values
columns_to_check <- c("TOTCHOL", "CURSMOKE", "BMI", "PREVCHD", "PREVMI", "PREVSTRK", "PREVHYP")

# Remove cases with missing values only in the specified columns
fram_dat_clean <- fram_dat[complete.cases(fram_dat[,columns_to_check]),]

# Store the final number of cases after removing NA values
final_cases <- nrow(fram_dat_clean)

# Calculate and print the number of cases removed
cases_removed <- initial_cases - final_cases

# Print
print(cases_removed)


#3c
# Fit the full model
full_model <- lm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, data=fram_dat_clean)

# Fit the null model (model with only the intercept)
null_model <- lm(TOTCHOL ~ 1, data=fram_dat_clean)

# Perform an F-test to compare the two models
f_test_result <- anova(null_model, full_model)
f_test_result


#3d
# Fit the reduced model with BMI and smoking status
reduced_model <- glm(TOTCHOL ~ CURSMOKE + BMI, data=fram_dat_clean, family=gaussian())

# Fit the full model with BMI, smoking status, and the cardiovascular conditions
full_model <- glm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, data=fram_dat_clean, family=gaussian())

# Perform an ANOVA to compare the reduced model with the full model
anova_result <- anova(reduced_model, full_model, test="F")

# Output the results
anova_result



#3e
# Fit the model without the smoking status variable
model_without_smoke <- glm(TOTCHOL ~ BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, 
                           data=fram_dat_clean, family=gaussian())

# Fit the full model, including smoking status
model_with_smoke <- glm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, 
                        data=fram_dat_clean, family=gaussian())

# Perform an ANOVA to compare the model without CURSMOKE against the full model
anova_result <- anova(model_without_smoke, model_with_smoke, test="F")

# Output the results
anova_result



#3f
install.packages("car")

# Load the required library
library(car)

# Fit the full model with all predictors
full_model <- lm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, data=fram_dat_clean)

# Calculate the VIF for the model
vif_values <- vif(full_model)

# Output the VIF values
vif_values




#3g
# Fit the model without the BMI variable
model_without_BMI <- glm(TOTCHOL ~ CURSMOKE + PREVCHD + PREVMI + PREVSTRK + PREVHYP, 
                         data=fram_dat_clean)

# Fit the full model, including BMI
model_with_BMI <- glm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, 
                      data=fram_dat_clean)

# Perform an ANOVA to compare the model without BMI against the full model
anova_result <- anova(model_without_BMI, model_with_BMI, test="F")

# Output the results
anova_result




#3g
# Summary of the regression model
model_summary <- summary(full_model)

# Print the summary to the console
print(model_summary)

# To get the 95% confidence intervals for the coefficients
conf_intervals <- confint(full_model)

# Print the confidence intervals
print(conf_intervals)


