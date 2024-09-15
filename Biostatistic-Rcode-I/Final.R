
# Coefficient for sex
estimate_sex <- 0.03973
standard_error_sex <- 0.44109
critical_value <- 1.96  # for 95% CI

# Calculate the lower and upper bounds of the CI
lower_bound <- estimate_sex - (critical_value * standard_error_sex)
upper_bound <- estimate_sex + (critical_value * standard_error_sex)

# Print the confidence interval
cat("The 95% CI for the coefficient associated with sex is (", lower_bound, ",", upper_bound, ")\n")


#1e
# Given values for the sum of squares and degrees of freedom
sum_squares_full_model = 81842.94
sum_squares_reduced_model = 6824.94
df_full_model = 6
df_reduced_model = 4
mean_square_error = 216.65

# Calculate the partial F-test statistic
f_statistic = ((sum_squares_full_model - sum_squares_reduced_model) / (df_full_model - df_reduced_model)) / mean_square_error

# Print the F-test statistic
cat("The partial F-test statistic is:", f_statistic, "\n")



#2a
# Coefficients for depression (crude and adjusted)
beta_crude <- -0.3779
beta_adj <- -0.02835

# Calculate percent change for biostatisticians
percent_change_biostatisticians <- ((beta_crude - beta_adj) / beta_crude) * 100

# Calculate percent change for epidemiologists
percent_change_epidemiologists <- ((beta_crude - beta_adj) / beta_adj) * 100

# Print the results
cat("Percent change for biostatisticians:", percent_change_biostatisticians, "%\n")
cat("Percent change for epidemiologists:", percent_change_epidemiologists, "%\n")



#4c
p1 <- 1009/1660
p2 <- 1526/3117

a <- p1/p2
a

effect_size <- 1
sd <- 1.5
size <- 30

a5 <- effect_size/(sd/sqrt(size)) - 1.96
a5

dd <- pnorm(a5)
dd