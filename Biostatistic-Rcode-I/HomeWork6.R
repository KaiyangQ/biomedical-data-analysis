# Get and print the current working directory
current_directory <- getwd()
print(current_directory)

# Change the working directory to a specific path
setwd("/Users/kaiyangqian/Rcode")

#1a
library(readr)
library(ggplot2)
library(stats)

# Load the data
data <- read_csv("ProcedureCost.csv")

# Filter data for the new procedure (coded as "2")
subdata <- subset(data, Procedure == 2)

# Plot a histogram
ggplot(subdata, aes(x = Cost)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Cost ($1000)", y = "Frequency") +
  ggtitle("Histogram of New Procedure Costs")

# Plot a normal quantile plot (Q-Q plot)
qqnorm(subdata$Cost)
qqline(subdata$Cost, col = "red")
title("Normal Q-Q Plot")

# Plot a boxplot
boxplot(subdata$Cost, horizontal = TRUE, col = "green",
        xlab = "Cost ($1000)", main = "Boxplot of New Procedure Costs")



#1c
# Calculate the mean
mean_cost <- mean(subdata$Cost)

# Calculate the standard deviation
sd_cost <- sd(subdata$Cost)

# Display the mean and standard deviation
cat("Mean Cost: ", mean_cost, "\n")
cat("Standard Deviation Cost: ", sd_cost, "\n")


#1d
# Number of iterations
n_iterations <- 10000

# Create an empty vector to store bootstrap sample means
bootstrap_means <- numeric(n_iterations)

# Set seed
set.seed(1011)

# Perform the bootstrap
for (i in 1:n_iterations) {
  # Sample with replacement from the "Cost" variable
  bootstrap_sample <- sample(subdata$Cost, length(subdata$Cost), replace = TRUE)
  bootstrap_means[i] <- mean(bootstrap_sample)
}

# Create a histogram of bootstrap sample means
hist(bootstrap_means, breaks = 30, main = "Bootstrap Sampling Distribution",
     xlab = "Bootstrap Sample Means", ylab = "Frequency", col = "blue", border = "black")

# Create a QQ-plot of bootstrap sample means
qqnorm(bootstrap_means, main = "QQ-Plot of Bootstrap Sample Means")
qqline(bootstrap_means, col = "red")


#1f
# Calculate the bootstrap mean
bootstrap_mean <- mean(bootstrap_means)

# Calculate the standard error of the mean
bootstrap_se <- sd(bootstrap_means)

# Calculate the bias
sample_mean <- mean(subdata$Cost)  # Original sample mean
bias <- bootstrap_mean - sample_mean

# Print the results
cat("Bootstrap Mean:", bootstrap_mean, "\n")
cat("Standard Error of the Mean:", bootstrap_se, "\n")
cat("Bias:", bias, "\n")


#1g
# Calculate the 95% normal percentile confidence interval
confidence_level <- 0.95
z <- qnorm(1 - (1 - confidence_level) / 2)
se <- sd(bootstrap_means) 
lower_limit <- mean(bootstrap_means) - z * se
upper_limit <- mean(bootstrap_means) + z * se

# Coverage of CI at lower end
lower_end <- sum(bootstrap_means < lower_limit)/n_iterations

# Coverage of CI at upper end
upper_end <- sum(bootstrap_means > upper_limit)/n_iterations

# Print the results
cat("Lower limit of 95% normal percentile CI:", lower_limit, "\n")
cat("Upper limit of 95% normal percentile CI:", upper_limit, "\n")
cat("Coverage of CI at lower end:", lower_end, "\n")
cat("Coverage of CI at upper end:", upper_end, "\n")

# Calculate the 95% bootstrap percentile confidence interval
bootstrap_CI <- quantile(bootstrap_means, c(0.025,0.975))

# Accuracy of the bootstrap CI bias/SE
bootstrap_CI_Accuracy <- bias/se

# Print the results
cat("95% bootstrap percentile CI:", bootstrap_CI, "\n")
cat("Accuracy of the bootstrap CI:", bootstrap_CI_Accuracy, "\n")



#2a
library(readr)
library(boot)
# Change the working directory to a specific path
setwd("/Users/kaiyangqian/Rcode")

# Load the data
data <- read_csv("ProcedureCost.csv")

# Number of bootstrap samples
nboot <- 10000

# Create a function to calculate the ratio of means for each bootstrap sample
bootstrap_mean_ratio <- function(data, indices) {
  resampled_data <- data[indices, ]
  new_mean <- mean(resampled_data$Cost[resampled_data$Procedure == 2])
  standard_mean <- mean(resampled_data$Cost[resampled_data$Procedure == 1])
  return(new_mean / standard_mean)
}

# Perform the bootstrap resampling
set.seed(1013)  # Set a seed for reproducibility
boot_samples <- boot(data, statistic = bootstrap_mean_ratio, R = nboot)

# Plot a histogram of the bootstrap sampling distribution
hist(boot_samples$t, main = "Bootstrap Sampling Distribution",
     xlab = "Ratio of Mean Costs (New/Standard)")

# Create a QQ-plot of the bootstrap sampling distribution
qqnorm(boot_samples$t)
qqline(boot_samples$t)


#2c
# Calculate the bootstrap mean
bootstrap_mean <- mean(boot_samples$t)

# Calculate the standard error of the mean
bootstrap_se <- sd(boot_samples$t)

# Calculate the ratio of mean costs for the original data
original_ratio <- mean(data$Cost[data$Procedure == 2]) / mean(data$Cost[data$Procedure == 1])

# Calculate the bias
bias <- bootstrap_mean - original_ratio

# Print the results
cat("Bootstrap Mean: ", bootstrap_mean, "\n")
cat("Bootstrap Standard Error: ", bootstrap_se, "\n")
cat("Bias: ", bias, "\n")



#2d
# Calculate the 95% normal percentile confidence interval
confidence_level <- 0.95
z <- qnorm(1 - (1 - confidence_level) / 2)
lower_limit <- bootstrap_mean - z * bootstrap_se
upper_limit <- bootstrap_mean + z * bootstrap_se

# Coverage of CI at lower end
lower_end <- sum(boot_samples$t < lower_limit) / nboot

# Coverage of CI at upper end
upper_end <- sum(boot_samples$t > upper_limit) / nboot

# Calculate the 95% bootstrap percentile confidence interval
bootstrap_CI <- quantile(boot_samples$t, c(0.025, 0.975))

# Accuracy of the bootstrap CI bias/SE
bootstrap_CI_Accuracy <- bias / bootstrap_se

# Print the results
cat("95% normal percentile CI:", lower_limit, upper_limit, "\n")
cat("Coverage of CI at lower end:", lower_end, "\n")
cat("Coverage of CI at upper end:", upper_end, "\n")
cat("95% bootstrap percentile CI:", bootstrap_CI, "\n")
cat("Accuracy of the bootstrap CI:", bootstrap_CI_Accuracy, "\n")



#3a
# Change the working directory to a specific path
setwd("/Users/kaiyangqian/Rcode")

# Read the CSV data into a dataframe
ProcedureCost <- read.csv("ProcedureCost.csv")

# Calculate the observed ratio of mean costs
observed_new_mean <- mean(ProcedureCost$Cost[ProcedureCost$Procedure == 2])
observed_standard_mean <- mean(ProcedureCost$Cost[ProcedureCost$Procedure == 1])
observed_ratio <- observed_new_mean / observed_standard_mean

# Number of permutations
n_permutations <- 10000

# Create an empty vector to store permuted ratios
permuted_ratios <- numeric(n_permutations)

# Perform the permutation test
for (i in 1:n_permutations) {
  # Permute the "Procedure" labels
  permuted_procedure <- sample(ProcedureCost$Procedure, replace = FALSE)
  
  # Calculate the ratio of mean costs for this permutation
  new_mean <- mean(ProcedureCost$Cost[permuted_procedure == 2])
  standard_mean <- mean(ProcedureCost$Cost[permuted_procedure == 1])
  ratio <- new_mean / standard_mean
  
  permuted_ratios[i] <- ratio
}

# Create a histogram of the permutation distribution
hist(permuted_ratios, breaks = 30, main = "Permutation Distribution of Ratio",
     xlab = "Ratio of Mean Costs (New/Standard)", ylab = "Frequency", col = "blue", border = "black")

# Add a vertical line at the observed ratio
abline(v = observed_ratio, col = "red", lwd = 2)



#3b
# Calculate the two-sided p-value
p_value <- 2 * max(sum(permuted_ratios <= observed_ratio) + 1, sum(permuted_ratios >= observed_ratio) + 1) / (n_permutations + 1)

# Print the p-value
cat("The p-value is", p_value)


#4a
# Define the LOS data for both hospitals
cauchy_general <- c(3, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 15)
skellam_memorial <- c(6, 7, 7, 7, 8, 8, 8, 9, 9, 10, 10, 11, 13, 13, 15)

# Perform the Wilcoxon rank-sum test with continuity correction and specify the historic median (mu = 9)
result <- wilcox.test(cauchy_general, skellam_memorial, alternative = "two.sided", mu = 9, exact = FALSE)

# Print the test result
print(result)



#4b
# Set up the 2x1 panel layout
par(mfrow = c(2, 1))

# Create a histogram for Cauchy General
hist(
  cauchy_general,
  breaks = seq(0, 16, by = 2),
  main = "Histogram of LOS for Cauchy General",
  xlab = "Length of Stay (days)",
  ylab = "Frequency",
  col = "lightyellow"
)

# Create a histogram for Skellam Memorial
hist(
  skellam_memorial,
  breaks = seq(0, 16, by = 2),
  main = "Histogram of LOS for Skellam Memorial",
  xlab = "Length of Stay (days)",
  ylab = "Frequency",
  col = "lightblue"
)

#4c
# Perform the Wilcoxon rank-sum test
wilcox_result <- wilcox.test(cauchy_general, skellam_memorial, alternative = "two.sided", exact = FALSE)

# Print the test result
print(wilcox_result)