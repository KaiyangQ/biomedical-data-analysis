# Set the random seed for reproducibility
set.seed(917)

# Simulate Exponential(3) distribution with size 100,000
simulated_data <- rexp(100000, rate = 3)

# Calculate the mean and variance of the simulated data
mean_simulated <- mean(simulated_data)
variance_simulated <- var(simulated_data)

cat("Mean of simulated data:", mean_simulated, "\n")
cat("Variance of simulated data:", variance_simulated, "\n")




# Set the random seed for reproducibility
set.seed(917)

# Simulate a sample of 10,000 from a Normal distribution with mean μ=125 and standard deviation σ=8
normal_sample <- rnorm(10000, mean = 125, sd = 8)

# Simulate a sample of 10,000 from an Exponential distribution with rate λ=1.5
exponential_sample <- rexp(10000, rate = 1.5)

mean_normal_sample <- mean(normal_sample)
sd_normal_sample <- sd(normal_sample)

mean_exponential_sample <- mean(exponential_sample)
sd_exponential_sample <- sd(exponential_sample)

# print
cat("Sample Mean:", mean_normal_sample, "Sample Standard Deviation:", sd_normal_sample, "\n\n")
cat("Sample Mean:", mean_exponential_sample, "Sample Standard Deviation:", sd_exponential_sample, "\n")





# histogram and boxplot for the Normal sample
hist(normal_sample, main="Histogram of Normal Distribution", xlab="Value", col="lightblue")
boxplot(normal_sample, main="Boxplot of Normal Distribution", col="lightgreen")

# histogram and boxplot for the Exponential sample
hist(exponential_sample, main="Histogram of Exponential Distribution", xlab="Value", col="lightblue")
boxplot(exponential_sample, main="Boxplot of Exponential Distribution", col="lightgreen")




# Set seed for reproducibility
set.seed(917)

# Parameters
population_mean <- 40
population_sd <- 10
sample_size <- 10
num_iterations <- 1000

# Create empty vectors to store the results
means <- numeric(num_iterations)
medians <- numeric(num_iterations)
variances <- numeric(num_iterations)

# Perform 1000 simulation iterations
for (i in 1:num_iterations) {
  sample_data <- rnorm(sample_size, mean = population_mean, sd = population_sd)
  sample_mean <- mean(sample_data)
  sample_median <- median(sample_data)
  sample_variance <- var(sample_data)
  
  # Store the results
  means[i] <- sample_mean
  medians[i] <- sample_median
  variances[i] <- sample_variance
}

#Create histograms for mean , median ,variance
hist(means, main="Sampling Distribution of the Mean", xlab="Sample Mean", col="orange")
hist(medians, main="Sampling Distribution of the Median", xlab="Sample Median", col="orange")
hist(variances, main="Sampling Distribution of Variance", xlab="Sample Variance", col="orange")



#Save
getwd()
setwd("/Users/kaiyangqian/Rcode")

#Parameters
n_sample <- 500
sample_size <- 10
success_prob <- 0.15

set.seed(918)
sample_means <- replicate(n_sample , mean(rbinom(sample_size, 1, success_prob)))
write.csv(sample_means,file = "sample_means_10.csv")






# Parameters
n_samples <- 500          
sample_sizes <- c(20, 30, 40, 50)  
success_prob <- 0.15     

set.seed(918)
# Create an empty list
sample_means_list <- list()

for (n in sample_sizes) {
  
  sample_means <- replicate(n_samples, mean(rbinom(n, 1, success_prob)))
  sample_means_list[[as.character(n)]] <- sample_means
  
  # Save
  file_name <- paste("sample_means_", n, ".csv", sep = "")
  write.csv(sample_means, file = file_name)
}




# Create a vector to store the sample sizes
sample_sizes <- c(10, 20, 30, 40, 50)

# Initialize empty vectors to store means and standard deviations
means <- numeric(length(sample_sizes))
std_devs <- numeric(length(sample_sizes))

# Loop through the sample sizes and calculate mean and standard deviation
for (i in 1:length(sample_sizes)) {
  # Read the CSV file for the current sample size
  file_name <- paste("sample_means_", sample_sizes[i], ".csv", sep = "")
  sample_means <- read.csv(file_name)$x
  
  # Calculate the mean and standard deviation
  means[i] <- mean(sample_means)
  std_devs[i] <- sd(sample_means)
}

# Print the means and standard deviations
for (i in 1:length(sample_sizes)) {
  cat("Sample Size:", sample_sizes[i], "\n")
  cat("Mean:", means[i], "\n")
  cat("Standard Deviation:", std_devs[i], "\n\n")
}




# Load the necessary libraries
library(ggplot2)

# Create a function to generate and plot histograms
plot_sampling_distribution <- function(sample_means, sample_size) {
  # Create a data frame with sample means
  df <- data.frame(Sample_Means = sample_means)
  
  # Create a histogram
  p <- ggplot(df, aes(x = Sample_Means)) +
    geom_histogram(binwidth = 0.01, fill = "green", color = "black") +
    labs(
      title = paste("Sampling Distribution of Mean (n =", sample_size, ")"),
      x = "Sample Mean",
      y = "Frequency"
    )
  
  # Display the histogram
  print(p)
}

# Loop through the sample sizes and plot histograms
for (n in sample_sizes) {
  file_name <- paste("sample_means_", n, ".csv", sep = "")
  sample_means <- read.csv(file_name)$x  # Read the sample means from the CSV file
  plot_sampling_distribution(sample_means, n)  # Plot the histogram
}

