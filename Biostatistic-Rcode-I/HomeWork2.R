# Set a random seed for reproducibility
set.seed(910)

# Simulate a sample of 10,000 from Poisson(λ=1.5)
poisson_sample <- rpois(10000, lambda = 1.5)

# Simulate a sample of 10,000 from Binomial(n=5, p=0.15)
binomial_sample <- rbinom(10000, size = 5, prob = 0.15)

# Print the first few values from each sample to verify
cat("Poisson Sample:\n")
head(poisson_sample)

cat("\nBinomial Sample:\n")
head(binomial_sample)

# Calculate sample mean and standard deviation for Poisson sample
poisson_mean <- mean(poisson_sample)
poisson_std <- sd(poisson_sample)

# Calculate sample mean and standard deviation for Binomial sample
binomial_mean <- mean(binomial_sample)
binomial_std <- sd(binomial_sample)

# Print the results
cat("Poisson Sample Mean:", poisson_mean, "\n")
cat("Poisson Sample Standard Deviation:", poisson_std, "\n")
cat("Binomial Sample Mean:", binomial_mean, "\n")
cat("Binomial Sample Standard Deviation:", binomial_std, "\n")

# Create a histogram for the Poisson sample
hist(poisson_sample, main="Poisson(λ=1.5) Sample Histogram", xlab="Value", ylab="Frequency", col="lightblue")

# Create a histogram for the Binomial sample
hist(binomial_sample, main="Binomial(n=5, p=0.15) Sample Histogram", xlab="Value", ylab="Frequency", col="lightgreen")



# Create a boxplot for the Poisson sample
boxplot(poisson_sample, main="Poisson(λ=1.5) Sample Boxplot", ylab="Value", col="lightblue")

# Create a boxplot for the Binomial sample
boxplot(binomial_sample, main="Binomial(n=5, p=0.15) Sample Boxplot", ylab="Value", col="lightgreen")




# Parameters
sample_size <- 120
population_prevalence <- 0.01  
observed_cases <- 3

# Exact Binomial Probability
binominal_prob <- dbinom(observed_cases, size = sample_size, prob = population_prevalence)

# Poisson Approximation
lambda <- sample_size * population_prevalence
poisson_prob <- dpois(observed_cases, lambda)

# Print results
cat("Binomial Probability:", binominal_prob, "\n")
cat("Poisson Approximation:", poisson_prob, "\n")




install.packages("ggplot2")

# Create vectors for sample sizes and population prevalence
sample_sizes <- seq(80, 400, by = 40)
prevalence_values <- seq(0.0025, 0.025, by = 0.0025)  # As fractions

# Create a data frame with all combinations of sample size and prevalence
combinations <- expand.grid(n = sample_sizes, p = prevalence_values)

# Initialize a vector to store the differences
differences <- numeric(length(combinations$n))

# Loop through each combination
for (i in 1:nrow(combinations)) {
  # Parameters for the current combination
  sample_size <- combinations$n[i]
  population_prevalence <- combinations$p[i]
  observed_cases <- sample_size * 0.025  # Fixed prevalence in your sample
  
  # Calculate exact binomial probability
  exact_prob <- dbinom(observed_cases, size = sample_size, prob = population_prevalence)
  
  # Calculate Poisson approximation
  lambda <- sample_size * population_prevalence
  poisson_prob <- dpois(observed_cases, lambda)
  
  # Calculate the difference and store it in the vector
  differences[i] <- exact_prob - poisson_prob
}

# Add the differences to the combinations data frame
combinations$Difference <- differences

# Create a plot
library(ggplot2)

ggplot(combinations, aes(x = n, y = p, fill = Difference)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Difference Between Exact Binomial and Poisson Approximation",
    x = "Sample Size",
    y = "Population Prevalence",
    fill = "Difference"
  ) +
  theme_minimal()














