# Generate data
x <- seq(0, 100, by=0.5)

# Set seed for reproducibility
set.seed(1024)

# Uniform distribution
uniform_data <- dunif(x, min=0, max=100)

# Exponential distribution
lambda <- 0.03  # rate parameter
exponential_data <- dexp(x, rate=lambda)

# Binomial distribution
size <- 100  # number of trials
prob <- 0.5  # probability of success
binomial_data <- dbinom(0:size, size=size, prob=prob)

# Base R Plot
plot(x, uniform_data, type="l", col="green", ylim=c(0, max(c(uniform_data, binomial_data, exponential_data))), ylab="Density", xlab="X", main="Uniform, Exponential, and Binomial Distributions")
lines(x, exponential_data, col="blue")
lines(0:size, binomial_data, col="red")

# Legend
legend("topright", legend=c("Uniform", "Exponential", "Binomial"), fill=c("green", "blue", "red"))





# Set up the simulation parameters
num_repeats <- 1000
sample_sizes <- c(2, 5, 30, 100)

# Create an empty list to store the results
results <- list()

# Set seed for reproducibility
set.seed(1024)

# Simulate for Uniform Distribution
results$Uniform <- lapply(sample_sizes, function(n) {
  replicate(num_repeats, mean(runif(n)))
})

# Simulate for Exponential Distribution (lambda = 1)
results$Exponential <- lapply(sample_sizes, function(n) {
  replicate(num_repeats, mean(rexp(n, rate = 1)))
})

# Simulate for Binomial Distribution (size=1, prob=0.5)
results$Binomial <- lapply(sample_sizes, function(n) {
  replicate(num_repeats, mean(rbinom(n, size = 1, prob = 0.5)))
})

# Plot the results
par(mfrow=c(3, 4))
for (distribution in names(results)) {
  for (i in seq_along(sample_sizes)) {
    if(distribution == "Uniform"){
      hist(results[[distribution]][[i]], main = paste(distribution, "n =", sample_sizes[i]), 
           xlim = c(0, 1),  probability = TRUE,
           xlab = paste("Sample Mean"))
    }else if (distribution == "Exponential"){
      hist(results[[distribution]][[i]], main = paste(distribution, "n =", sample_sizes[i]), 
           xlim = c(0, 3),  probability = TRUE, breaks = 20,
           xlab = paste("Sample Mean"))
    }else if (distribution == "Binomial"){
      hist(results[[distribution]][[i]], main = paste(distribution, "n =", sample_sizes[i]), 
           xlim = c(0, 1),  probability = TRUE, breaks = 20,
           xlab = paste("Sample Mean"))
    }
    
  }
}









# Plot the results
par(mfrow=c(3, 4))
for (distribution in names(results)) {
  for (i in seq_along(sample_sizes)) {
    hist(results[[distribution]][[i]], main = paste(distribution, "n =", sample_sizes[i]), 
         xlim = c(0, 3),  probability = TRUE,
         xlab = paste("Sample Mean"))
  }
}


