

a <- qnorm(0.975)
a

b <- 100 / (75 / sqrt(5))
b

c <- b - a
c

result <- pnorm(c)
result


n <- 5
delta <- 100
sigma_change <- 75
alpha <- 0.05

power.t.test(n = 5, delta = 100, sd = 75, sig.level = 0.05)

install.packages("pwr")
library(pwr)
desired_power <- 0.90


z_0.9 <- qnorm(0.9)
z_0.9

z_0.975 <- qnorm(0.975)
z_0.975

n <- 75*75*(z_0.9+z_0.975)*(z_0.9+z_0.975)/10000
n


power.t.test(delta = 100, sd = 75, sig.level = 0.05, power = 0.9)



# 2c

# Given values
sigma_change <- 75
alpha <- 0.05  # Significance level (two-sided)
desired_power <- 0.90

# Calculate critical values from the standard normal distribution
Z_beta <- qnorm(desired_power)
Z_alpha_over_2 <- qnorm(1 - alpha / 2)

# Calculate the smallest detectable mean change (delta) with known SD
smallest_delta_known <- (Z_beta + Z_alpha_over_2) * (sigma_change / sqrt(n))

# Display the result
Z_beta
Z_alpha_over_2
smallest_delta_known


power.t.test(n = 5, sd = 75, sig.level = 0.05, power = 0.9)


#2d
# Given values
sigma_change <- 75
alpha <- 0.05  # Significance level (two-sided)
desired_power <- 0.80

# Calculate critical values from the standard normal distribution
Z_beta <- qnorm(desired_power)
Z_alpha_over_2 <- qnorm(1 - alpha / 2)

# Calculate the smallest detectable mean change (delta) with known SD
smallest_delta_known <- (Z_beta + Z_alpha_over_2) * (sigma_change / sqrt(n))

# Display the result
Z_beta
Z_alpha_over_2
smallest_delta_known


power.t.test(n = 5, sd = 75, sig.level = 0.05, power = 0.8)




#3a
# Set seed for reproducibility
set.seed(2345)

# Parameters
sd <- 75
n <- 5
alpha <- 0.05
numTrials <- 10000
mean <- 0

# Initialize a variable to count rejections
count <- 0

# Perform simulation
for (i in 1:numTrials) {
  # Simulate data under the null hypothesis (mean = 0)
  data <- rnorm(n, mean = mean, sd = sd)
  
  # Perform one-sample two-sided t-test
  t_test_result <- t.test(data, mu = mean, alternative = "two.sided")
  
  # Check if null hypothesis is rejected (p-value < alpha)
  if (t_test_result$p.value < alpha) {
    count <- count + 1
  }
}

# Calculate the proportion of rejections
power <- count / numTrials

# Display the proportion of rejections
cat("Proportion of rejections:", power, "\n")



#3b
# Set seed for reproducibility
set.seed(1796)

# Parameters
sd <- 75
n <- 5
alpha <- 0.05
numTrials <- 10000
alternative_mean <- 100

# Initialize a variable to count rejections
count <- 0

# Perform simulation
for (i in 1:numTrials) {
  # Simulate data under the alternative hypothesis (mean = alternative_mean)
  data <- rnorm(n, mean = alternative_mean, sd = sd)
  
  # Perform one-sample two-sided t-test
  t_test_result <- t.test(data, mu = 0, alternative = "two.sided")
  
  # Check if null hypothesis is rejected (p-value < alpha)
  if (t_test_result$p.value < alpha) {
    count <- count + 1
  }
}

# Calculate the proportion of rejections
power <- count / numTrials

# Display the proportion of rejections
cat("Proportion of rejections:", power, "\n")

