getwd()
setwd("/Users/kaiyangqian/Rcode")


# Set the seed for reproducibility
set.seed(1207)

# Load necessary libraries
library(boot)

# Read in the data from the CSV file
pd_data <- read.csv('PD Exercise RCT Selected Secondary Outcomes - Wide.csv')

# Define the bootstrap statistic function
bootstrap_mean <- function(data, indices) {
  # Sample with the given indices
  d <- data[indices,] 
  # Calculate mean
  mean(d$UPDRS16, na.rm = TRUE)
}

# Perform the bootstrap analysis for each group
bootstrap_results <- lapply(split(pd_data, pd_data$Group), function(group_data) {
  boot(data = group_data, statistic = bootstrap_mean, R = 1000)
})

# Calculate the mean and confidence intervals for each group
bootstrap_summary <- sapply(bootstrap_results, function(b) {
  c(Mean = mean(b$t),
    Variance = var(b$t),
    `Lower CI (2.5%)` = quantile(b$t, 0.025),
    `Upper CI (97.5%)` = quantile(b$t, 0.975))
})

# Print the results
print(bootstrap_summary)


library(ggplot2)

# Combine all bootstrap results into one data frame for plotting
bootstrap_data <- do.call(rbind, lapply(names(bootstrap_results), function(group) {
  data.frame(Group = group, Mean = bootstrap_results[[group]]$t)
}))

# Convert the 'Group' variable to a factor if it's not already
bootstrap_data$Group <- factor(bootstrap_data$Group, levels = names(bootstrap_results))

# Create the plot
ggplot(bootstrap_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_boxplot() +
  labs(title = "Bootstrap Distribution of UPDRS Scores at 16 Months",
       x = "Exercise Group",
       y = "Bootstrap Mean UPDRS Score",
       fill = "Group") +
  theme_minimal()








library(ggplot2)

# Create a summary data frame with the mean and standard error (square root of variance)
summary_data <- do.call(rbind, lapply(names(bootstrap_results), function(group) {
  data.frame(
    Group = group,
    Mean = mean(bootstrap_results[[group]]$t),
    SE = sqrt(var(bootstrap_results[[group]]$t))
  )
}))

# Convert the 'Group' variable to a factor
summary_data$Group <- factor(summary_data$Group, levels = names(bootstrap_results))

# Create the plot with error bars
ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Bootstrap Mean UPDRS Scores at 16 Months with Standard Error",
       x = "Exercise Group",
       y = "Bootstrap Mean UPDRS Score",
       fill = "Group") +
  theme_minimal()




