# Exercise 1: Correcting for Multiple Comparisons
# Given p-values
p_values <- c(0.040, 0.100, 0.400, 0.550, 0.340, 0.620, 0.001, 0.010, 0.800, 0.005)

# Apply FDR and Bonferroni corrections and round the results
adjusted_p_values <- round(cbind('fdr' = p.adjust(p_values, method = 'fdr'),
                                 'bon' = p.adjust(p_values, method = 'bonferroni')), 4)

# Print adjusted_p_values
adjusted_p_values

# Convert the matrix to a dataframe
adjusted_p_values_df <- as.data.frame(adjusted_p_values)

# Determine the number of significant p-values after FDR adjustment
significant_fdr <- sum(adjusted_p_values_df$fdr < 0.05)

# Determine the number of significant p-values after Bonferroni adjustment
significant_bon <- sum(adjusted_p_values_df$bon < 0.05)

# Output the counts
print(paste("Significant SNPs after FDR correction:", significant_fdr))
print(paste("Significant SNPs after Bonferroni correction:", significant_bon))



#2a
# Create data set from table
lung <- data.frame( group=c( rep('A',5), rep('B',12), rep('C',5) ),
                    react=c(20.8,4.1,30,24.7,13.8,
                            7.5,7.5,11.9,4.5,3.1,8,4.7,28.1,10.3,10,5.1,2.2,
                            9.2,2,2.5,6.1,7.5) )

# Perform ANOVA
anova_result <- aov(react ~ group, data=lung)

# Display the summary of the ANOVA
summary(anova_result)


#2b
# Conduct the Tukey HSD test
tukey_result <- TukeyHSD(anova_result)

# Print the results of the Tukey HSD test
print(tukey_result)


#2c
# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(react ~ group, data=lung)

# Print the results of the Kruskal-Wallis test
print(kruskal_result)




# Write the carotenoids dataframe to a CSV file
write.csv(carotenoids, "carotenoids.csv", row.names = FALSE)

getwd()
#setwd("/Users/kaiyangqian/Rcode")

#Exercise 3
#3a
carotenoids <- read.table('carotenoids.dat')
colnames(carotenoids) <- c('age','sex','smoke','bmi','vitamins','calories','fat',
                           'fiber','alcohol','chol','betadiet','retdiet','betaplas','retplas')

# Subset the data by smoking status
never_smokers <- subset(carotenoids, smoke == 1)
former_smokers <- subset(carotenoids, smoke == 2)
current_smokers <- subset(carotenoids, smoke == 3)

# Define a function to calculate the required statistics
calculate_stats <- function(subset_data) {
  n <- length(subset_data$betaplas) # sample size
  mean_value <- mean(subset_data$betaplas) # mean
  sd_value <- sd(subset_data$betaplas) # standard deviation
  se_value <- sd_value / sqrt(n) # standard error
  
  return(c(n, mean_value, sd_value, se_value))
}

# Apply the function to each subset
stats_never <- calculate_stats(never_smokers)
stats_former <- calculate_stats(former_smokers)
stats_current <- calculate_stats(current_smokers)

# Display the results
stats_never
stats_former
stats_current



#3b
# Convert 'smoke' to categorical 
carotenoids$smoke <- as.factor(carotenoids$smoke)

# Make the Never smoke reference group
carotenoids$smoke <- relevel(carotenoids$smoke, ref = "1")

# Fit the linear regression model
model <- lm(betaplas ~ calories + smoke, data = carotenoids)

# Display the summary of the model to get the coefficients
summary(model)



#3e
# Fit the linear regression model without the 'calories' variable
reduced_model <- lm(betaplas ~ smoke, data = carotenoids)

# Display the summary of the reduced model 
summary(reduced_model)



