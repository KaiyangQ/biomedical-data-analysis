library(ggplot2)

# Create a dataframe with Sensitivity and 1 - Specificity
data <- data.frame(Sensitivity = c(0.125, 0.1875, 0.4375, 0.750, 0.9375, 1.000),
                   Specificity = c(1.000, 1.000, 0.9348, 0.7391, 0.3913, 0.000))

# Create the ROC curve plot
roc_plot <- ggplot(data, aes(1 - Specificity, Sensitivity)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       title = "ROC Curve") +
  theme_minimal() +
  xlim(0, 1) + ylim(0, 1)
# Display the ROC curve
print(roc_plot)

sum(diff(1 - data$Specificity) * (data$Sensitivity[-1] + data$Sensitivity[-length(data$Sensitivity)]) / 2)
diff(1 - data$Specificity)




library(epiR)

# Create a 2x2 table
tab <- matrix(c(65, 15, 72, 48), nrow = 2, byrow = TRUE)
colnames(tab) <- c("Nausea_Yes", "Nausea_No")
rownames(tab) <- c("Anesthetic_A", "Anesthetic_B")

# Calculate risk difference, risk ratio, and odds ratio
result <- epi.2by2(tab)

# Print the results
result





# Chi-squared test without continuity correction
chisq_result <- chisq.test(tab, correct = FALSE)

# Print the result
chisq_result


# Chi-squared test with continuity correction
chisq_corrected_result <- chisq.test(tab, correct = TRUE)

# Print the result
chisq_corrected_result


# Fisher's exact test
fisher_test_result <- fisher.test(tab)

# Print the result
fisher_test_result



# McNemar's test with continuity correction
mcnemar_result <- mcnemar.test(tab, correct = TRUE)

# Print the result
mcnemar_result

