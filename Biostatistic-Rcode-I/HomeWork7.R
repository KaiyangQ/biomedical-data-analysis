hw7 <- data.frame(
  id = 1:7,
  gender = c(0,1,0,1,0,1,0),
  chol = c(254,402,288,354,220,451,405),
  wtkg = c(57,79,63,84,30,76,65),
  age =c(23,57,28,46,34,57,52)
)

# Performing the linear regression
model <- lm(chol ~ wtkg, data = hw7)

# Displaying the summary of the regression model
summary(model)

# Getting the confidence intervals for the coefficients
confint(model)






#2j
# Load necessary library
library(ggplot2)

# Using the data frame and model from previous code
hw7 <- data.frame(id = 1:7, gender = c(0,1,0,1,0,1,0), 
                  chol = c(254,402,288,354,220,451,405), 
                  wtkg = c(57,79,63,84,30,76,65), age = c(23,57,28,46,34,57,52))

model <- lm(chol ~ wtkg, data = hw7)

# Getting the predictions with confidence and prediction intervals
preds_conf <- predict(model, interval = "confidence")
preds_pred <- predict(model, interval = "prediction")

# Adding the predictions and intervals to the data frame
hw7$fit <- preds_conf[,1]
hw7$lwr_conf <- preds_conf[,2]
hw7$upr_conf <- preds_conf[,3]
hw7$lwr_pred <- preds_pred[,2]
hw7$upr_pred <- preds_pred[,3]

# Creating the scatterplot with regression line, confidence intervals, and prediction intervals as lines
p <- ggplot(hw7, aes(x = wtkg, y = chol)) +
  geom_point() +  # Add points
  geom_line(aes(y = fit, color = "Regression Fit")) +  # Add regression line
  geom_line(aes(y = lwr_conf, color = "95% CI"), linetype = "dashed") +  # Add lower bound of confidence interval
  geom_line(aes(y = upr_conf, color = "95% CI"), linetype = "dashed") +  # Add upper bound of confidence interval
  geom_line(aes(y = lwr_pred, color = "95% PI"), linetype = "dashed") +  # Add lower bound of prediction interval
  geom_line(aes(y = upr_pred, color = "95% PI"), linetype = "dashed") +  # Add upper bound of prediction interval
  scale_color_manual(values = c("blue", "red", "green")) +
  labs(x = "Weight (kg)", y = "Cholesterol (mg/100ml)", 
       title = "Scatterplot with Regression Line, 95% CI, and Prediction Interval",
       color = "Legend") +
  theme_minimal()

# Display the plot
print(p)
