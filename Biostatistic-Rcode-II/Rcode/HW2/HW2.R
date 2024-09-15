# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW2")


library(readr) 

titanic_df <- read_csv("Titanic.csv")

titanic_df$Sex <- ifelse(titanic_df$Sex == "female", 0, 1)
titanic_df$Pclass <- as.factor(titanic_df$Pclass)

model <- glm(Survived ~ Sex + Age + Pclass, data = titanic_df, family = binomial)

summary(model)

confint_model <- confint(model)
print(confint_model)


new_passenger <- data.frame(Sex = 0,  
                            Age = 17,
                            Pclass = factor(3, levels = c(1, 2, 3)))

predicted_probability <- predict(model, newdata = new_passenger, type = "response")

print(predicted_probability)

new_passenger <- data.frame(Sex = 1,  
                            Age = 51,
                            Pclass = factor(2, levels = c(1, 2, 3)))

predicted_probability <- predict(model, newdata = new_passenger, type = "response")

print(predicted_probability)




coef_age <- coef(model)["Age"]
se_age <- summary(model)$coefficients["Age", "Std. Error"]
or_age_10yr <- exp(coef_age * 10)

z_value <- 1.96 
ci_lower <- exp((coef_age - z_value * se_age) * 10)
ci_upper <- exp((coef_age + z_value * se_age) * 10)

cat("Odds Ratio for a 10-year increase in age:", or_age_10yr, "\n")
cat("95% CI for OR:", ci_lower, "-", ci_upper, "\n")


