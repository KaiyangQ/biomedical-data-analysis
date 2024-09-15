# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW4")

# Load the data
data <- read.csv("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW4/framingham_Homework4.csv")

#data <- na.omit(data)

data$male <- as.factor(replace(data$male, data$male == "Male", 1))

data$male <- factor(data$male, levels = c(0, 1), labels = c("Female", "Male"))

data$TenYearCHD <- as.factor(replace(data$TenYearCHD, data$TenYearCHD == "No", 0))

data$TenYearCHD <- factor(data$TenYearCHD, levels = c(0, 1), labels = c("No", "Yes"))

data$currentSmoker <- factor(data$currentSmoker,levels = c(0, 1), labels = c("No", "Yes"))

summary(data)


#2a
library(ggplot2)

model <- glm(TenYearCHD ~ sysBP, data = data, family = binomial())

summary(model)

hat_values <- hatvalues(model)

plot_data <- data.frame(Observation = 1:nrow(data), HatValues = hat_values)

ggplot(plot_data, aes(x = Observation, y = HatValues)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Hat Matrix Diagonal vs. Observation Number",
       x = "Observation Number",
       y = "Hat Matrix Diagonal")


#2b
dfbetas <- dfbetas(model)

dfbetas_data <- data.frame(Observation = 1:nrow(dfbetas),
                           DFBETAs_Intercept = dfbetas[, 1],
                           DFBETAs_sysBP = dfbetas[, 2])  

library(ggplot2)

ggplot(dfbetas_data, aes(x = Observation, y = DFBETAs_Intercept)) +
  geom_point() +
  theme_minimal() +
  labs(title = "DFBETAs for Intercept vs. Observation Number",
       x = "Observation Number",
       y = "DFBETAs for Intercept")

# Plot for the first predictor (adjust the column name if necessary)
ggplot(dfbetas_data, aes(x = Observation, y = DFBETAs_sysBP)) +  # Ensure this matches your dfbetas_data column name
  geom_point() +
  theme_minimal() +
  labs(title = "DFBETAs for sysBP vs. Observation Number",
       x = "Observation Number",
       y = "DFBETAs for sysBP")


#2c
avg_hat_value <- mean(hat_values)
cutoff_hat_value <- 2 * avg_hat_value

n <- nrow(data)
cutoff_dfbeta <- 2 / sqrt(n)

influential_hat <- which(hat_values > cutoff_hat_value)

influential_dfbeta <- which(abs(dfbetas_data$DFBETAs_sysBP) > cutoff_dfbeta)

all_influential <- union(influential_hat, influential_dfbeta)

influential_observations <- data[all_influential, c("Observation", "sysBP")]
print(influential_observations)

#2d
rows_to_exclude <- which(data$Observation %in% c(1938, 3027))

data_excluded <- data[-rows_to_exclude, ]

#write.csv(data_excluded, "data_excluded.csv", row.names = FALSE)

model_excluded <- glm(TenYearCHD ~ sysBP, data = data_excluded, family = binomial())

summary(model_excluded)

coef_original <- coef(model)["sysBP"]
coef_excluded <- coef(model_excluded)["sysBP"]

print(paste("Original sysBP Coefficient:", coef_original))
print(paste("Excluded sysBP Coefficient:", coef_excluded))

# Change in coefficient
change_in_coef <- coef_excluded - coef_original

print(paste("Change in sysBP Coefficient:", change_in_coef))


#3
# Model A: TenYearCHD ~ sysBP
modelA <- glm(TenYearCHD ~ sysBP, data = data_excluded, family = binomial())
summary(modelA)

# Model B: TenYearCHD ~ sysBP + Age
modelB <- glm(TenYearCHD ~ sysBP + age, data = data_excluded, family = binomial())
summary(modelB)

# Model C: TenYearCHD ~ Age + Sex
# Assuming 'male' is the column representing sex, with 1 for male and 0 for female
modelC <- glm(TenYearCHD ~ age + factor(male), data = data_excluded, family = binomial())
summary(modelC)
exp(confint(modelC))

install.packages("rcompanion")
install.packages("tibble")


library(rcompanion)
library(pROC)
library(DescTools)
library(tibble)

# define function to get predictive measures based on what model is input
get_predictive_measures = function(model_of_interest, model_name){
  r_squared = rcompanion::nagelkerke(model_of_interest)
  
  tibble(
    model = model_name,
    aic = AIC(model_of_interest),
    bic = BIC(model_of_interest),
    gen_Rsq = r_squared$Pseudo.R2[2],
    nagel_Rsq = r_squared$Pseudo.R3[3],
    somersD = (pROC::auc(model_of_interest$y, model_of_interest$fitted.values))/0.5 - 1,
    gamma = DescTools::GoodmanKruskalGamma(model_of_interest$y, model_of_interest$fitted.values),
    tauA = DescTools::KendallTauA(model_of_interest$y, model_of_interest$fitted.values),
    Cindex = pROC::auc(model_of_interest$y, model_of_interest$fitted.values)
  )
}


results_modelA <- get_predictive_measures(modelA, "Model A")

results_modelB <- get_predictive_measures(modelB, "Model B")

results_modelC <- get_predictive_measures(modelC, "Model C")

results_modelA
results_modelB
results_modelC


exp(0.05510)

