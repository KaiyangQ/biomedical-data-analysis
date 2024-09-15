#write.csv(breastcancer2, "D:\GoogleDownloads\breastcancer2.csv", row.names = FALSE)
getwd()
setwd("D:/Homework")

load("breastcancer2.rda")


#1a
# Calculate percentages of event and censoring
total_events <- sum(c$pfsstat == 1)
total_censored <- sum(breastcancer$pfsstat == 0)
percent_events <- (total_events / nrow(breastcancer)) * 100
percent_censored <- (total_censored / nrow(breastcancer)) * 100

# By treatment arm
table_events <- table(breastcancer$arm, breastcancer$pfsstat)
percent_by_arm <- prop.table(table_events, 1) * 100
percent_events
percent_censored
percent_by_arm


#1b
install.packages("ggplot2")

library(ggplot2)

ggplot(breastcancer, aes(x=pfsmos, fill=factor(arm))) +
  geom_histogram(bins=30, position="dodge") +
  labs(fill="Treatment Arm", x="Progression-Free Survival (months)", y="Count") +
  ggtitle("Distribution of Progression-Free Survival Times")


#2a
install.packages("survival")
library(survival)

surv_obj <- Surv(time = breastcancer$pfsmos, event = breastcancer$pfsstat)

# Fit Kaplan-Meier survival curves
fit <- survfit(surv_obj ~ factor(arm), data = breastcancer)

# Plot
plot(fit, col = 1:2, main="Kaplan-Meier Estimates", xlab="Time in months", ylab="Survival Probability", mark.time=TRUE)
legend("bottomleft", legend=levels(factor(breastcancer$arm)), col=1:2, lty=1)

summary(fit, times = 30)


#2b
print(fit)

#2c
surv_prob_at_200 <- survfit(surv_obj ~ 1, data = breastcancer)
summary(surv_prob_at_200, times = 6.67)$surv

#2d
library(survival)

fit1 <- survfit(Surv(pfsmos, pfsstat) ~ arm, data = breastcancer, subset = (arm == 1))
fit2 <- survfit(Surv(pfsmos, pfsstat) ~ arm, data = breastcancer, subset = (arm == 2))

plot(fit1, fun = "F")
plot(fit2, fun = "F")

summary(fit1, times = 12)
summary(fit2, times = 12)


#2e
library(survival)

surv_obj <- Surv(time = breastcancer$pfsmos, event = breastcancer$pfsstat)
log_rank_test <- survdiff(surv_obj ~ factor(arm), data = breastcancer)

print(log_rank_test)

#2f
library(survival)
library(ggplot2)

surv_obj <- Surv(time = breastcancer$pfsmos, event = breastcancer$pfsstat)
fit_age <- survfit(surv_obj ~ factor(age), data = breastcancer)

# Plotting
plot(fit_age, col = 1:3, 
     xlab = "Months", 
     ylab = "Survival Probability",
     main = "Survival Curves by Age Category",
     mark.time = TRUE)


fit_subtype <- survfit(surv_obj ~ factor(tumor_subtype), data = breastcancer)

# Plotting
plot(fit_subtype, col = 1:2, 
     xlab = "Months", 
     ylab = "Survival Probability",
     main = "Survival Curves by Tumor Subtype",
     mark.time = TRUE)
legend("bottomleft", legend = levels(factor(breastcancer$tumor_subtype)), col = 1:2, lty = 1)


#3b
cox_model <- coxph(Surv(pfsmos, pfsstat) ~ factor(arm) + factor(age) + factor(tumor_subtype), data = breastcancer)
summary(cox_model)
library(broom)
cox_model %>%
  tidy(exp = TRUE,
       conf.int = TRUE) %>%
  knitr::kable(digits = 3)

#3d

full_model <- coxph(Surv(pfsmos, pfsstat) ~ factor(arm) + factor(age) + factor(tumor_subtype), data = breastcancer)
reduced_model <- coxph(Surv(pfsmos, pfsstat) ~ factor(arm) + factor(tumor_subtype), data = breastcancer)

lrt_result <- anova(reduced_model, full_model)
print(lrt_result)


#3f
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test)
