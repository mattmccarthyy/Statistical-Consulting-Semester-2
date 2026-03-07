################################################################################
# EDA (8): DrivAge
################################################################################
rm(list = ls())
options(timeout = 600)

# Load required library
library(splines)

# Load required dataset for EDA
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load data required for GLM Spec fitting and validation. 
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# 1). DrivAge Summary and Distribution
################################################################################
 # DrivAge Summary
print(summary(learn$DrivAge))

#Unique DrivAge values
length(unique(learn$DrivAge))))


# Age distribution
age_dist <- table(learn$DrivAge)

# DrivAge distribution (first 20)
print(head(age_dist, 20))



################################################################################
# 2). Frequency by DrivAge
################################################################################
drivage_analysis <- data.frame(
  DrivAge = sort(unique(learn$DrivAge)),
  Policies = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$DrivAge == x)),
  Exposure = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$Exposure[learn$DrivAge == x])),
  Claims = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$ClaimNb[learn$DrivAge == x]))
)

drivage_analysis$Frequency <- drivage_analysis$Claims / drivage_analysis$Exposure
drivage_analysis$Pct_Policies <- 100 * drivage_analysis$Policies / sum(drivage_analysis$Policies)

# Frequency by DrivAge (first 30)
print(head(drivage_analysis, 30))

# Frequency by DrivAge (ages 60+)
print(drivage_analysis[drivage_analysis$DrivAge >= 60, ])

# Plot frequency by age
plot(drivage_analysis$DrivAge, drivage_analysis$Frequency, 
     type = "p", pch = 19, col = "#8d17f1", cex = 0.8,
     xlab = "Driver Age", ylab = "Frequency",
     main = "Frequency by Driver Age")
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)
grid()



################################################################################
# 3). Check for Young Driver Effect
################################################################################
young_drivers <- drivage_analysis[drivage_analysis$DrivAge <= 25, ]
print("Young drivers (<= 25) analysis:")
print(young_drivers)

# Ages 18-21 average frequency
round(mean(drivage_analysis$Frequency[drivage_analysis$DrivAge >= 18 & drivage_analysis$DrivAge <= 21]), 4)

# Ages 22-25 average frequency
round(mean(drivage_analysis$Frequency[drivage_analysis$DrivAge >= 22 & drivage_analysis$DrivAge <= 25]), 4)

# Ages 26-60 average frequency
round(mean(drivage_analysis$Frequency[drivage_analysis$DrivAge >= 26 & drivage_analysis$DrivAge <= 60]), 4)



################################################################################
# Part 2: GLM Specification 
################################################################################
##########################################################
# Specification 1: Paper - 7 categorical classes
##########################################################
# Paper's breaks: [18,21), [21,26), [26,31), [31,41), [41,51), [51,71), [71, \infty)
# Reference level [41,51) - stable middle age group
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, Inf)
train$DrivAgeGLM <- cut(train$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
train$DrivAgeGLM <- relevel(train$DrivAgeGLM, ref = "5")
validate$DrivAgeGLM <- cut(validate$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
validate$DrivAgeGLM <- relevel(validate$DrivAgeGLM, ref = "5")

spec1 <- glm(ClaimNb ~ DrivAgeGLM, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))



##########################################################
# Specification 2: Splines with df = 2 to 9
##########################################################
# Data frame to store results
spline_results <- data.frame(
  DF = 2:9,
  AIC = NA,
  Train_Dev = NA,
  Val_Dev = NA,
  Params = NA
)

for(i in 1:nrow(spline_results)) {
  df_val <- spline_results$DF[i]
  
  model <- glm(ClaimNb ~ ns(DrivAge, df = df_val), family = poisson(), 
               data = train, offset = log(Exposure))
  
  spline_results$AIC[i] <- AIC(model)
  spline_results$Train_Dev[i] <- Poisson.Deviance(fitted(model), train$ClaimNb)
  spline_results$Val_Dev[i] <- Poisson.Deviance(predict(model, newdata = validate, type = "response"), 
                                                validate$ClaimNb)
  spline_results$Params[i] <- length(coef(model))
}

print("Spline AIC by degrees of freedom:")
print(spline_results)

# Plot AIC vs DF
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))
plot(spline_results$DF, spline_results$AIC, type = "b", pch = 19, col = "#8d17f1",
     xlab = "Spline Degrees of Freedom", ylab = "AIC",
     main = "DrivAge Spline AIC vs Degrees of Freedom", lwd = 2, cex = 1.2)
abline(h = spec1_aic, col = "red", lty = 2, lwd = 2)
text(7, spec1_aic + 5, "Paper (7 classes)", col = "red", cex = 1.1)
grid()



##########################################################
# Find best spline (elbow at DF=4)
##########################################################
best_spline <- spline_results[spline_results$DF == 4, ]

print("Best spline specification (elbow at DF=4):")
print(best_spline)



##########################################################
# Comparison Table: Paper vs Best Spline
##########################################################
comparison <- data.frame(
  Specification = c("Paper: 7 categorical classes", 
                    paste0("Spline (DF=", best_spline$DF, ")")),
  Params = c(spec1_params, best_spline$Params),
  AIC = c(spec1_aic, best_spline$AIC),
  Train_Dev = c(spec1_train_dev, best_spline$Train_Dev),
  Val_Dev = c(spec1_val_dev, best_spline$Val_Dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic

# Paper vs Best Spline
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])

# Results: Spline DF=4 dominates paper's 7 categorical classes
# AIC improvement: -95.5, Validation improvement: -0.021 deviance 
# Spline uses fewer parameters (5 vs 7) yet performs better

# EDA showed smooth continuous decline: Age 18 (32.6%) to Age 30 (9.3%), not discrete jumps
# Paper's rigid breaks [18,21), [21,26), [26,31) force artificial categorical steps
# Spline captures natural aging curve: young driver premium gradually declines

# Elbow point in AIC plot at df = 4 - last major improvement before plateau
# DF=4 to DF=5 actually increases AIC (+1.8), then need DF=7-8 for more gains
# DF=4 sufficient to capture: (1) steep young driver decline, (2) middle-age stability, (3) minor variation
# Validation only 0.005 worse than DF=8 but saves 3 parameters

# DECISION: Going to use Spline DF=4 for DrivAge
# Clear improvement over paper, theoretically sound (aging is continuous), parsimonious
# No bootstrap needed - improvement decisive and makes sense



################################################################################
# Plot: Actual Frequency vs Spline Fit (DF=4)
################################################################################
# Fit spline DF=4 model
spline_model <- glm(ClaimNb ~ ns(DrivAge, df = 4), family = poisson(), 
                    data = train, offset = log(Exposure))

# Create prediction data across age range
pred_ages <- data.frame(DrivAge = 18:100, Exposure = 1)
pred_freq <- predict(spline_model, newdata = pred_ages, type = "response")

# Plot
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(drivage_analysis$DrivAge, drivage_analysis$Frequency, 
     pch = 19, col = "darkgrey", cex = 0.8,
     xlab = "Driver Age", ylab = "Frequency",
     main = "Frequency vs Driver Age: Actual vs Spline DF=4",
     ylim = c(0, 0.35))

lines(pred_ages$DrivAge, pred_freq, col = "#8d17f1", lwd = 3)

abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)

legend("topright", 
       legend = c("Actual Frequency", "Spline DF=4 Fit", "Overall Average"),
       col = c("darkgrey", "#8d17f1", "red"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 3, 2),
       cex = 1.1)

grid()



################################################################################
# Part 3: Save Essential Plots for Report
################################################################################
# Plot 1: Frequency by Driver Age (raw data)
png("figs/DrivAge_figs/01_frequency_by_age.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(drivage_analysis$DrivAge, drivage_analysis$Frequency, 
     type = "p", pch = 19, col = "#8d17f1", cex = 0.8,
     xlab = "Driver Age", ylab = "Frequency",
     main = "Frequency by Driver Age")
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)
grid()
dev.off()

# Plot 2: Spline AIC vs Degrees of Freedom
png("figs/DrivAge_figs/02_spline_AIC_comparison.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(spline_results$DF, spline_results$AIC, type = "b", pch = 19, col = "#8d17f1",
     xlab = "Spline Degrees of Freedom", ylab = "AIC",
     main = "DrivAge Spline AIC vs Degrees of Freedom", lwd = 2, cex = 1.2)
abline(h = spec1_aic, col = "red", lty = 2, lwd = 2)
text(7, spec1_aic + 5, "Paper (7 classes)", col = "red", cex = 1.1)
grid()
dev.off()

# Plot 3: Actual Frequency vs Spline Fit
png("figs/DrivAge_figs/03_actual_vs_spline_fit.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(drivage_analysis$DrivAge, drivage_analysis$Frequency, 
     pch = 19, col = "darkgrey", cex = 0.8,
     xlab = "Driver Age", ylab = "Frequency",
     main = "Frequency vs Driver Age: Actual vs Spline DF=4",
     ylim = c(0, 0.35))
lines(pred_ages$DrivAge, pred_freq, col = "#8d17f1", lwd = 3)
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("Actual Frequency", "Spline DF=4 Fit", "Overall Average"),
       col = c("darkgrey", "#8d17f1", "red"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 3, 2),
       cex = 1.1)
grid()
dev.off()
