################################################################################
# EDA (6): Density (checking for confounding with Area/Region)
################################################################################
rm(list = ls())

options(timeout = 100) # Learn not loading in in time again

# Load in required EDA data. 
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load in required data for fitting and testing GLM specs
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}

# Required package
library(splines)



################################################################################
# 1). Density Summary and Distribution
################################################################################
# Density Summary
print(summary(learn$Density))

# Unique Density values 
length(unique(learn$Density))



################################################################################
# 2). Correlation with Area
################################################################################
# Convert Area to numeric
learn$Area_Num <- as.numeric(factor(learn$Area, levels = c("A","B","C","D","E","F")))

# Correlation between Density and Area 
density_area_cor <- cor(learn$Density, learn$Area_Num)
round(density_area_cor, 3)

# Density and Area correlation: 0.589 (moderate, 35% shared variance)
# NOT redundant - both predictors needed in model
# Area = urbanisation category (A-F), Density = continuous population within area



################################################################################
# 3). Density by Region (focus on high-B12 regions)
################################################################################
density_by_region <- aggregate(Density ~ Region, data = learn, FUN = function(x) c(mean = mean(x), median = median(x)))
density_by_region <- data.frame(Region = density_by_region$Region, 
                                Mean_Density = density_by_region$Density[,1],
                                Median_Density = density_by_region$Density[,2])

print("Mean Density by Region (top 10):")
print(density_by_region[order(-density_by_region$Mean_Density), ][1:10, ])

# Check our high-B12 regions specifically
high_b12_regions <- c("R94", "R11", "R21", "R22")
print("Density in high-B12 regions (Cluster 3):")
print(density_by_region[density_by_region$Region %in% high_b12_regions, ])

# High-B12 regions (Cluster 3) have WILDLY different densities:
# R11: 8,081 (EXTREME, highest region) - frequency could be density-driven
# R21: 906, R22: 882, R94: 534 (LOW-medium)
# R94 key insight: low density (534) + extreme B12 (69.9%) + high freq (13.80%)
# R94's frequency CANNOT be explained by density alone - validates B12 rental effect



################################################################################
# 4). Frequency vs log(Density)
################################################################################
# Bin by log(Density) deciles
learn$LogDensity <- log(learn$Density)
density_deciles <- quantile(learn$LogDensity, probs = seq(0, 1, 0.1))

density_freq <- data.frame(
  Decile = 1:10,
  Avg_LogDensity = NA,
  Frequency = NA
)

for(i in 1:10) {
  lower <- density_deciles[i]
  upper <- density_deciles[i+1]
  mask <- learn$LogDensity >= lower & learn$LogDensity <= upper
  
  density_freq$Avg_LogDensity[i] <- mean(learn$LogDensity[mask])
  density_freq$Frequency[i] <- sum(learn$ClaimNb[mask]) / sum(learn$Exposure[mask])
}

print("Frequency by log(Density) deciles:")
print(density_freq)

# Quick plot
plot(density_freq$Avg_LogDensity, density_freq$Frequency, 
     type = "b", pch = 19, col = "#8d17f1",
     xlab = "Average log(Density)", ylab = "Frequency",
     main = "Frequency vs log(Density)")
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)

# Monotonic increase: 7.78% (low density) to 12.70% (high density) - 63% range
# Similar magnitude to Area effect (70%)
# Strong predictor - must control for Density when testing VehBrand x Region interaction
# Otherwise R11 high frequency attributed to B12 when it's actually density


# DECISION: Proceed with k-means k=4 for Region (Cluster 3 = R94/R11/R21/R22)
# Density confounds R11 (extreme density explains frequency), but R94 validates B12 effect (low density, high B12, high freq)
# CRITICAL: Must include log(Density) as control when testing VehBrand x Region interaction
# Going to test density main effect before moving to the interaction testing. 




################################################################################
# Density Specification Testing
################################################################################
# Create log(Density) for continuous specs
train$LogDensity <- log(train$Density)
validate$LogDensity <- log(validate$Density)



##########################################################
# Specification 1: Paper - log(Density) continuous
##########################################################
# Paper's baseline: log transformation handles right skew, linear relationship on log scale
spec1 <- glm(ClaimNb ~ LogDensity, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))



##########################################################
# Specification 2: Decile grouping
##########################################################
# EDA showed monotonic increase across deciles - test if grouping captures non-linearity
train$Density_Decile <- cut(train$LogDensity, breaks = quantile(train$LogDensity, probs = seq(0, 1, 0.1)), 
                            labels = 1:10, include.lowest = TRUE)
validate$Density_Decile <- cut(validate$LogDensity, breaks = quantile(train$LogDensity, probs = seq(0, 1, 0.1)),
                               labels = 1:10, include.lowest = TRUE)

spec2 <- glm(ClaimNb ~ Density_Decile, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))



##########################################################
# Specification 3: Spline (DF=3)
##########################################################
# Test if non-linearity exists beyond log transformation
spec3 <- glm(ClaimNb ~ ns(LogDensity, df = 3), family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))



##########################################################
# Specification 4: Simple tercile grouping [Low, Medium, High]
##########################################################
# Simpler grouping if deciles overfit
train$Density_Tercile <- cut(train$LogDensity, breaks = quantile(train$LogDensity, probs = c(0, 0.33, 0.67, 1)),
                             labels = c("Low", "Medium", "High"), include.lowest = TRUE)
validate$Density_Tercile <- cut(validate$LogDensity, breaks = quantile(train$LogDensity, probs = c(0, 0.33, 0.67, 1)),
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE)

spec4 <- glm(ClaimNb ~ Density_Tercile, family = poisson(), data = train, offset = log(Exposure))
spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(fitted(spec4), train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(predict(spec4, newdata = validate, type = "response"), validate$ClaimNb)
spec4_params <- length(coef(spec4))



################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: log(Density) continuous", "Decile grouping", "Spline (DF=3)", "Tercile grouping"),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ]) 
# While yes, Spline (DF=3) wins validation by 0.00121 deviance (32.982 vs 32.983)
# Difference is completely negligible (~100x smaller than BonusMalus, ~10x smaller than Area)
# Paper's log(Density) has better AIC (spline +3.3 AIC for 2 extra params)
# Decile grouping overfits (10 params, +4.5 AIC, no validation improvement)

# DECISION: Use paper's log(Density) continuous specification
# Validation performance essentially identical (0.00121 difference is noise)
# Simpler (2 params vs 4), better AIC, more interpretable
# CRITICAL: Density is control variable for VehBrand x Region interaction, simpler should be way better
# Paper got this one right, log transformation captures relationship adequately



################################################################################
# Part 3: Save Essential Plots for Report
################################################################################
# Plot: Frequency vs log(Density) showing monotonic increase
png("figs/Density_figs/01_frequency_vs_logdensity.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(density_freq$Avg_LogDensity, density_freq$Frequency, 
     type = "b", pch = 19, col = "#8d17f1",
     xlab = "Average log(Density)", ylab = "Frequency",
     main = "Frequency vs log(Density) by Decile", lwd = 2, cex = 1.2)
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)
grid()
dev.off()
