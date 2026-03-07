################################################################################
# Enhanced GLM - Exact Paper Data + Our Improvements
################################################################################
rm(list = ls())

# Load required packages
library(splines)

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# Load Paper's Exact Data
################################################################################
train <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")



################################################################################
# Data Preparation, Some of Paper's Specs and All of my Improvements
################################################################################
# 1). Area: SAME as paper (6 categorical, convert to integer)
train$AreaGLM <- as.integer(as.factor(train$Area))
test$AreaGLM <- as.integer(as.factor(test$Area))

# 2). VehPower: SAME as paper (merge >= 9 into single class)
train$VehPowerGLM <- as.factor(pmin(train$VehPower, 9))
test$VehPowerGLM <- as.factor(pmin(test$VehPower, 9))

# 3). VehAge: OUR IMPROVEMENT [0], [1-14], [15+]
train$VehAge_Final <- cut(train$VehAge, breaks = c(-0.5, 0.5, 14.5, 1000),
                          labels = c("0", "1-14", "15+"), right = FALSE)
test$VehAge_Final <- cut(test$VehAge, breaks = c(-0.5, 0.5, 14.5, 1000),
                         labels = c("0", "1-14", "15+"), right = FALSE)

# 4). DrivAge: OUR IMPROVEMENT (spline DF=4, not 7 categorical classes)
# Applied directly in formula as ns(DrivAge, df = 4)

# 5). BonusMalus: SAME as paper (continuous, capped at 150)
train$BonusMalusGLM <- pmin(train$BonusMalus, 150)
test$BonusMalusGLM <- pmin(test$BonusMalus, 150)

# 6). Density: SAME as paper (log-density)
train$DensityGLM <- log(train$Density)
test$DensityGLM <- log(test$Density)

# 7). Region: SAME as paper (categorical with R24 ref)
train$Region <- relevel(as.factor(train$Region), ref = "R24")
test$Region <- relevel(as.factor(test$Region), ref = "R24")

# 8). VehBrand: OUR IMPROVEMENT (frequency tiers, not 11 levels)
train$VehBrand_Final <- ifelse(train$VehBrand == "B12", "High",
                               ifelse(train$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))
test$VehBrand_Final <- ifelse(test$VehBrand == "B12", "High",
                              ifelse(test$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))

# 9). VehGas: SAME as paper (binary factor)
train$VehGas <- as.factor(train$VehGas)
test$VehGas <- as.factor(test$VehGas)

# 10). Log-Exposure offset
train$logExposure <- log(train$Exposure)
test$logExposure <- log(test$Exposure)

################################################################################
# Fit Enhanced GLM with VehBrand × Region Interaction
################################################################################
enhanced_glm <- glm(ClaimNb ~ VehAge_Final + BonusMalusGLM + VehBrand_Final + 
                      AreaGLM + Region + DensityGLM + 
                      ns(DrivAge, df = 4) + VehGas + VehPowerGLM +
                      VehBrand_Final:Region,
                    family = poisson(), data = train, offset = logExposure)

# Evaluate on TRAIN
enhanced_aic <- AIC(enhanced_glm)
enhanced_train_dev <- Poisson.Deviance(fitted(enhanced_glm), train$ClaimNb)
enhanced_params <- length(coef(enhanced_glm))

# Evaluate on TEST
test_pred <- predict(enhanced_glm, newdata = test, type = "response")
enhanced_test_dev <- Poisson.Deviance(test_pred, test$ClaimNb)

# Enhanced GLM Results
# Data frame to store results
print(data.frame(
  Model = "Enhanced GLM",
  Params = enhanced_params,
  AIC = enhanced_aic,
  Train_Dev = enhanced_train_dev,
  Test_Dev = enhanced_test_dev
))


# Comparison to Paper's GLM1
# Paper's GLM1 test deviance: 32.17123
print(paste("Our Enhanced GLM test deviance:", round(enhanced_test_dev, 5)))
print(paste("Improvement:", round(32.17123 - enhanced_test_dev, 5)))
