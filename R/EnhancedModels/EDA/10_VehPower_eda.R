################################################################################
# EDA (10): VehPower
################################################################################
rm(list = ls())

# Load in EDA data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load in data for GLM spec testing
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# 1). VehPower Summary and Distribution
################################################################################
# VehPower Summary
print(table(learn$VehPower))

# Unique VehPower values
length(unique(learn$VehPower))

# VehPower distribution (proportions)
print(prop.table(table(learn$VehPower)))



################################################################################
# 2). Frequency by VehPower
################################################################################
vehpower_analysis <- data.frame(
  VehPower = sort(unique(learn$VehPower)),
  Policies = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$VehPower == x)),
  Exposure = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$Exposure[learn$VehPower == x])),
  Claims = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$ClaimNb[learn$VehPower == x]))
)

vehpower_analysis$Frequency <- vehpower_analysis$Claims / vehpower_analysis$Exposure
vehpower_analysis$Pct_Policies <- 100 * vehpower_analysis$Policies / sum(vehpower_analysis$Policies)

# Frequency by VehPower
print(vehpower_analysis)

# Plot frequency by VehPower
plot(vehpower_analysis$VehPower, vehpower_analysis$Frequency, 
     type = "b", pch = 19, col = "#8d17f1", cex = 1.2,
     xlab = "Vehicle Power", ylab = "Frequency",
     main = "Frequency by Vehicle Power")
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)
grid()

# Frequency range 
round(max(vehpower_analysis$Frequency) - min(vehpower_analysis$Frequency), 4)


################################################################################
# Part 2: GLM Specification Testing for VehPower
################################################################################
##########################################################
# Specification 1: Paper - Categorical, merge >= 9 into single class
##########################################################
# Paper groups high powers (9-15) together: [4], [5], [6], [7], [8], [9+]
# Reduces sparsity in high power categories (9-15 only 10.9% combined)
train$VehPowerGLM <- as.factor(pmin(train$VehPower, 9))
validate$VehPowerGLM <- as.factor(pmin(validate$VehPower, 9))

spec1 <- glm(ClaimNb ~ VehPowerGLM, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))

##########################################################
# Specification 2: Volume-based grouping [4-7], [8-11], [12-15]
##########################################################
# Low power (78.8%), Medium power (18.9%), High power (2.5%)
train$VehPower_Vol <- cut(train$VehPower, breaks = c(3.5, 7.5, 11.5, 15.5), 
                          labels = c("Low", "Medium", "High"), right = TRUE)
validate$VehPower_Vol <- cut(validate$VehPower, breaks = c(3.5, 7.5, 11.5, 15.5),
                             labels = c("Low", "Medium", "High"), right = TRUE)

spec2 <- glm(ClaimNb ~ VehPower_Vol, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))

##########################################################
# Specification 3: Simple binary [4-7] vs [8-15]
##########################################################
# Common cars vs high-performance (tests if any signal exists)
train$VehPower_Binary <- ifelse(train$VehPower <= 7, "Low", "High")
validate$VehPower_Binary <- ifelse(validate$VehPower <= 7, "Low", "High")

spec3 <- glm(ClaimNb ~ VehPower_Binary, family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))

##########################################################
# Specification 4: Continuous (numeric)
##########################################################
# Test linear relationship (unlikely given noisy pattern)
spec4 <- glm(ClaimNb ~ VehPower, family = poisson(), data = train, offset = log(Exposure))
spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(fitted(spec4), train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(predict(spec4, newdata = validate, type = "response"), validate$ClaimNb)
spec4_params <- length(coef(spec4))

################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: [4],[5],[6],[7],[8],[9+]", 
                    "Volume groups [4-7],[8-11],[12-15]",
                    "Binary [4-7] vs [8-15]",
                    "Continuous"),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic

# VehPower Specification Comparison
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])

# Deciding to use paper's specification [4],[5],[6],[7],[8],[9+]
# Paper wins validation (33.083) vs all simplifications (33.110-33.114, +100 AIC)
# Despite VehPower being weakest predictor (1.5 univariate deviance), 6-level spec captures at least some signal
# Power 5 elevated (10.68%), Power 8 low (8.33%), pattern lost when grouped
# All simplifications (volume groups, binary, continuous) perform identically badly - grouping destroys signal
# Use paper's spec for replication, though effect tiny and may not survive in full model