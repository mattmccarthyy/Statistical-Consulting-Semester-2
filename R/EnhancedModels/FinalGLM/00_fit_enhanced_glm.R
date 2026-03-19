################################################################################
# Fitting and testing final improved GLM 
################################################################################
rm(list = ls())
options(timeout = 600)

# Age being modelled as a spline on 5 df. 
library(splines)

# Comparison metrics
Poisson.Deviance <- function(pred, obs) {
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}


################################################################################
# Load train/test data
################################################################################
train <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test  <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")



################################################################################
# Pre-processing (all changes outlined in EDA)
################################################################################
# 1). Area: categorical with same train-set level structure
area_levels <- levels(as.factor(train$Area))
train$AreaGLM <- factor(match(train$Area, area_levels), levels = 1:length(area_levels))
test$AreaGLM  <- factor(match(test$Area, area_levels), levels = 1:length(area_levels))

# 2). VehPower: categorical, merge >= 9
vehpower_levels <- as.character(sort(unique(pmin(train$VehPower, 9))))
train$VehPowerGLM <- factor(pmin(train$VehPower, 9), levels = vehpower_levels)
test$VehPowerGLM  <- factor(pmin(test$VehPower, 9), levels = vehpower_levels)

# 3). VehAge: updated specification [0], [1-12], [13+]
train$VehAgeCap <- pmin(train$VehAge, 20)
test$VehAgeCap  <- pmin(test$VehAge, 20)

train$VehAge_3grp <- cut(
  train$VehAgeCap,
  breaks = c(-0.5, 0.5, 12.5, 1000),
  labels = c("0", "1_12", "13plus"),
  right = FALSE
)
train$VehAge_3grp <- relevel(train$VehAge_3grp, ref = "1_12")

test$VehAge_3grp <- cut(
  test$VehAgeCap,
  breaks = c(-0.5, 0.5, 12.5, 1000),
  labels = c("0", "1_12", "13plus"),
  right = FALSE
)
test$VehAge_3grp <- relevel(test$VehAge_3grp, ref = "1_12")

# 4). DrivAge: updated spline df = 5
# Including in the GLM call. 

# 5). BonusMalus: updated mass point at 50 + hinge at 100
train$BonusMalusCap <- pmin(train$BonusMalus, 150)
test$BonusMalusCap  <- pmin(test$BonusMalus, 150)

train$BM_is50     <- ifelse(train$BonusMalusCap == 50, 1, 0)
test$BM_is50      <- ifelse(test$BonusMalusCap == 50, 1, 0)
train$BM_above50  <- pmax(train$BonusMalusCap - 50, 0)
test$BM_above50   <- pmax(test$BonusMalusCap - 50, 0)
train$BM_above100 <- pmax(train$BonusMalusCap - 100, 0)
test$BM_above100  <- pmax(test$BonusMalusCap - 100, 0)

# 6). Density: log-density
train$DensityGLM <- log(train$Density)
test$DensityGLM  <- log(test$Density)

# 7). Region: categorical with R24 reference
region_levels <- levels(as.factor(train$Region))
train$Region <- factor(train$Region, levels = region_levels)
train$Region <- relevel(train$Region, ref = "R24")
test$Region  <- factor(test$Region, levels = region_levels)
test$Region  <- relevel(test$Region, ref = "R24")

# 8). VehBrand: categorical with B1 reference
brand_levels <- levels(as.factor(train$VehBrand))
train$VehBrand <- factor(train$VehBrand, levels = brand_levels)
train$VehBrand <- relevel(train$VehBrand, ref = "B1")
test$VehBrand  <- factor(test$VehBrand, levels = brand_levels)
test$VehBrand  <- relevel(test$VehBrand, ref = "B1")

# Targeted B12 indicator for the retained interaction
train$B12_only <- ifelse(train$VehBrand == "B12", 1, 0)
test$B12_only  <- ifelse(test$VehBrand == "B12", 1, 0)

# 9). Log-exposure offset
train$logExposure <- log(train$Exposure)
test$logExposure  <- log(test$Exposure)

# 10). VehGas: binary factor
gas_levels <- levels(as.factor(train$VehGas))
train$VehGas <- factor(train$VehGas, levels = gas_levels)
test$VehGas  <- factor(test$VehGas, levels = gas_levels)



################################################################################
# Fit final improved GLM
################################################################################
final_glm <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp + ns(DrivAge, df = 5) +
    BM_is50 + BM_above50 + BM_above100 + VehBrand + VehGas +
    DensityGLM + Region + VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM + B12_only:Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)


################################################################################
# Evaluate final improved GLM
################################################################################
train_pred <- predict(final_glm, newdata = train, type = "response")
test_pred  <- predict(final_glm, newdata = test, type = "response")

final_results <- data.frame(
  Model = c("Model GLM1", "Final improved GLM"),
  Parameters = c(49, length(coef(final_glm))),
  AIC = c(253062, AIC(final_glm)),
  Train_Dev = c(31.26738, Poisson.Deviance(train_pred, train$ClaimNb)),
  Test_Dev = c(32.17123, Poisson.Deviance(test_pred, test$ClaimNb))
)

print(final_results)



################################################################################
# Saving all to push to GitHub
################################################################################
write.csv(final_results, file = "R/EnhancedModels/FinalGLM/GLM1_v_EnhancedGLM_Comparison")

# Re-using strip GLM from re-producing GLM's script.
{
  strip_glm <- function(mod) {
    ### NOTE: This is taken from online, not original work.
    # 1). Removing model frame and orig. data 
    mod$data <- NULL
    mod$model <- NULL
    mod$y <- NULL
    
    # 2). Remove working vectors used for training diagnostics
    # (recalculated anyway when using predict() on new data)
    mod$residuals <- NULL
    mod$fitted.values <- NULL
    mod$effects <- NULL
    mod$linear.predictors <- NULL
    mod$weights <- NULL
    mod$prior.weights <- NULL
    
    # 3). Clean up the QR decomposition to save further space
    # Just removing memory of fitting process, don't need this matrix
    mod$qr$qr <- NULL 
    
    return(mod)
  }
}
# Applying to models
final_glm.push <- strip_glm(final_glm)
saveRDS(final_glm.push, file = "R/EnhancedModels/FinalGLM/EnhancedGLM")

# Saving the datasets too incase I want them in the XGBoost
saveRDS(train, file = "C:/Users/matth/Desktop/Statistical Consultancy/Semester 2/R/EnhancedModels/data/EnhancedGLMDataset/Train.rds")
saveRDS(test, file = "C:/Users/matth/Desktop/Statistical Consultancy/Semester 2/R/EnhancedModels/data/EnhancedGLMDataset/Test.rds")
