################################################################################
# Interaction test: Region x VehBrand
################################################################################
rm(list = ls())
options(timeout = 600)

################################################################################
# Load data
################################################################################
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

library(splines)

Poisson.Deviance <- function(pred, obs){
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}

################################################################################
# Recreate the current best GLM data setup
################################################################################
area_levels <- levels(as.factor(learn$Area))
vehpower_levels <- as.character(sort(unique(pmin(learn$VehPower, 9))))
region_levels <- levels(as.factor(learn$Region))
brand_levels <- levels(as.factor(learn$VehBrand))
gas_levels <- levels(as.factor(learn$VehGas))

# Area
train$AreaGLM <- factor(match(train$Area, area_levels), levels = 1:length(area_levels))
validate$AreaGLM <- factor(match(validate$Area, area_levels), levels = 1:length(area_levels))

# VehPower
train$VehPowerGLM <- factor(pmin(train$VehPower, 9), levels = vehpower_levels)
validate$VehPowerGLM <- factor(pmin(validate$VehPower, 9), levels = vehpower_levels)

# VehAge
train$VehAgeCap <- pmin(train$VehAge, 20)
validate$VehAgeCap <- pmin(validate$VehAge, 20)

train$VehAge_3grp <- cut(train$VehAgeCap,
                         breaks = c(-0.5, 0.5, 12.5, 1000),
                         labels = c("0", "1_12", "13plus"),
                         right = FALSE)
train$VehAge_3grp <- relevel(train$VehAge_3grp, ref = "1_12")

validate$VehAge_3grp <- cut(validate$VehAgeCap,
                            breaks = c(-0.5, 0.5, 12.5, 1000),
                            labels = c("0", "1_12", "13plus"),
                            right = FALSE)
validate$VehAge_3grp <- relevel(validate$VehAge_3grp, ref = "1_12")

# BonusMalus
train$BonusMalusCap <- pmin(train$BonusMalus, 150)
validate$BonusMalusCap <- pmin(validate$BonusMalus, 150)

train$BM_is50 <- ifelse(train$BonusMalusCap == 50, 1, 0)
validate$BM_is50 <- ifelse(validate$BonusMalusCap == 50, 1, 0)

train$BM_above50 <- pmax(train$BonusMalusCap - 50, 0)
validate$BM_above50 <- pmax(validate$BonusMalusCap - 50, 0)

train$BM_above100 <- pmax(train$BonusMalusCap - 100, 0)
validate$BM_above100 <- pmax(validate$BonusMalusCap - 100, 0)

# Density
train$DensityGLM <- log(train$Density)
validate$DensityGLM <- log(validate$Density)

# Region
train$Region <- factor(train$Region, levels = region_levels)
train$Region <- relevel(train$Region, ref = "R24")

validate$Region <- factor(validate$Region, levels = region_levels)
validate$Region <- relevel(validate$Region, ref = "R24")

# VehBrand
train$VehBrand <- factor(train$VehBrand, levels = brand_levels)
train$VehBrand <- relevel(train$VehBrand, ref = "B1")

validate$VehBrand <- factor(validate$VehBrand, levels = brand_levels)
validate$VehBrand <- relevel(validate$VehBrand, ref = "B1")

# Grouped VehBrand helper
train$VehBrand_Final <- ifelse(train$VehBrand == "B12", "High",
                               ifelse(train$VehBrand %in% c("B3", "B4", "B5", "B11", "B13"),
                                      "Medium", "Low"))
validate$VehBrand_Final <- ifelse(validate$VehBrand == "B12", "High",
                                  ifelse(validate$VehBrand %in% c("B3", "B4", "B5", "B11", "B13"),
                                         "Medium", "Low"))

train$VehBrand_Final <- factor(train$VehBrand_Final, levels = c("Low", "Medium", "High"))
validate$VehBrand_Final <- factor(validate$VehBrand_Final, levels = c("Low", "Medium", "High"))

# Targeted B12 indicator
train$B12_only <- ifelse(train$VehBrand == "B12", 1, 0)
validate$B12_only <- ifelse(validate$VehBrand == "B12", 1, 0)

# VehGas
train$VehGas <- factor(train$VehGas, levels = gas_levels)
validate$VehGas <- factor(validate$VehGas, levels = gas_levels)

# Offset
train$logExposure <- log(train$Exposure)
validate$logExposure <- log(validate$Exposure)

################################################################################
# Fit current best GLM and interaction candidates
################################################################################
# Current best GLM at this stage:
# updated main effects + VehAge x VehGas + VehAge x VehPower
glm_base <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

# Add targeted B12 x Region interaction
glm_b12_region <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM +
    B12_only:Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

# Add grouped VehBrand_Final x Region interaction
glm_brandfinal_region <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM +
    VehBrand_Final:Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

# Add full VehBrand x Region interaction
glm_brand_region <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM +
    VehBrand:Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

################################################################################
# Compare using Poisson deviance
################################################################################
base_train_pred <- predict(glm_base, newdata = train, type = "response")
base_val_pred <- predict(glm_base, newdata = validate, type = "response")

b12_train_pred <- predict(glm_b12_region, newdata = train, type = "response")
b12_val_pred <- predict(glm_b12_region, newdata = validate, type = "response")

brandfinal_train_pred <- predict(glm_brandfinal_region, newdata = train, type = "response")
brandfinal_val_pred <- predict(glm_brandfinal_region, newdata = validate, type = "response")

brand_train_pred <- predict(glm_brand_region, newdata = train, type = "response")
brand_val_pred <- predict(glm_brand_region, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Current best GLM",
    "Add B12_only x Region",
    "Add VehBrand_Final x Region",
    "Add VehBrand x Region"
  ),
  Parameters = c(length(coef(glm_base)),
                 length(coef(glm_b12_region)),
                 length(coef(glm_brandfinal_region)),
                 length(coef(glm_brand_region))),
  AIC = c(AIC(glm_base),
          AIC(glm_b12_region),
          AIC(glm_brandfinal_region),
          AIC(glm_brand_region)),
  Train_Dev = c(Poisson.Deviance(base_train_pred, train$ClaimNb),
                Poisson.Deviance(b12_train_pred, train$ClaimNb),
                Poisson.Deviance(brandfinal_train_pred, train$ClaimNb),
                Poisson.Deviance(brand_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(base_val_pred, validate$ClaimNb),
              Poisson.Deviance(b12_val_pred, validate$ClaimNb),
              Poisson.Deviance(brandfinal_val_pred, validate$ClaimNb),
              Poisson.Deviance(brand_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Base <- comparison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Base <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Base <- comparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)

