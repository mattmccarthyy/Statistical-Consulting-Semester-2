################################################################################
# Interaction test: VehAge x VehPower
################################################################################
rm(list = ls())
options(timeout=600)

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

# VehGas
train$VehGas <- factor(train$VehGas, levels = gas_levels)
validate$VehGas <- factor(validate$VehGas, levels = gas_levels)

# Offset
train$logExposure <- log(train$Exposure)
validate$logExposure <- log(validate$Exposure)

################################################################################
# Fit base model and interaction model
################################################################################
# Current best GLM at this stage: main effects + VehAge x VehGas
glm_base <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

# Add VehAge x VehPower
glm_vage_vpower <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas +
    VehAge_3grp:VehPowerGLM,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

################################################################################
# Compare using Poisson deviance
################################################################################
base_train_pred <- predict(glm_base, newdata = train, type = "response")
base_val_pred <- predict(glm_base, newdata = validate, type = "response")

int_train_pred <- predict(glm_vage_vpower, newdata = train, type = "response")
int_val_pred <- predict(glm_vage_vpower, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Current best GLM",
    "Add VehAge_3grp x VehPowerGLM"
  ),
  Parameters = c(length(coef(glm_base)),
                 length(coef(glm_vage_vpower))),
  AIC = c(AIC(glm_base),
          AIC(glm_vage_vpower)),
  Train_Dev = c(Poisson.Deviance(base_train_pred, train$ClaimNb),
                Poisson.Deviance(int_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(base_val_pred, validate$ClaimNb),
              Poisson.Deviance(int_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Base <- comparison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Base <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Base <- comparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)

# Adding VehAge x VehPower improves both AIC and validation deviance.
# The gain is smaller than for VehAge x VehGas, but still material.
# Keeping it for the current best model.


## Plot for the report
# Recreate grouped variables on full learning sample
learn$VehAgeCap <- pmin(learn$VehAge, 20)

learn$VehAge_3grp <- cut(learn$VehAgeCap,
                         breaks = c(-0.5, 0.5, 12.5, 1000),
                         labels = c("0", "1_12", "13plus"),
                         right = FALSE)
learn$VehAge_3grp <- relevel(learn$VehAge_3grp, ref = "1_12")

learn$VehPowerGLM <- factor(pmin(learn$VehPower, 9),
                            levels = levels(train$VehPowerGLM))

# Pooled frequencies by VehAge group and VehPowerGLM
vp_tab <- aggregate(cbind(Exposure, ClaimNb) ~ VehAge_3grp + VehPowerGLM,
                    data = learn, FUN = sum)

vp_tab$Frequency <- vp_tab$ClaimNb / vp_tab$Exposure
vp_tab$SE <- sqrt(vp_tab$Frequency / vp_tab$Exposure)
vp_tab$CI_lower <- pmax(0, vp_tab$Frequency - 1.96 * vp_tab$SE)
vp_tab$CI_upper <- vp_tab$Frequency + 1.96 * vp_tab$SE

vp_tab <- vp_tab[order(vp_tab$VehAge_3grp, vp_tab$VehPowerGLM), ]
print(vp_tab)



x <- seq_along(levels(learn$VehPowerGLM))
x_labels <- levels(learn$VehPowerGLM)
age_order <- c("0", "1_12", "13plus")

tab0  <- vp_tab[vp_tab$VehAge_3grp == "0", ]
tab12 <- vp_tab[vp_tab$VehAge_3grp == "1_12", ]
tab13 <- vp_tab[vp_tab$VehAge_3grp == "13plus", ]

tab0  <- tab0[match(levels(learn$VehPowerGLM), tab0$VehPowerGLM), ]
tab12 <- tab12[match(levels(learn$VehPowerGLM), tab12$VehPowerGLM), ]
tab13 <- tab13[match(levels(learn$VehPowerGLM), tab13$VehPowerGLM), ]

x0  <- x - 0.08
x12 <- x
x13 <- x + 0.08

# Actually plotting
{
  plot(NA, NA,
       xlim = c(0.7, length(x) + 0.3),
       ylim = c(0, max(vp_tab$CI_upper) * 1.08),
       xaxt = "n",
       xlab = "VehPowerGLM", ylab = "Frequency")
  
  grid()
  
  arrows(x0, tab0$CI_lower, x0, tab0$CI_upper,
         angle = 90, code = 3, length = 0.06, lwd = 1.1, col = "black")
  arrows(x12, tab12$CI_lower, x12, tab12$CI_upper,
         angle = 90, code = 3, length = 0.06, lwd = 1.1, col = "red")
  arrows(x13, tab13$CI_lower, x13, tab13$CI_upper,
         angle = 90, code = 3, length = 0.06, lwd = 1.1, col = "#1f77b4")
  
  lines(x0, tab0$Frequency, lwd = 1.8, col = "black")
  points(x0, tab0$Frequency, pch = 19, cex = 1.0, col = "black")
  
  lines(x12, tab12$Frequency, lwd = 1.8, col = "red")
  points(x12, tab12$Frequency, pch = 17, cex = 1.1, col = "red")
  
  lines(x13, tab13$Frequency, lwd = 1.8, col = "#1f77b4")
  points(x13, tab13$Frequency, pch = 15, cex = 1.0, col = "#1f77b4")
  
  h1 <- sum(learn$ClaimNb) / sum(learn$Exposure)
  abline(h = h1, col = "#8d17f1", lty = 2, lwd = 2)
  
  axis(1, at = x, labels = x_labels)
  
  legend("topright",
         legend = c("[0,1)", "[1,13)", "13+", "Overall frequency"),
         col = c("black", "red", "#1f77b4", "#8d17f1"),
         lty = c(1, 1, 1, 2),
         lwd = c(1.8, 1.8, 1.8, 2),
         pch = c(19, 17, 15, NA),
         pt.cex = c(1.0, 1.1, 1.0, NA),
         bty = "box",
         cex = 1.0)
}
#RETURN HERE WHEN READY
#NEED TO COMBINE OUTPUT AND PLOT FOR REPORT. 

