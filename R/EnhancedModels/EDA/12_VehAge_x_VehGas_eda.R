################################################################################
# Interaction test: VehAge x VehGas
################################################################################
rm(list = ls())

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
# Recreate the current best main-effect GLM
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
glm_base <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_vage_vgas <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region +
    VehAge_3grp:VehGas,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

################################################################################
# Compare using Poisson deviance
################################################################################
base_train_pred <- predict(glm_base, newdata = train, type = "response")
base_val_pred <- predict(glm_base, newdata = validate, type = "response")

int_train_pred <- predict(glm_vage_vgas, newdata = train, type = "response")
int_val_pred <- predict(glm_vage_vgas, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Current best main-effects GLM",
    "Add VehAge_3grp x VehGas"
  ),
  Parameters = c(length(coef(glm_base)),
                 length(coef(glm_vage_vgas))),
  AIC = c(AIC(glm_base),
          AIC(glm_vage_vgas)),
  Train_Dev = c(Poisson.Deviance(base_train_pred, train$ClaimNb),
                Poisson.Deviance(int_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(base_val_pred, validate$ClaimNb),
              Poisson.Deviance(int_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Base <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Base <- crison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Base <-omparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)
# Adding VehAge x VehGas gives a large improvement in both AIC and validation deviance.

# The gain is much larger than for the earlier main-effect changes,
# and it comes from only 2 extra parameters.

# Train and validation improve by almost the same amount,
# so this interaction looks stable rather than overfit.

# Going to keep VehAge_3grp x VehGas in the model.


## Additional Plot for report
################################################################################
# Empirical interaction table and plot: VehAge_3grp x VehGas
################################################################################
# Recreate the grouped variables on the full learning sample
# Need to work on this code block, keep getting errors. 
learn$VehAgeCap <- pmin(learn$VehAge, 20)

learn$VehAge_3grp <- cut(learn$VehAgeCap,
                         breaks = c(-0.5, 0.5, 12.5, 1000),
                         labels = c("0", "1_12", "13plus"),
                         right = FALSE)
learn$VehAge_3grp <- relevel(learn$VehAge_3grp, ref = "1_12")

learn$VehGas <- factor(learn$VehGas, levels = levels(train$VehGas))

# Pooled frequencies by VehAge group and fuel type
vg_tab <- aggregate(cbind(Exposure, ClaimNb) ~ VehAge_3grp + VehGas,
                    data = learn, FUN = sum)

vg_tab$Frequency <- vg_tab$ClaimNb / vg_tab$Exposure
vg_tab$SE <- sqrt(vg_tab$Frequency / vg_tab$Exposure)
vg_tab$CI_lower <- pmax(0, vg_tab$Frequency - 1.96 * vg_tab$SE)
vg_tab$CI_upper <- vg_tab$Frequency + 1.96 * vg_tab$SE

vg_tab <- vg_tab[order(vg_tab$VehGas, vg_tab$VehAge_3grp), ]
print(vg_tab)

################################################################################
# Plot
################################################################################
x <- 1:3
x_labels <- c("[0,1)", "[1,13)", "[13,\\infty)")

gas1 <- levels(learn$VehGas)[1]
gas2 <- levels(learn$VehGas)[2]

tab1 <- vg_tab[vg_tab$VehGas == gas1, ]
tab2 <- vg_tab[vg_tab$VehGas == gas2, ]

ideal_order <- c("0", "1_12", "13plus")

tab1 <- tab1[match(ideal_order, tab1$VehAge_3grp), ]
tab2 <- tab2[match(ideal_order, tab2$VehAge_3grp), ]

x1 <- x - 0.04
x2 <- x + 0.04

{
  plot(NA, NA,
       xlim = c(0.8, 3.2),
       ylim = c(0, max(tab1$CI_upper, tab2$CI_upper) * 1.08),
       xaxt = "n",
       xlab = "VehAge group", ylab = "Frequency")
  grid()
  
  arrows(x1, tab1$CI_lower, x1, tab1$CI_upper,
         angle = 90, code = 3, length = 0.08, lwd = 1.2, col = "black")
  
  arrows(x2, tab2$CI_lower, x2, tab2$CI_upper,
         angle = 90, code = 3, length = 0.08, lwd = 1.2, col = "red")
  
  lines(x1, tab1$Frequency, lwd = 2.2, col = "black")
  points(x1, tab1$Frequency, pch = 19, cex = 1, col = "black")
  
  lines(x2, tab2$Frequency, lwd = 2.2, col = "red")
  points(x2, tab2$Frequency, pch = 17, cex = 1.1, col = "red")
  
  h1 <- sum(learn$ClaimNb) / sum(learn$Exposure)
  abline(h = h1, col = "#8d17f1", lty = 2, lwd = 2.2)
  
  axis(1, at = x, labels = c("[0,1)", "[1,13)", "13+"))
  
  legend("topright",
         legend = c(gas1, gas2, "Overall frequency"),
         col = c("black", "red", "#8d17f1"),
         lty = c(1, 1, 2),
         lwd = c(1.8, 1.8, 2),
         pch = c(19, 17, NA),
         pt.cex = c(1, 1.1, NA),
         bty = "box",
         cex = 1.0)
}

