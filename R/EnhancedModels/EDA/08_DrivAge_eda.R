################################################################################
# EDA: DrivAge
################################################################################
rm(list = ls())
options(timeout = 600)

################################################################################
# Load data
################################################################################
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Helper
Poisson.Deviance <- function(pred, obs){
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}


################################################################################
# 1). Basic distribution
################################################################################
summary(learn$DrivAge)
range(learn$DrivAge)
length(unique(learn$DrivAge))
table(learn$DrivAge)
# Older ages way too sparse.

################################################################################
# 2). Frequency by exact driver age
################################################################################
drivage_analysis <- data.frame(
  DrivAge = sort(unique(learn$DrivAge)),
  Policies = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$DrivAge == x)),
  Exposure = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$Exposure[learn$DrivAge == x])),
  Claims = sapply(sort(unique(learn$DrivAge)), function(x) sum(learn$ClaimNb[learn$DrivAge == x]))
)

drivage_analysis$Frequency <- drivage_analysis$Claims / drivage_analysis$Exposure
drivage_analysis$SE <- sqrt(drivage_analysis$Frequency / drivage_analysis$Exposure)
drivage_analysis$CI_lower <- pmax(0, drivage_analysis$Frequency - 1.96 * drivage_analysis$SE)
drivage_analysis$CI_upper <- drivage_analysis$Frequency + 1.96 * drivage_analysis$SE

drivage_analysis

# This is the main table.
# It shows the raw shape of the age effect.


################################################################################
# 3). Restrict to ages with enough exposure
################################################################################
drivage_analysis[drivage_analysis$Exposure >= 500,
                 c("DrivAge", "Exposure", "Claims", "Frequency", "CI_lower", "CI_upper")]

# This removes the noisiest tail.
# Use this to judge the broad pattern.


################################################################################
# 4). Plot frequency by driver age
################################################################################
drivage_analysis[drivage_analysis$CI_lower == drivage_analysis$CI_upper,
                 c("DrivAge", "Exposure", "Claims", "Frequency", "CI_lower", "CI_upper")]
# Few CI's that have identical upper and lower bounds, because no claims at those ages
# So below gives a warning, but can ignore. 
plot(drivage_analysis$DrivAge, drivage_analysis$Frequency,
     type = "n",
     xlab = "Driver Age", ylab = "Frequency")

grid()

arrows(drivage_analysis$DrivAge, drivage_analysis$CI_lower,
       drivage_analysis$DrivAge, drivage_analysis$CI_upper,
       angle = 90, code = 3, length = 0.08, lwd = 1.2)

lines(drivage_analysis$DrivAge, drivage_analysis$Frequency, lwd = 1.8)
points(drivage_analysis$DrivAge, drivage_analysis$Frequency,
       pch = 19, cex = 1)

h1 <- sum(learn$ClaimNb) / sum(learn$Exposure)
abline(h = h1, col = "#8d17f1", lty = 2, lwd = 2)
text(18 + 4.5, h1 - 0.019,labels = "Overall frequency", col = "#8d17f1", cex = 1.1)
# The plot is mainly to see whether the effect looks stepped or smooth.


################################################################################
# 5). Paper groups
################################################################################
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, Inf)

# Using the papers bins to see if they line up with the actual patterns.
drivage_paper_groups <- data.frame(
  group = c("18_20", "21_25", "26_30", "31_40", "41_50", "51_70", "71plus"),
  exposure = c(
    sum(learn$Exposure[learn$DrivAge >= 18 & learn$DrivAge < 21]),
    sum(learn$Exposure[learn$DrivAge >= 21 & learn$DrivAge < 26]),
    sum(learn$Exposure[learn$DrivAge >= 26 & learn$DrivAge < 31]),
    sum(learn$Exposure[learn$DrivAge >= 31 & learn$DrivAge < 41]),
    sum(learn$Exposure[learn$DrivAge >= 41 & learn$DrivAge < 51]),
    sum(learn$Exposure[learn$DrivAge >= 51 & learn$DrivAge < 71]),
    sum(learn$Exposure[learn$DrivAge >= 71])
  ),
  claims = c(
    sum(learn$ClaimNb[learn$DrivAge >= 18 & learn$DrivAge < 21]),
    sum(learn$ClaimNb[learn$DrivAge >= 21 & learn$DrivAge < 26]),
    sum(learn$ClaimNb[learn$DrivAge >= 26 & learn$DrivAge < 31]),
    sum(learn$ClaimNb[learn$DrivAge >= 31 & learn$DrivAge < 41]),
    sum(learn$ClaimNb[learn$DrivAge >= 41 & learn$DrivAge < 51]),
    sum(learn$ClaimNb[learn$DrivAge >= 51 & learn$DrivAge < 71]),
    sum(learn$ClaimNb[learn$DrivAge >= 71])
  )
)

drivage_paper_groups$frequency <- drivage_paper_groups$claims / drivage_paper_groups$exposure
drivage_paper_groups
# Strong young-driver effect.
# Frequency falls quickly up to about age 30.

# After about 30, the pattern is much flatter.
# There are no clear hard cutpoints.

# So grouped-factor alternatives are not strongly supported here.
# The main question is whether a smooth spline improves on the paper's bins.

# The only DrivAge specifications worth testing next are:
# (i) the paper's 7 classes
# (ii) a spline with df = 4
# (iii) a spline with df = 5



################################################################################
# PART 2: Compare DrivAge specifications inside the full GLM
################################################################################
library(splines)
################################################################################
# Recreate the GLM data setup
################################################################################
area_levels <- levels(as.factor(learn$Area))
vehpower_levels <- as.character(sort(unique(pmin(learn$VehPower, 9))))
region_levels <- levels(as.factor(learn$Region))
brand_levels <- levels(as.factor(learn$VehBrand))
gas_levels <- levels(as.factor(learn$VehGas))

# 1). Area
train$AreaGLM <- factor(match(train$Area, area_levels), levels = 1:length(area_levels))
validate$AreaGLM <- factor(match(validate$Area, area_levels), levels = 1:length(area_levels))

# 2). VehPower
train$VehPowerGLM <- factor(pmin(train$VehPower, 9), levels = vehpower_levels)
validate$VehPowerGLM <- factor(pmin(validate$VehPower, 9), levels = vehpower_levels)

# 3). VehAge
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

# 4). BonusMalus
train$BonusMalusCap <- pmin(train$BonusMalus, 150)
validate$BonusMalusCap <- pmin(validate$BonusMalus, 150)

train$BM_is50 <- ifelse(train$BonusMalusCap == 50, 1, 0)
validate$BM_is50 <- ifelse(validate$BonusMalusCap == 50, 1, 0)

train$BM_above50 <- pmax(train$BonusMalusCap - 50, 0)
validate$BM_above50 <- pmax(validate$BonusMalusCap - 50, 0)

train$BM_above100 <- pmax(train$BonusMalusCap - 100, 0)
validate$BM_above100 <- pmax(validate$BonusMalusCap - 100, 0)

# 5). Density
train$DensityGLM <- log(train$Density)
validate$DensityGLM <- log(validate$Density)

# 6). Region
train$Region <- factor(train$Region, levels = region_levels)
train$Region <- relevel(train$Region, ref = "R24")
validate$Region <- factor(validate$Region, levels = region_levels)
validate$Region <- relevel(validate$Region, ref = "R24")

# 7). VehBrand
train$VehBrand <- factor(train$VehBrand, levels = brand_levels)
train$VehBrand <- relevel(train$VehBrand, ref = "B1")
validate$VehBrand <- factor(validate$VehBrand, levels = brand_levels)
validate$VehBrand <- relevel(validate$VehBrand, ref = "B1")

# 8). VehGas
train$VehGas <- factor(train$VehGas, levels = gas_levels)
validate$VehGas <- factor(validate$VehGas, levels = gas_levels)

# 9). Offset
train$logExposure <- log(train$Exposure)
validate$logExposure <- log(validate$Exposure)

################################################################################
# Define the three DrivAge specifications to compare
################################################################################
# Paper bins
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, 150)
train$DrivAge_paper <- cut(train$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
train$DrivAge_paper <- relevel(train$DrivAge_paper, ref = "5")

validate$DrivAge_paper <- cut(validate$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
validate$DrivAge_paper <- relevel(validate$DrivAge_paper, ref = "5")

################################################################################
# Fit the three full GLMs
################################################################################
glm_da_paper <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    DrivAge_paper + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_da_ns4 <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 4) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_da_ns5 <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp +
    ns(DrivAge, df = 5) + BM_is50 + BM_above50 + BM_above100 +
    VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

################################################################################
# Compare using Poisson deviance
################################################################################
paper_train_pred <- predict(glm_da_paper, newdata = train, type = "response")
paper_val_pred <- predict(glm_da_paper, newdata = validate, type = "response")

ns4_train_pred <- predict(glm_da_ns4, newdata = train, type = "response")
ns4_val_pred <- predict(glm_da_ns4, newdata = validate, type = "response")

ns5_train_pred <- predict(glm_da_ns5, newdata = train, type = "response")
ns5_val_pred <- predict(glm_da_ns5, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Paper 7 classes",
    "Spline df = 4",
    "Spline df = 5"
  ),
  Parameters = c(length(coef(glm_da_paper)),
                 length(coef(glm_da_ns4)),
                 length(coef(glm_da_ns5))),
  AIC = c(AIC(glm_da_paper),
          AIC(glm_da_ns4),
          AIC(glm_da_ns5)),
  Train_Dev = c(Poisson.Deviance(paper_train_pred, train$ClaimNb),
                Poisson.Deviance(ns4_train_pred, train$ClaimNb),
                Poisson.Deviance(ns5_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(paper_val_pred, validate$ClaimNb),
              Poisson.Deviance(ns4_val_pred, validate$ClaimNb),
              Poisson.Deviance(ns5_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Paper <- comparison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Paper <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Paper <- comparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)

# Validation deviance is again main decision rule.
# Both spline specifications improve on the paper's 7-class DrivAge term.

# The best specification is spline df = 5.
# It gives the lowest AIC, training deviance, and validation deviance.

# The gain over df = 4 is small, but df = 5 still uses fewer parameters than
# the paper model, so keeping df = 5 for now.

### Another nice plot for the report. 
# Prediction grid from the fitted df = 5 spline model
# Fitting a univariate spline only for visualisation
drivage_spline5_uni <- glm(
  ClaimNb ~ ns(DrivAge, df = 5),
  family = poisson(link = log),
  data = learn,
  offset = log(Exposure)
)

# Smooth prediction grid
age_grid <- data.frame(
  DrivAge = seq(min(drivage_analysis$DrivAge),
                max(drivage_analysis$DrivAge),
                by = 0.1),
  Exposure = 1
)

age_grid$fit <- predict(drivage_spline5_uni, newdata = age_grid, type = "response")

plot(drivage_analysis$DrivAge, drivage_analysis$Frequency,
     type = "n",
     xlab = "Driver Age", ylab = "Frequency")

grid()

arrows(drivage_analysis$DrivAge, drivage_analysis$CI_lower,
       drivage_analysis$DrivAge, drivage_analysis$CI_upper,
       angle = 90, code = 3, length = 0.08, lwd = 1.2)

lines(drivage_analysis$DrivAge, drivage_analysis$Frequency, lwd = 1.8)
points(drivage_analysis$DrivAge, drivage_analysis$Frequency,
       pch = 19, cex = 1)

lines(age_grid$DrivAge, age_grid$fit, col = "red", lwd = 2.2)

h1 <- sum(learn$ClaimNb) / sum(learn$Exposure)
abline(h = h1, col = "#8d17f1", lty = 2, lwd = 2)

legend("topright",
       legend = c("Observed frequency", "Spline (df = 5)", "Overall frequency"),
       col = c("black", "red", "#8d17f1"),
       lty = c(1, 1, 2),
       lwd = c(1.8, 2.2, 2),
       pch = c(19, NA, NA),
       bty = "box",
       cex = 1.0)

