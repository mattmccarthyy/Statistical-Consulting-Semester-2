#########################################################################################
# EDA (1): VehAge
#########################################################################################
rm(list = ls())

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
# PART 1: EDA to decide which VehAge specifications are worth testing
################################################################################
################################################################################
# Data volume and credibility by integer VehAge
################################################################################
vehage_analysis <- data.frame(
  VehAge = 0:20,
  Policies = sapply(0:20, function(x) sum(learn$VehAge == x)),
  Exposure = sapply(0:20, function(x) sum(learn$Exposure[learn$VehAge == x])),
  Claims = sapply(0:20, function(x) sum(learn$ClaimNb[learn$VehAge == x]))
)

vehage_analysis$Frequency <- vehage_analysis$Claims / vehage_analysis$Exposure
vehage_analysis$Pct_Policies <- 100 * vehage_analysis$Policies / sum(vehage_analysis$Policies)
vehage_analysis$Pct_Exposure <- 100 * vehage_analysis$Exposure / sum(vehage_analysis$Exposure)

print(vehage_analysis)
# Age 0 stands out.
# Middle ages looking much flatter.


################################################################################
# Confidence intervals by integer VehAge
################################################################################
vehage_analysis$SE <- sqrt(vehage_analysis$Frequency / vehage_analysis$Exposure)
vehage_analysis$CI_lower <- pmax(0, vehage_analysis$Frequency - 1.96 * vehage_analysis$SE)
vehage_analysis$CI_upper <- vehage_analysis$Frequency + 1.96 * vehage_analysis$SE

print(vehage_analysis[, c("VehAge", "Frequency", "CI_lower", "CI_upper")])

# Age 0 is clearly distinct.
# The lower positive ages overlap heavily.


################################################################################
# Looking again at ages 0 to 12
################################################################################
middle_table <- vehage_analysis[vehage_analysis$VehAge <= 12,
                                c("VehAge", "Exposure", "Claims", "Frequency", "CI_lower", "CI_upper")]
print(middle_table)

# Ages 1-12 are broadly similar.
# No strong case for splitting 1-2 away from the rest.


################################################################################
# Looking at the older ages
################################################################################
older_table <- vehage_analysis[vehage_analysis$VehAge >= 13,
                               c("VehAge", "Exposure", "Claims", "Frequency", "CI_lower", "CI_upper")]
print(older_table)

# Ages 13-19 are lower than the middle band.
# The drop is stronger again in the capped tail.


################################################################################
# Pooled capped tail: 20+
################################################################################
tail_20plus <- data.frame(
  Policies_20plus = sum(learn$VehAge >= 20),
  Exposure_20plus = sum(learn$Exposure[learn$VehAge >= 20]),
  Claims_20plus   = sum(learn$ClaimNb[learn$VehAge >= 20]),
  Freq_20plus     = sum(learn$ClaimNb[learn$VehAge >= 20]) / sum(learn$Exposure[learn$VehAge >= 20])
)
print(tail_20plus)

# The capped 20+ tail is not tiny.
# It looks lower-risk than 13-19.


################################################################################
# Pooled comparison: 1-12 versus 13-19
################################################################################
tmp <- data.frame(
  group = c("1_12", "13_19"),
  exposure = c(sum(learn$Exposure[learn$VehAge >= 1 & learn$VehAge <= 12]),
               sum(learn$Exposure[learn$VehAge >= 13 & learn$VehAge <= 19])),
  claims = c(sum(learn$ClaimNb[learn$VehAge >= 1 & learn$VehAge <= 12]),
             sum(learn$ClaimNb[learn$VehAge >= 13 & learn$VehAge <= 19]))
)
tmp$frequency <- tmp$claims / tmp$exposure
print(tmp)

# This is the main split in the positive ages.
# 13-19 is clearly below 1-12.


################################################################################
# Four-group pooled comparison
################################################################################
vehage_groups <- data.frame(
  group = c("0", "1_12", "13_19", "20plus"),
  exposure = c(
    sum(learn$Exposure[learn$VehAge == 0]),
    sum(learn$Exposure[learn$VehAge >= 1  & learn$VehAge <= 12]),
    sum(learn$Exposure[learn$VehAge >= 13 & learn$VehAge <= 19]),
    sum(learn$Exposure[learn$VehAge >= 20])
  ),
  claims = c(
    sum(learn$ClaimNb[learn$VehAge == 0]),
    sum(learn$ClaimNb[learn$VehAge >= 1  & learn$VehAge <= 12]),
    sum(learn$ClaimNb[learn$VehAge >= 13 & learn$VehAge <= 19]),
    sum(learn$ClaimNb[learn$VehAge >= 20])
  )
)

vehage_groups$frequency <- vehage_groups$claims / vehage_groups$exposure
vehage_groups$se <- sqrt(vehage_groups$frequency / vehage_groups$exposure)
vehage_groups$CI_lower <- pmax(0, vehage_groups$frequency - 1.96 * vehage_groups$se)
vehage_groups$CI_upper <- vehage_groups$frequency + 1.96 * vehage_groups$se

print(vehage_groups)

# Clear ordering: 0, then 1-12, then 13-19, then 20+.
# This motivates only two new recodings.

# Plot for report. 
{
  par(mfrow = c(1, 1),
      xaxs = "r", yaxs = "i",
      mar = c(5.5, 5.5, 2, 2),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  x <- 1:nrow(vehage_groups)
  overall_freq <- sum(learn$ClaimNb) / sum(learn$Exposure)
  
  plot(x, vehage_groups$frequency,
       type = "n",
       xaxt = "n",
       xlab = "VehAge group",
       ylab = "Frequency",
       xlim = c(1, 4.25),
       ylim = c(0, max(vehage_groups$CI_upper) * 1.1))
  
  grid()
  
  abline(h = overall_freq, col = "red", lty = 2, lwd = 2)
  
  text(x = 3.65, y = overall_freq + 0.008,
       labels = "Overall frequency",
       col = "red", cex = 1.08)
  
  lines(x, vehage_groups$frequency, col = "black", lwd = 2)
  
  points(x, vehage_groups$frequency,
         pch = 19, cex = 1.5, col = "#8d17f1")
  
  arrows(x, vehage_groups$CI_lower,
         x, vehage_groups$CI_upper,
         angle = 90, code = 3, length = 0.05,
         col = "black", lwd = 1.8)
  
  axis(1, at = x, labels = c("[0,1)", "[1,13)", "[13,20)", "[20,\\infty)"))
}



################################################################################
# Decision from the EDA
################################################################################
# Keep the paper coding as the baseline.
# Test two new codings only:
# (i)  [0] / [1-12] / [13+]
# (ii) [0] / [1-12] / [13-19] / [20+]


################################################################################
# PART 2: Compare the three VehAge codings inside the full GLM
################################################################################

################################################################################
# Recreate the GLM1 data setup from the paper
################################################################################

# Fix factor levels using the full learning data
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

# 3). DrivAge
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, Inf)
train$DrivAgeGLM <- cut(train$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
train$DrivAgeGLM <- relevel(train$DrivAgeGLM, ref = "5")
validate$DrivAgeGLM <- cut(validate$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
validate$DrivAgeGLM <- relevel(validate$DrivAgeGLM, ref = "5")

# 4). BonusMalus
train$BonusMalusGLM <- pmin(train$BonusMalus, 150)
validate$BonusMalusGLM <- pmin(validate$BonusMalus, 150)

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

# 10). Cap VehAge at 20, as used in the models
train$VehAgeCap <- pmin(train$VehAge, 20)
validate$VehAgeCap <- pmin(validate$VehAge, 20)


################################################################################
# Define the three VehAge codings to compare
################################################################################

# Paper coding: [0], [1-10], [11+]
train$VehAge_Paper <- cut(train$VehAgeCap,
                          breaks = c(-0.5, 0.5, 10.5, 1000),
                          labels = c("0", "1_10", "11plus"),
                          right = FALSE)
train$VehAge_Paper <- relevel(train$VehAge_Paper, ref = "1_10")

validate$VehAge_Paper <- cut(validate$VehAgeCap,
                             breaks = c(-0.5, 0.5, 10.5, 1000),
                             labels = c("0", "1_10", "11plus"),
                             right = FALSE)
validate$VehAge_Paper <- relevel(validate$VehAge_Paper, ref = "1_10")

# New coding 1: [0], [1-12], [13+]
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

# New coding 2: [0], [1-12], [13-19], [20+]
train$VehAge_4grp <- cut(train$VehAgeCap,
                         breaks = c(-0.5, 0.5, 12.5, 19.5, 1000),
                         labels = c("0", "1_12", "13_19", "20plus"),
                         right = FALSE)
train$VehAge_4grp <- relevel(train$VehAge_4grp, ref = "1_12")

validate$VehAge_4grp <- cut(validate$VehAgeCap,
                            breaks = c(-0.5, 0.5, 12.5, 19.5, 1000),
                            labels = c("0", "1_12", "13_19", "20plus"),
                            right = FALSE)
validate$VehAge_4grp <- relevel(validate$VehAge_4grp, ref = "1_12")


################################################################################
# Fit the three full GLMs
################################################################################
glm_paper <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_Paper + DrivAgeGLM +
    BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_3grp <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp + DrivAgeGLM +
    BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_4grp <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_4grp + DrivAgeGLM +
    BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)


################################################################################
# Compare using Poisson deviance
################################################################################
paper_train_pred <- predict(glm_paper, newdata = train, type = "response")
paper_val_pred <- predict(glm_paper, newdata = validate, type = "response")

grp3_train_pred <- predict(glm_3grp, newdata = train, type = "response")
grp3_val_pred <- predict(glm_3grp, newdata = validate, type = "response")

grp4_train_pred <- predict(glm_4grp, newdata = train, type = "response")
grp4_val_pred <- predict(glm_4grp, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Paper [0],[1-10],[11+]",
    "New [0],[1-12],[13+]",
    "New [0],[1-12],[13-19],[20+]"
  ),
  Parameters = c(length(coef(glm_paper)),
                 length(coef(glm_3grp)),
                 length(coef(glm_4grp))),
  AIC = c(AIC(glm_paper),
          AIC(glm_3grp),
          AIC(glm_4grp)),
  Train_Dev = c(Poisson.Deviance(paper_train_pred, train$ClaimNb),
                Poisson.Deviance(grp3_train_pred, train$ClaimNb),
                Poisson.Deviance(grp4_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(paper_val_pred, validate$ClaimNb),
              Poisson.Deviance(grp3_val_pred, validate$ClaimNb),
              Poisson.Deviance(grp4_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Paper <- comparison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Paper <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Paper <- comparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)
# Replacing the paper's VehAge coding with [0], [1-12], [13+] improves both
# training and validation deviance. A more detailed split of the upper
# tail, [0], [1-12], [13-19], [20+], lowers AIC further but not improving
# validation deviance beyond simpler 3-group alternative.
# Deciding to keep [0], [1-12], [13+] as the preferred VehAge specification.