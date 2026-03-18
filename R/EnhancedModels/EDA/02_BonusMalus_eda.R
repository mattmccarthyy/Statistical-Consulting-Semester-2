################################################################################
# EDA: BonusMalus
################################################################################
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
# PART 1: EDA to decide which BonusMalus specifications are worth testing
################################################################################

################################################################################
# 1). Basic distribution
################################################################################
summary(learn$BonusMalus)
range(learn$BonusMalus)
length(unique(learn$BonusMalus))
table(learn$BonusMalus)

# Big spike at 50.
# Start by checking whether 50 behaves differently from the rest.


################################################################################
# 2). Capped version actually used in the GLM
################################################################################
summary(pmin(learn$BonusMalus, 150))
table(pmin(learn$BonusMalus, 150))

# Baseline GLM caps at 150.
# Keep this in mind when choosing candidate specifications.


################################################################################
# 3). Exact-value frequency table
################################################################################
bm_tab <- aggregate(
  cbind(Exposure, ClaimNb) ~ BonusMalus,
  data = learn,
  FUN = sum
)

bm_tab$Frequency <- bm_tab$ClaimNb / bm_tab$Exposure
bm_tab$SE <- sqrt(bm_tab$Frequency / bm_tab$Exposure)
bm_tab$CI_lower <- pmax(0, bm_tab$Frequency - 1.96 * bm_tab$SE)
bm_tab$CI_upper <- bm_tab$Frequency + 1.96 * bm_tab$SE

bm_tab[bm_tab$BonusMalus <= 150 & bm_tab$Exposure >= 500,
       c("BonusMalus", "Exposure", "ClaimNb", "Frequency", "CI_lower", "CI_upper")]

# Above 50, frequency rises overall.
# But the exact-value pattern is too jagged for many hard cutpoints.


################################################################################
# 4). Compare 50 against everything above 50
################################################################################
bm_50_vs_rest <- data.frame(
  group = c("50", "51plus"),
  exposure = c(
    sum(learn$Exposure[learn$BonusMalus == 50]),
    sum(learn$Exposure[learn$BonusMalus > 50])
  ),
  claims = c(
    sum(learn$ClaimNb[learn$BonusMalus == 50]),
    sum(learn$ClaimNb[learn$BonusMalus > 50])
  )
)

bm_50_vs_rest$frequency <- bm_50_vs_rest$claims / bm_50_vs_rest$exposure
bm_50_vs_rest

# 50 is clearly lower-risk than the rest.
# So 50 should be treated separately.


################################################################################
# 5). Pooled comparison with a breakpoint at 100
################################################################################
bm_pool_100 <- data.frame(
  group = c("50", "51_99", "100_150"),
  exposure = c(
    sum(learn$Exposure[learn$BonusMalus == 50]),
    sum(learn$Exposure[learn$BonusMalus >= 51 & learn$BonusMalus <= 99]),
    sum(learn$Exposure[learn$BonusMalus >= 100 & learn$BonusMalus <= 150])
  ),
  claims = c(
    sum(learn$ClaimNb[learn$BonusMalus == 50]),
    sum(learn$ClaimNb[learn$BonusMalus >= 51 & learn$BonusMalus <= 99]),
    sum(learn$ClaimNb[learn$BonusMalus >= 100 & learn$BonusMalus <= 150])
  )
)

bm_pool_100$frequency <- bm_pool_100$claims / bm_pool_100$exposure
bm_pool_100

# There is a second clear rise from about 100 onward.
# So 100 is the preferred hinge candidate.


################################################################################
# EDA conclusion
################################################################################
# Main points:
# 1). BonusMalus = 50 is a genuine mass point.
# 2). Above 50, risk rises overall.
# 3). There is a second clear rise around 100.
# 4). Many grouped-factor cutpoints are not well supported.

# So the only candidate specifications worth testing next are:
# (i)   paper spec: capped continuous BonusMalus
# (ii)  I(BonusMalus == 50) + linear effect above 50
# (iii) I(BonusMalus == 50) + piecewise linear effect with a hinge at 100


################################################################################
# PART 2: Compare the three BonusMalus specifications inside the full GLM
################################################################################

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
# Use the preferred VehAge specification from the previous section.
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

# 4). DrivAge
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, Inf)
train$DrivAgeGLM <- cut(train$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
train$DrivAgeGLM <- relevel(train$DrivAgeGLM, ref = "5")
validate$DrivAgeGLM <- cut(validate$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
validate$DrivAgeGLM <- relevel(validate$DrivAgeGLM, ref = "5")

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
# Define the three BonusMalus specifications to compare
################################################################################

# Cap at 150, as in the paper.
train$BonusMalusCap <- pmin(train$BonusMalus, 150)
validate$BonusMalusCap <- pmin(validate$BonusMalus, 150)

# 1). Paper specification: single capped continuous term
train$BM_paper <- train$BonusMalusCap
validate$BM_paper <- validate$BonusMalusCap

# 2). Mass point at 50 + linear effect above 50
train$BM_is50 <- ifelse(train$BonusMalusCap == 50, 1, 0)
validate$BM_is50 <- ifelse(validate$BonusMalusCap == 50, 1, 0)

train$BM_above50 <- pmax(train$BonusMalusCap - 50, 0)
validate$BM_above50 <- pmax(validate$BonusMalusCap - 50, 0)

# 3). Mass point at 50 + hinge at 100
train$BM_above100 <- pmax(train$BonusMalusCap - 100, 0)
validate$BM_above100 <- pmax(validate$BonusMalusCap - 100, 0)


################################################################################
# Fit the three full GLMs
################################################################################
glm_bm_paper <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp + DrivAgeGLM +
    BM_paper + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_bm_mass_linear <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp + DrivAgeGLM +
    BM_is50 + BM_above50 + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

glm_bm_mass_hinge <- glm(
  ClaimNb ~ AreaGLM + VehPowerGLM + VehAge_3grp + DrivAgeGLM +
    BM_is50 + BM_above50 + BM_above100 + VehBrand + VehGas + DensityGLM + Region,
  family = poisson(link = log),
  data = train,
  offset = logExposure
)

################################################################################
# Compare using Poisson deviance
################################################################################
paper_train_pred <- predict(glm_bm_paper, newdata = train, type = "response")
paper_val_pred <- predict(glm_bm_paper, newdata = validate, type = "response")

linear_train_pred <- predict(glm_bm_mass_linear, newdata = train, type = "response")
linear_val_pred <- predict(glm_bm_mass_linear, newdata = validate, type = "response")

hinge_train_pred <- predict(glm_bm_mass_hinge, newdata = train, type = "response")
hinge_val_pred <- predict(glm_bm_mass_hinge, newdata = validate, type = "response")

comparison <- data.frame(
  Specification = c(
    "Paper continuous",
    "Mass point at 50 + linear above 50",
    "Mass point at 50 + hinge at 100"
  ),
  Parameters = c(length(coef(glm_bm_paper)),
                 length(coef(glm_bm_mass_linear)),
                 length(coef(glm_bm_mass_hinge))),
  AIC = c(AIC(glm_bm_paper),
          AIC(glm_bm_mass_linear),
          AIC(glm_bm_mass_hinge)),
  Train_Dev = c(Poisson.Deviance(paper_train_pred, train$ClaimNb),
                Poisson.Deviance(linear_train_pred, train$ClaimNb),
                Poisson.Deviance(hinge_train_pred, train$ClaimNb)),
  Val_Dev = c(Poisson.Deviance(paper_val_pred, validate$ClaimNb),
              Poisson.Deviance(linear_val_pred, validate$ClaimNb),
              Poisson.Deviance(hinge_val_pred, validate$ClaimNb))
)

comparison$Delta_AIC_vs_Paper <- comparison$AIC - comparison$AIC[1]
comparison$Delta_Train_Dev_vs_Paper <- comparison$Train_Dev - comparison$Train_Dev[1]
comparison$Delta_Val_Dev_vs_Paper <- comparison$Val_Dev - comparison$Val_Dev[1]

print(comparison)
# Validation deviance is the main decision rule.
# Here the hinge specification is best, so we keep it.



# Plot for the report:
bm_plot <- bm_tab[bm_tab$BonusMalus <= 150 & bm_tab$Exposure >= 500,
                  c("BonusMalus", "Exposure", "ClaimNb", "Frequency", "CI_lower", "CI_upper")]
{
  par(mfrow = c(1, 1),
      xaxs = "r", yaxs = "i",
      mar = c(5.5, 5.5, 2, 2),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  overall_freq <- sum(learn$ClaimNb) / sum(learn$Exposure)
  
  plot(bm_plot$BonusMalus, bm_plot$Frequency,
       type = "n",
       xlab = "BonusMalus",
       ylab = "Frequency",
       xlim = c(min(bm_plot$BonusMalus), max(bm_plot$BonusMalus) + 2),
       ylim = c(0, max(bm_plot$CI_upper) * 1.05))
  
  # Put grid behind everything else
  grid()
  
  # Reference lines for key values
  abline(v = 50, col = "red", lty = 2, lwd = 2.2)
  abline(v = 100, col = "red", lty = 2, lwd = 2.2)
  abline(h = overall_freq, col = "grey40", lty = 2, lwd = 2)
  
  # Confidence intervals
  arrows(bm_plot$BonusMalus, bm_plot$CI_lower,
         bm_plot$BonusMalus, bm_plot$CI_upper,
         angle = 90, code = 3, length = 0.04,
         col = "black", lwd = 1.9)
  
  # Line and points
  lines(bm_plot$BonusMalus, bm_plot$Frequency, col = "black", lwd = 2.5)
  points(bm_plot$BonusMalus, bm_plot$Frequency, pch = 19, cex = 1.4, col = "#8d17f1")
  
  # Labels for reference lines
  text(50, max(bm_plot$CI_upper) * 1.01, labels = "50", col = "red", cex = 1.2, pos = 4)
  text(100, max(bm_plot$CI_upper) * 1.01, labels = "100", col = "red", cex = 1.2, pos = 4)
  text(max(bm_plot$BonusMalus) - 8, overall_freq + 0.012,
       labels = "Overall frequency", col = "grey40", cex = 1.2)
}



