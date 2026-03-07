################################################################################
# EDA (4): Area
################################################################################
rm(list = ls())

# Load in required eda data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load in required training and validation data for GLM specs
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# PART 1: EDA to decide on GLM Specifications to Test
################################################################################
################################################################################
# 1). Frequency by Area
################################################################################
area_analysis <- data.frame(
  Area = sort(unique(learn$Area)),
  Policies = sapply(sort(unique(learn$Area)), function(x) sum(learn$Area == x)),
  Exposure = sapply(sort(unique(learn$Area)), function(x) sum(learn$Exposure[learn$Area == x])),
  Claims = sapply(sort(unique(learn$Area)), function(x) sum(learn$ClaimNb[learn$Area == x]))
)

area_analysis$Frequency <- area_analysis$Claims / area_analysis$Exposure
area_analysis$Pct_Policies <- 100 * area_analysis$Policies / sum(area_analysis$Policies)

# Frequency by Area
print(area_analysis)



################################################################################
# 2). Plot Frequency by Area
################################################################################
par(mfrow = c(1, 1), 
    mar = c(5.5, 5.5, 3, 1), 
    tcl = -0.25,
    cex.main = 1.5, 
    cex.lab = 1.3, 
    cex.axis = 1.2, 
    col = "black", 
    mgp = c(3.5, 0.7, 0))

barplot(area_analysis$Frequency, 
        names.arg = area_analysis$Area, 
        col = "#8d17f1",
        xlab = "Area", 
        ylab = "Frequency", 
        main = "Frequency by Area", 
        ylim = c(0, 0.15))

abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)

# Area shows clear monotonic increase from A (8.14%) to F (13.75%)
# Nearly 70% frequency increase across the range
# Pattern suggests ordinal relationship: A = rural/low density, F = urban/high density
# Linear progression (~1-2 percentage point increases per level)
# Volume is good across A-E (11-28% each), but F only has 2.6% of policies
# Unlike VehBrand (B12 outlier) or BonusMalus (concentration at 50), Area shows smooth gradient
# This ordinal structure suggests continuous specification might perform well




################################################################################
# PART 2: GLM Specification Testing for Area
################################################################################
str(train$Area)
# Convert Area to numeric for continuous specs
train$Area_Num <- as.numeric(factor(train$Area, levels = c("A","B","C","D","E","F")))
validate$Area_Num <- as.numeric(factor(validate$Area, levels = c("A","B","C","D","E","F")))
str(train$Area_Num)



##########################################################
# Specification 1: Paper - All 6 categorical levels
##########################################################
# Paper's baseline: Area as categorical factor with all 6 levels.
spec1 <- glm(ClaimNb ~ Area, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))



##########################################################
# Specification 2: Area as continuous (linear)
##########################################################
# EDA showed monotonic linear pattern (8.14% -> 13.75%), test if continuous captures this.
# Would reduce from 6 params to 2 if linear relationship holds.
spec2 <- glm(ClaimNb ~ Area_Num, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))



##########################################################
# Specification 3: Density Tiers [A-B], [C-D], [E-F]
##########################################################
# While pattern is linear, pairing consecutive levels tests if gradient can be simplified.
# Low (A-B: 8.1-8.8%), Medium (C-D: 9.4-10.8%), High (E-F: 12.2-13.8%) represent risk tiers.
train$Area_Tiers <- ifelse(train$Area %in% c("A","B"), "Low",
                           ifelse(train$Area %in% c("C","D"), "Medium", "High"))
validate$Area_Tiers <- ifelse(validate$Area %in% c("A","B"), "Low",
                              ifelse(validate$Area %in% c("C","D"), "Medium", "High"))

spec3 <- glm(ClaimNb ~ Area_Tiers, family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))



##########################################################
# Specification 4: Simple split [A-C], [D-F]
##########################################################
# Clear break around middle: A-C average 8.8%, D-F average 12.2%.
# Tests if simple binary split (lower vs higher density) captures most of signal.
train$Area_Simple <- ifelse(train$Area %in% c("A","B","C"), "Lower", "Higher")
validate$Area_Simple <- ifelse(validate$Area %in% c("A","B","C"), "Lower", "Higher")

spec4 <- glm(ClaimNb ~ Area_Simple, family = poisson(), data = train, offset = log(Exposure))
spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(fitted(spec4), train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(predict(spec4, newdata = validate, type = "response"), validate$ClaimNb)
spec4_params <- length(coef(spec4))



################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: All 6 categorical", "Continuous (linear)", "Tiers [A-B],[C-D],[E-F]", "Simple [A-C],[D-F]"),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])

# Paper's 6 categorical levels wins. All simplifications performed worse.
# Continuous is very close (+3.4 AIC, +0.004 deviance) but still loses. Could bootstrap but not a meaningful increase in fit regardless. Skipping this. 
# Groupings fail badly because gradient accelerates: A-B (+0.70pp), B-C (+0.57pp), then C-D (+1.43pp), D-E (+1.34pp), E-F (+1.57pp).
# Early levels have small increments, later levels have larger jumps, groupings lost this non-linearity.

# From above, deciding to test quadratic specification:
# Acceleration pattern (small jumps A-C, large jumps C-F) suggests quadratic relationship.
# Area + Area^2 could capture curvature while reducing from 6 params to 3.
# If quadratic performs well, validates that Area has non-linear but smooth progression once more.



##########################################################
# Specification 5: Quadratic (Area + Area^2)
##########################################################
spec5 <- glm(ClaimNb ~ Area_Num + I(Area_Num^2), family = poisson(), data = train, offset = log(Exposure))
spec5_aic <- AIC(spec5)
spec5_train_dev <- Poisson.Deviance(fitted(spec5), train$ClaimNb)
spec5_val_dev <- Poisson.Deviance(predict(spec5, newdata = validate, type = "response"), validate$ClaimNb)
spec5_params <- length(coef(spec5))

# Compare quadratic vs paper
comparison_quad <- data.frame(
  Specification = c("Paper: All 6 categorical", "Quadratic (Area + Area^2)"),
  Params = c(spec1_params, spec5_params),
  AIC = c(spec1_aic, spec5_aic),
  Train_Dev = c(spec1_train_dev, spec5_train_dev),
  Val_Dev = c(spec1_val_dev, spec5_val_dev)
)

comparison_quad$Delta_AIC <- comparison_quad$AIC - spec1_aic
print(comparison_quad)

# Quadratic performs nearly identically to paper: better AIC (-1.45) but slightly worse validation (+0.00019).
# Difference is negligible - essentially tied on validation performance.
# Quadratic uses 3 params vs 6, captures acceleration pattern with parsimony.
# Bootstrap needed to determine if validation difference is significant or noise.

################################################################################
# Bootstrap: Paper vs Quadratic
################################################################################
set.seed(100)
n_bootstrap <- 1000

# data frame to store results
bootstrap_results <- data.frame(Paper = numeric(n_bootstrap), Quadratic = numeric(n_bootstrap))

for(b in 1:n_bootstrap) {
  boot_idx <- sample(1:nrow(validate), nrow(validate), replace = TRUE)
  boot_data <- validate[boot_idx, ]
  
  bootstrap_results$Paper[b] <- Poisson.Deviance(predict(spec1, newdata = boot_data, type = "response"), boot_data$ClaimNb)
  bootstrap_results$Quadratic[b] <- Poisson.Deviance(predict(spec5, newdata = boot_data, type = "response"), boot_data$ClaimNb)
}

# Paper mean
mean(bootstrap_results$Paper)

# Quadratic mean
mean(bootstrap_results$Quadratic)

# Difference
mean(bootstrap_results$Paper) - mean(bootstrap_results$Quadratic)

# Wilcoxon test
wilcox_test <- wilcox.test(bootstrap_results$Paper, bootstrap_results$Quadratic, paired = TRUE)
print(wilcox_test)
# Paper's 6 categorical levels wins???
# God these guys simple GLM is fair hard to beat. 
# Paper's spec is reliably better.

# Paper wins statistically (p < 0.001) but difference is tiny (0.000289 deviance).
# Quadratic has better AIC (-1.45) and uses 3 params vs 6, but paper consistently validates better.
# With only 6 levels, categorical parameterisation is not excessive.
# Already simplified VehBrand (11->3), don't need to simplify everything.
# FOR NOW deciding to use paper's 6 categorical Area levels, statistically justified, not over-parameterised.
# May return here later and look at the other factor specs, ONLY if interactions are leading to way too many terms with 6 levels here. 


################################################################################
# Prt 3: Saving Most Important Plot for Report
################################################################################
# Have to reset graphics device before par.
# My usual par causes issues due to xaxs and yaxs args. 
png("figs/Area_figs/01_frequency_by_area.png", width = 800, height = 600)

par(mfrow = c(1, 1), 
    mar = c(5.5, 5.5, 3, 1), 
    tcl = -0.25,
    cex.main = 1.5, 
    cex.lab = 1.3, 
    cex.axis = 1.2, 
    col = "black", 
    mgp = c(3.5, 0.7, 0))

barplot(area_analysis$Frequency, names.arg = area_analysis$Area, col = "darkgrey",
        xlab = "Area", ylab = "Frequency", main = "Frequency by Area",
        ylim = c(0, 0.15))

abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "#8d17f1", lty = 2, lwd = 2)

dev.off()  # Close PNG device AFTER plotting


