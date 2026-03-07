################################################################################
# EDA: BonusMalus
################################################################################
rm(list = ls())

# Load EDA data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load required package for GLM spec testing
library(splines)

# Metric once more
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}

# Required data for GLM Spec testing
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

################################################################################
# 1). Distribution and Range
################################################################################
#BonusMalus Summary Statistics
print(summary(learn$BonusMalus))

#BonusMalus Range
print(paste("Min:", min(learn$BonusMalus), "Max:", max(learn$BonusMalus)))

# Count unique values
print(paste("Unique BonusMalus values:", length(unique(learn$BonusMalus))))

# More than 50% are at 50 exactly, so we can't just bin as normal.

# Check concentration at baseline
#Policies at BonusMalus = 50
bm50_pct <- 100 * sum(learn$BonusMalus == 50) / nrow(learn)
print(paste(round(bm50_pct, 1), "% of policies"))

# Frequency at BonusMalus = 50 specifically
bm50_freq <- sum(learn$ClaimNb[learn$BonusMalus == 50]) / sum(learn$Exposure[learn$BonusMalus == 50])
print(paste("Frequency at BM=50:", round(bm50_freq, 4)))

# Frequency for BonusMalus > 50
bmhigh_freq <- sum(learn$ClaimNb[learn$BonusMalus > 50]) / sum(learn$Exposure[learn$BonusMalus > 50])
print(paste("Frequency at BM>50:", round(bmhigh_freq, 4)))

# CONCLUSION:
## BonusMalus = 50 clearly needs to be its own group. 
## Baseline frequency (at BM = 50) is 7.94%.
## Freq for BM > 50 is 13.55%. (71% higher).

################################################################################
# 2). Looking at policies where BM > 50 specifically, need to handle these separately
################################################################################
# Analyze BonusMalus > 50 only
bm_high <- learn[learn$BonusMalus > 50, ]

# Bin every 20 points from 50 to 230
bm_bins <- seq(50, 230, by = 20)

bm_high_analysis <- data.frame(
  BM_Range = paste(bm_bins[-length(bm_bins)], bm_bins[-1], sep="-"),
  Midpoint = (bm_bins[-length(bm_bins)] + bm_bins[-1]) / 2,
  Policies = NA,
  Exposure = NA,
  Claims = NA,
  Frequency = NA
)

for(i in 1:(length(bm_bins)-1)) {
  mask <- bm_high$BonusMalus >= bm_bins[i] & bm_high$BonusMalus < bm_bins[i+1]
  
  bm_high_analysis$Policies[i] <- sum(mask)
  bm_high_analysis$Exposure[i] <- sum(bm_high$Exposure[mask])
  bm_high_analysis$Claims[i] <- sum(bm_high$ClaimNb[mask])
  bm_high_analysis$Frequency[i] <- bm_high_analysis$Claims[i] / bm_high_analysis$Exposure[i]
}

## Frequency for BonusMalus > 50
print(bm_high_analysis)

# COMMENTS: 
# BonusMalus = 50 (56.7% of policies, freq 7.94%) must be separate group - clear baseline effect
# Non-linear relationship: frequency jumps at 90 (19%) and 110 (39%) - not continuous
# Good credibility up to 110 (258k policies), poorer credibility above 110 (5k policies)
# Natural breaks suggest grouping: will try [50], [50-90], [90-110], [110+] or simpler [50], [50-110], [110+]
# Will test factor specifications vs spline, expect factors to win given clear step changes, similiar to VehAge



################################################################################
# PART 2: GLM Specification Testing for BonusMalus
################################################################################
##########################################################
# Specification 1: Paper - BonusMalus continuous
##########################################################
spec1 <- glm(ClaimNb ~ BonusMalus, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))

##########################################################
# Specification 2: [50], [51-110], [111+]
##########################################################
train$BM_Simple <- cut(train$BonusMalus, breaks = c(49, 50, 110, 1000), labels = c("50", "51-110", "111+"))
validate$BM_Simple <- cut(validate$BonusMalus, breaks = c(49, 50, 110, 1000), labels = c("50", "51-110", "111+"))

spec2 <- glm(ClaimNb ~ BM_Simple, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))

##########################################################
# Specification 3: [50], [51-90], [91-110], [111+]
##########################################################
train$BM_Granular <- cut(train$BonusMalus, breaks = c(49, 50, 90, 110, 1000), labels = c("50", "51-90", "91-110", "111+"))
validate$BM_Granular <- cut(validate$BonusMalus, breaks = c(49, 50, 90, 110, 1000), labels = c("50", "51-90", "91-110", "111+"))

spec3 <- glm(ClaimNb ~ BM_Granular, family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))

##########################################################
# Specification 4: Spline - Testing DF 2-8
##########################################################
# df_values <- 2:8
# 
# # Create data frame to save results and compare. 
# spline_results <- data.frame(DF = df_values, 
#                              AIC = NA, 
#                              Params = NA)
# 
# for(i in 1:length(df_values)) {
#   spec_temp <- glm(ClaimNb ~ ns(BonusMalus, df = df_values[i]), family = poisson(), data = train, offset = log(Exposure))
#   spline_results$AIC[i] <- AIC(spec_temp)
#   spline_results$Params[i] <- length(coef(spec_temp))
# }
# 
# print(spline_results)


## Too much data at 50, multiple knots get placed there.
## Spline unsuitable for this data.


################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: Continuous", "[50],[51-110],[111+]", "[50],[51-90],[91-110],[111+]"),
  Params = c(spec1_params, spec2_params, spec3_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic

print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])


# BonusMalus Conclusion:
# Paper's continuous specification wins (Val_Dev: 32.356 vs 32.390 for granular grouping)
# Despite clear non-linearity in EDA, linear on log scale captures relationship adequately
# Granular grouping [50],[51-90],[91-110],[111+] has better AIC (-7.7) but worse validation performance
# Difference is marginal (0.034 deviance), suggests overfitting with more parameters
# Current decision: Use paper's continuous BonusMalus specification - simpler, validates better
# This is one predictor where our enhanced GLM does NOT improve on the paper

# So paper spec wins on both validation and simplicity. 
# Spec3 only wins AIC.
# Diffierence is only about 0.1%.
# Still deciding to bootstrap test for consistency.


################################################################################
# Bootstrap: Paper vs Spec3
################################################################################
set.seed(100)
n_bootstrap <- 1000

# Data frame for results
bootstrap_results <- data.frame(Paper = numeric(n_bootstrap), 
                                Spec3 = numeric(n_bootstrap))

# Bootstrap on validate once more
for(b in 1:n_bootstrap) {
  boot_idx <- sample(1:nrow(validate), nrow(validate), replace = TRUE)
  boot_data <- validate[boot_idx, ]
  
  bootstrap_results$Paper[b] <- Poisson.Deviance(predict(spec1, newdata = boot_data, type = "response"), boot_data$ClaimNb)
  bootstrap_results$Spec3[b] <- Poisson.Deviance(predict(spec3, newdata = boot_data, type = "response"), boot_data$ClaimNb)
}

#Paper mean
mean(bootstrap_results$Paper
     
#Spec3 mean 
mean(bootstrap_results$Spec3)))

# Difference
mean(bootstrap_results$Paper) - mean(bootstrap_results$Spec3)))

# Wilson cox test 
wilcox_test <- wilcox.test(bootstrap_results$Paper, bootstrap_results$Spec3, paired = TRUE)
wilcox_test
# Locations differ

## Continuous specification from paper is indeed better. 

