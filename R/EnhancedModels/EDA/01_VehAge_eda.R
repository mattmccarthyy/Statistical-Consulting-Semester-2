#########################################################################################
# EDA (1): VehAge
#########################################################################################
rm(list = ls())

# Only 1 library needed here.
library(splines)



################################################################################
# PART 1: EDA to decide on GLM Specifications to Test
################################################################################
# Load data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")
# Doing EDA on full "learn", to try come up with ideas of how to split into factor, if we should use a spline etc
# Test the hypothesis by fitting on "train", which is just 80% of learn.
# Then test this fit on "validate". 



################################################################################
# Define one helper function for comparison (in GLM section)
################################################################################
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# 1). Data Volume and Credibility
################################################################################
# Capping VehAge at 20 as in the report. 
vehage_analysis <- data.frame(
  VehAge = 0:20,
  Policies = sapply(0:20, function(x) sum(learn$VehAge == x)),
  Exposure = sapply(0:20, function(x) sum(learn$Exposure[learn$VehAge == x])),
  Claims = sapply(0:20, function(x) sum(learn$ClaimNb[learn$VehAge == x]))
)

vehage_analysis$Frequency <- vehage_analysis$Claims / vehage_analysis$Exposure
vehage_analysis$Pct_Policies <- 100 * vehage_analysis$Policies / sum(vehage_analysis$Policies)
vehage_analysis$Pct_Exposure <- 100 * vehage_analysis$Exposure / sum(vehage_analysis$Exposure)

print("Volume and Frequency by VehAge:")
print(vehage_analysis)
# VehAge - vehicle age in year (0 - 20)
# Policies - npo. of individual policies at that age
# Exposure - total years-at-risk for all policies at that age
# Claims - total no. of claims from all policies at that age
# Frequency = Claims / Exposure = claims per year-at-risk
# Pct_Policies - what % of our portfolio is this age
# Pct_Exposure - what % of total exposure is this age



################################################################################
# 2). Checking statistical significance 
################################################################################
# Under Poisson assumption: SE = sqrt(frequency / exposure)
# 95% CI = frequency +/- 1.96 * SE

vehage_analysis$SE <- sqrt(vehage_analysis$Frequency / vehage_analysis$Exposure)
vehage_analysis$CI_lower <- pmax(0, vehage_analysis$Frequency - 1.96 * vehage_analysis$SE)
vehage_analysis$CI_upper <- vehage_analysis$Frequency + 1.96 * vehage_analysis$SE
vehage_analysis$CI_width <- vehage_analysis$CI_upper - vehage_analysis$CI_lower

print("Confidence Intervals:")
print(vehage_analysis[, c("VehAge", "Frequency", "CI_lower", "CI_upper", "CI_width")])



################################################################################
# 3). Frequency with Confidence Intervals
################################################################################
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", 
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25, 
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

grid()

plot(vehage_analysis$VehAge, vehage_analysis$Frequency,
     type = "p", pch = 19, 
     ylim = c(0, max(vehage_analysis$CI_upper) * 1.1),
     xlab = "Vehicle Age", ylab = "Frequency",
     main = "Frequency by VehAge with 95% Confidence Intervals",
     col = "#8d17f1",
     cex = 1.1)

# Add confidence interval bars
arrows(vehage_analysis$VehAge, vehage_analysis$CI_lower,
       vehage_analysis$VehAge, vehage_analysis$CI_upper,
       angle = 90, code = 3, length = 0.05, col = "green")

# Add overall frequency reference
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)

# Connect points
lines(vehage_analysis$VehAge, vehage_analysis$Frequency, col = "black", lwd = 1.6)

# VehAge = 0 is statistically distinct. (Freq of 31% vs. overall of 10%.)
# CI doesn't overlap with any other age. 
# Only 2.5% of policies, but 11,501 exposure years, gives tight CI
# Conclude these must be modelled as their own group. 

# CI's for ages 1 and 2 overlap, could merge these two
# Veh Age frequencies to discuss (age, freq):
## 0: 31%
## 1: 8.9%
## 2: 9.3%

# Ages 1-14 relatively stable, all around 9-10% freq.
# CI's almost all overlap

# Ages 15-20 show declining trend, drops from ~9% to ~6%# However, CI's wider, less data, less credible.
# Ages 18-20 only have <5,000 policies each, enough for GLM coefs at least.

# Ages 0-14 very credible
# 15-17 could be classed as borderline
# 18-20 is more questionable, considering capping further? Have to do more EDA

# So for GLM specification:
# Spec 1 will be the papers. Using this as a baseline to compare to (and try to beat).
# Spec 2 will be VehAge = 0 and rest is continuous. This is just testing it 0 is distinct, and rest could be classed as "linear decline". Seems plausible but doubt this will really work best for such a strong predictor.
# Spec 3 will be a simple split [0], [1-14] and [15+], this was originally best idea, but thinking [2-3] should also be it's own group due to the drop.
# Spec 4 will be [0], [1-2], [3-14], [15+], likely best based on the plot above.
# Spec 5 will be testing splines, will keep the best one and compare that. 



################################################################################
# PART 2: GLM Specification Testing, trying 5 models. 
################################################################################
##########################################################
# Specification 1: Paper's grouping [0,1), [1,10], (10, \infty)
##########################################################
train$VehAge_Paper <- cut(train$VehAge,
                          breaks = c(-0.5, 0.5, 10.5, 1000),
                          labels = c("0", "1-10", "11+"),
                          right = FALSE)
validate$VehAge_Paper <- cut(validate$VehAge,
                             breaks = c(-0.5, 0.5, 10.5, 1000),
                             labels = c("0", "1-10", "11+"),
                             right = FALSE)

spec1 <- glm(ClaimNb ~ VehAge_Paper,
             family = poisson(),
             data = train,
             offset = log(Exposure))

spec1_train_pred <- fitted(spec1)
spec1_val_pred <- predict(spec1, newdata = validate, type = "response")

spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(spec1_train_pred, train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(spec1_val_pred, validate$ClaimNb)
spec1_params <- length(coef(spec1))



##########################################################
# Specification 2: VehAge=0 separate + continuous for rest
##########################################################
train$VehAge_Zero <- ifelse(train$VehAge == 0, 1, 0)
train$VehAge_Cont <- ifelse(train$VehAge == 0, 0, train$VehAge)
validate$VehAge_Zero <- ifelse(validate$VehAge == 0, 1, 0)
validate$VehAge_Cont <- ifelse(validate$VehAge == 0, 0, validate$VehAge)

spec2 <- glm(ClaimNb ~ VehAge_Zero + VehAge_Cont,
             family = poisson(),
             data = train,
             offset = log(Exposure))

spec2_train_pred <- fitted(spec2)
spec2_val_pred <- predict(spec2, newdata = validate, type = "response")

spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(spec2_train_pred, train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(spec2_val_pred, validate$ClaimNb)
spec2_params <- length(coef(spec2))



##########################################################
# Specification 3: Simple [0], [1-14], [15+]
##########################################################
train$VehAge_Simple <- cut(train$VehAge,
                           breaks = c(-0.5, 0.5, 14.5, 1000),
                           labels = c("0", "1-14", "15+"),
                           right = FALSE)
validate$VehAge_Simple <- cut(validate$VehAge,
                              breaks = c(-0.5, 0.5, 14.5, 1000),
                              labels = c("0", "1-14", "15+"),
                              right = FALSE)

spec3 <- glm(ClaimNb ~ VehAge_Simple,
             family = poisson(),
             data = train,
             offset = log(Exposure))

spec3_train_pred <- fitted(spec3)
spec3_val_pred <- predict(spec3, newdata = validate, type = "response")

spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(spec3_train_pred, train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(spec3_val_pred, validate$ClaimNb)
spec3_params <- length(coef(spec3))



##########################################################
# Specification 4: Observed [0], [1-2], [3-14], [15+]
##########################################################
train$VehAge_Obs <- cut(train$VehAge,
                        breaks = c(-0.5, 0.5, 2.5, 14.5, 1000),
                        labels = c("0", "1-2", "3-14", "15+"),
                        right = FALSE)
validate$VehAge_Obs <- cut(validate$VehAge,
                           breaks = c(-0.5, 0.5, 2.5, 14.5, 1000),
                           labels = c("0", "1-2", "3-14", "15+"),
                           right = FALSE)

spec4 <- glm(ClaimNb ~ VehAge_Obs,
             family = poisson(),
             data = train,
             offset = log(Exposure))

spec4_train_pred <- fitted(spec4)
spec4_val_pred <- predict(spec4, newdata = validate, type = "response")

spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(spec4_train_pred, train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(spec4_val_pred, validate$ClaimNb)
spec4_params <- length(coef(spec4))



##########################################################
# Specification 5: Natural Spline - Testing Different DF
##########################################################
# Test splines with df from 2 to 8 to find optimal
df_values <- 2:8
spline_results <- data.frame(
  DF = df_values,
  AIC = NA,
  Params = NA
)

# Fit spline for each df
for(i in 1:length(df_values)) {
  spec_temp <- glm(ClaimNb ~ ns(VehAge, df = df_values[i]),
                   family = poisson(),
                   data = train,
                   offset = log(Exposure))
  
  spline_results$AIC[i] <- AIC(spec_temp)
  spline_results$Params[i] <- length(coef(spec_temp))
}

print("Spline DF Comparison:")
print(spline_results)

# Plot AIC vs DF
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25,
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

plot(spline_results$DF, spline_results$AIC,
     type = "b", pch = 19, col = "#8d17f1",
     xlab = "Degrees of Freedom",
     ylab = "AIC",
     main = "AIC vs Spline Degrees of Freedom",
     lwd = 2, cex = 1.2)

lines(spline_results$DF, spline_results$AIC, col = "black", lwd = 2)
grid()

# Based on elbow analysis, we choose df = 4
# Now fit spec5 with df = 4 for comparison table

spec5 <- glm(ClaimNb ~ ns(VehAge, df = 4),
             family = poisson(),
             data = train,
             offset = log(Exposure))

spec5_train_pred <- fitted(spec5)
spec5_val_pred <- predict(spec5, newdata = validate, type = "response")

spec5_aic <- AIC(spec5)
spec5_train_dev <- Poisson.Deviance(spec5_train_pred, train$ClaimNb)
spec5_val_dev <- Poisson.Deviance(spec5_val_pred, validate$ClaimNb)
spec5_params <- length(coef(spec5))



################################################################################
# Comparison Table for 5 Specifications above
################################################################################
comparison <- data.frame(
  Specification = c(
    "Paper [0,1),[1,10],(10, \infty)",
    "VehAge=0 + continuous",
    "Simple [0],[1-14],[15+]",
    "Observed [0],[1-2],[3-14],[15+]",
    "Spline (DF=4)"
  ),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params, spec5_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic, spec5_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev, spec5_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev, spec5_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - min(comparison$AIC)

print("VehAge Specification Comparison:")
print(comparison)

# Find best model by validation deviance
best_idx <- which.min(comparison$Val_Dev)
print("Best specification (lowest validation deviance):")
print(comparison[best_idx, ])
# Spec 4 is almost identical, running tests to see which would be better. 
# So since Spec 3 and Spec 4 are so close, I'm going to try implement the model appraisal techniques in slides 178-188 of the S2 notes (updated v5 version).



################################################################################
# Part C: Final Justification using Model Appraisal Slides
################################################################################
set.seed(100)
n_bootstrap <- 1000

# Storage for bootstrap deviances
bootstrap_results <- data.frame(
  Spec3_Dev = numeric(n_bootstrap),
  Spec4_Dev = numeric(n_bootstrap)
)

# Run bootstrap
for(b in 1:n_bootstrap) {
  # Sample validation data with replacement
  boot_idx <- sample(1:nrow(validate), nrow(validate), replace = TRUE)
  boot_data <- validate[boot_idx, ]
  
  # Get predictions for both specs on bootstrap sample
  spec3_boot_pred <- predict(spec3, newdata = boot_data, type = "response")
  spec4_boot_pred <- predict(spec4, newdata = boot_data, type = "response")
  
  # Calculate deviances
  bootstrap_results$Spec3_Dev[b] <- Poisson.Deviance(spec3_boot_pred, boot_data$ClaimNb)
  bootstrap_results$Spec4_Dev[b] <- Poisson.Deviance(spec4_boot_pred, boot_data$ClaimNb)
}

# Summary statistics
print(summary(bootstrap_results))
# They agree at literally every key summary statistic
# Likely take simpler model (spec 3)

# Boxplot comparison
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25,
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))
# No visible difference (obviously). Consider including this plot in the report.


boxplot(bootstrap_results$Spec3_Dev, bootstrap_results$Spec4_Dev,
        names = c("Spec 3: [0],[1-14],[15+]", "Spec 4: [0],[1-2],[3-14],[15+]"),
        ylab = "Poisson Deviance",
        main = "Bootstrap Validation Deviance Comparison",
        col = c("#8d17f1", "lightblue"),
        las = 1)


# Wilcoxon signed-rank test
wilcox_test <- wilcox.test(bootstrap_results$Spec3_Dev, 
                           bootstrap_results$Spec4_Dev, 
                           paired = TRUE)
print(wilcox_test)
# Insists the difference is significant (Despite being tiny)
# The boxplots show the distributions separately, but WC is testing if across the 1000 samples, is one model consistently better than the other.
# Since p-value small, likely that this difference isn't actually random noise, one of these is in fact better.

# Mean deviance
print(paste("Spec 3:", mean(bootstrap_results$Spec3_Dev)))
print(paste("Spec 4:", mean(bootstrap_results$Spec4_Dev)))

# Median deviance
print(paste("Spec 3:", median(bootstrap_results$Spec3_Dev)))
print(paste("Spec 4:", median(bootstrap_results$Spec4_Dev)))

# The winner depends on if we class on mean or median (of course, could never be simple)
# The difference is also negligible (mean difference is -0.000201396926087227).
# Choosing the simpler model (Spec 3). 



################################################################################
# Part D: Saving Final figs for Report
################################################################################
# Re-defining required plots at end of each EDA. This is to save having to comb through and find all plots when small formatting edits (Text size etc) being made for report.
# Will not be changing core content of each plot
# Plot 1: Frequency with Confidence Intervals
# THESE PLOTS NEED TO BE EDITED WHEN GOING INTO FINAL REPORT. 
png("figs/VehAge_figs/01_frequency_with_CI.png", width = 800, height = 600)
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", 
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25, 
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

plot(vehage_analysis$VehAge, vehage_analysis$Frequency,
     type = "p", pch = 19, 
     ylim = c(0, max(vehage_analysis$CI_upper) * 1.1),
     xlab = "Vehicle Age", ylab = "Frequency",
     main = "Frequency by VehAge with 95% Confidence Intervals",
     col = "#8d17f1",
     cex = 1.1)

arrows(vehage_analysis$VehAge, vehage_analysis$CI_lower,
       vehage_analysis$VehAge, vehage_analysis$CI_upper,
       angle = 90, code = 3, length = 0.05, col = "green")

abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)

lines(vehage_analysis$VehAge, vehage_analysis$Frequency, col = "black", lwd = 1.6)

grid()
dev.off()

# Plot 2: AIC vs Spline DF
png("figs/VehAge_figs/02_spline_AIC_comparison.png", width = 800, height = 600)
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25,
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

plot(spline_results$DF, spline_results$AIC,
     type = "b", pch = 19, col = "#8d17f1",
     xlab = "Degrees of Freedom",
     ylab = "AIC",
     main = "AIC vs Spline Degrees of Freedom",
     lwd = 2, cex = 1.2)

lines(spline_results$DF, spline_results$AIC, col = "black", lwd = 2)

grid()
dev.off()

# Plot 3: Bootstrap Validation Comparison
png("figs/VehAge_figs/03_bootstrap_validation.png", width = 800, height = 600)
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i",
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25,
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

boxplot(bootstrap_results$Spec3_Dev, bootstrap_results$Spec4_Dev,
        names = c("Spec 3: [0],[1-14],[15+]", "Spec 4: [0],[1-2],[3-14],[15+]"),
        ylab = "Poisson Deviance",
        main = "Bootstrap Validation Deviance Comparison",
        col = c("#8d17f1", "lightblue"),
        las = 1)

grid()
dev.off()

