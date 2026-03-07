################################################################################
# EDA: VehBrand - Finding the Rental Car Signal
################################################################################
rm(list = ls())

# Load EDA data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# GLM spec testing data
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Metric as per
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# PART 1: EDA to decide on GLM Specifications to Test
################################################################################
################################################################################
# 1). Frequency by VehBrand
################################################################################
vehbrand_analysis <- data.frame(
  VehBrand = sort(unique(learn$VehBrand)),
  Policies = sapply(sort(unique(learn$VehBrand)), function(x) sum(learn$VehBrand == x)),
  Exposure = sapply(sort(unique(learn$VehBrand)), function(x) sum(learn$Exposure[learn$VehBrand == x])),
  Claims = sapply(sort(unique(learn$VehBrand)), function(x) sum(learn$ClaimNb[learn$VehBrand == x]))
)

vehbrand_analysis$Frequency <- vehbrand_analysis$Claims / vehbrand_analysis$Exposure
vehbrand_analysis$Pct_Policies <- 100 * vehbrand_analysis$Policies / sum(vehbrand_analysis$Policies)

#Frequency by VehBrand
print(vehbrand_analysis[order(-vehbrand_analysis$Frequency), ])


################################################################################
# 2). VehBrand B12 x VehAge=0 Cross-Tab
################################################################################
# trying to conretely verify the B12 notoriety in report.
# Seems like could make for an interesting sub-chapter.
b12_age0 <- sum(learn$VehBrand == "B12" & learn$VehAge == 0)
b12_total <- sum(learn$VehBrand == "B12")

print(paste("B12 policies with VehAge=0:", b12_age0, "out of", b12_total))
print(paste("% of B12 that are VehAge=0:", round(100 * b12_age0 / b12_total, 1), "%"))



################################################################################
# 3). VehBrand B12 × Region Cross-Tab
################################################################################
b12_by_region <- aggregate(VehBrand == "B12" ~ Region, data = learn, FUN = sum)
names(b12_by_region) <- c("Region", "B12_Count")
b12_by_region$Total <- aggregate(VehBrand ~ Region, data = learn, FUN = length)$VehBrand
b12_by_region$B12_Pct <- 100 * b12_by_region$B12_Count / b12_by_region$Total

#B12 concentration by Region
print(b12_by_region[order(-b12_by_region$B12_Pct), ])


# VehBrand B12 is clearly a rental car fleet:
## Highest frequency: 13.59% (vs 9-10% for other brands)
## 22.4% of B12 policies are VehAge=0 (vs 2.5% overall) - rental turnover signal
## Geographically concentrated: R94 (69.9% B12), R21 (66.3%), R11 (52.8%)
## This explains elevated frequency: rental cars (VehAge=0) have 31% claim rate
## Suggests VehBrand=B12 × VehAge=0 × Region interaction in final model

## Very Annoying but necessary for great model considerations
# If I just optimise the main effects now, the interaction might not perform as well as if I had optimised that specifically.
# I do not know how to work on interactions before deciding main effects, so I have decided to just do main effect as if the interaction doesn't exist yet.
# Interaction testing will be performed later. 
# If we are using a 3-way interaction, should probably keep number of predictors low => k-means clustering could be useful sigh. 
# Man I hate EDA 





################################################################################
# PART 2: GLM Specification Testing for VehBrand
################################################################################
##########################################################
# Specification 1: Paper - All 11 VehBrand levels
##########################################################
# Baseline from paper. Testing if finer granularity needed.
spec1 <- glm(ClaimNb ~ VehBrand, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))

##########################################################
# Specification 2: B12 vs Rest
##########################################################
# Tests rental car hypothesis: B12 distinct (13.59% freq, 22.4% VehAge=0).
# Might give better fit, but I don't like the real world application of this one.
# Feels like skipping to much data granularity just to isolate one bad group. 
train$VehBrand_B12 <- ifelse(train$VehBrand == "B12", "B12", "Other")
validate$VehBrand_B12 <- ifelse(validate$VehBrand == "B12", "B12", "Other")

spec2 <- glm(ClaimNb ~ VehBrand_B12, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))

##########################################################
# Specification 3: Frequency Tiers
##########################################################
# Grouping "similiar" risk profiles, basically just informal k-means. High (B12: 13.59%), Medium (B3,B4,B5,B11,B13: 9.7-10.4%), Low (rest: 7.6-9.2%)
train$VehBrand_Tiers <- ifelse(train$VehBrand == "B12", "High",
                               ifelse(train$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))
validate$VehBrand_Tiers <- ifelse(validate$VehBrand == "B12", "High",
                                  ifelse(validate$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))

spec3 <- glm(ClaimNb ~ VehBrand_Tiers, family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))

##########################################################
# Specification 4: Volume + Risk Groups
##########################################################
# Similiar to spec 2, but allowing commom brands aswell. B12 (rental, 24.5%), B1+B2 (common brands, 47.6%), Rest (smaller brands)
train$VehBrand_Vol <- ifelse(train$VehBrand == "B12", "B12",
                             ifelse(train$VehBrand %in% c("B1","B2"), "Common", "Other"))
validate$VehBrand_Vol <- ifelse(validate$VehBrand == "B12", "B12",
                                ifelse(validate$VehBrand %in% c("B1","B2"), "Common", "Other"))

spec4 <- glm(ClaimNb ~ VehBrand_Vol, family = poisson(), data = train, offset = log(Exposure))
spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(fitted(spec4), train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(predict(spec4, newdata = validate, type = "response"), validate$ClaimNb)
spec4_params <- length(coef(spec4))

################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: All 11 levels", "B12 vs Rest", "Frequency Tiers", "Volume + Risk"),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])
# Spec 1 and 3 have same Val_Dev (0.00008) difference, but AIC better on spec 3.
# These sdo seem just to be tied, but I've written easy to implement code for bootstrapping, so will run test out of curiosity. 



################################################################################
# Bootstrap: Paper vs Spec3
################################################################################
set.seed(100)
n_bootstrap <- 1000

# Data fram to store results
bootstrap_results <- data.frame(Paper = numeric(n_bootstrap), 
                                Spec3 = numeric(n_bootstrap))

for(b in 1:n_bootstrap) {
  boot_idx <- sample(1:nrow(validate), nrow(validate), replace = TRUE)
  boot_data <- validate[boot_idx, ]
  
  bootstrap_results$Paper[b] <- Poisson.Deviance(predict(spec1, newdata = boot_data, type = "response"), boot_data$ClaimNb)
  bootstrap_results$Spec3[b] <- Poisson.Deviance(predict(spec3, newdata = boot_data, type = "response"), boot_data$ClaimNb)
}

# Paper mean 
mean(bootstrap_results$Paper)

# Spec3 mean
mean(bootstrap_results$Spec3)

# Difference
mean(bootstrap_results$Paper) - mean(bootstrap_results$Spec3)

# Wilcoxon test
wilcox_test <- wilcox.test(bootstrap_results$Paper, bootstrap_results$Spec3, paired = TRUE)
print(wilcox_test)
# Difference is not significant
# Spec 3 is the winner, but not by a significant amount.
# However, less parameters means lower AIC, and the interaction terms might be better. 

# Spec 3 frequency tiers seems to be the best. Both perform identically, less parameters will just be better when I get to the interactions stage





################################################################################
# Part 3: Saving Required Figures for the Report.
################################################################################
# Plot 1: Frequency by VehBrand
png("figs/VehBrand_figs/01_frequency_by_brand.png", width = 800, height = 600)
par(mfrow = c(1, 1), xaxs = "i", yaxs = "i", mar = c(5.5, 5.5, 3, 1), tcl = -0.25, 
    cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, col = "black", mgp = c(3.5, 0.7, 0))

vb_sorted <- vehbrand_analysis[order(vehbrand_analysis$Frequency), ]
barplot(vb_sorted$Frequency, names.arg = vb_sorted$VehBrand, 
        col = ifelse(vb_sorted$VehBrand == "B12", "#8d17f1", "lightgray"),
        xlab = "Vehicle Brand", ylab = "Frequency",
        main = "Claim Frequency by Vehicle Brand", las = 2)
abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2)
grid()
dev.off()

# Plot 2: B12 Geographic Clustering (top 10 regions)
png("figs/VehBrand_figs/02_B12_geographic_clustering.png", width = 800, height = 600)
par(mfrow = c(1, 1), xaxs = "i", yaxs = "i", mar = c(5.5, 5.5, 3, 1), tcl = -0.25, 
    cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, col = "black", mgp = c(3.5, 0.7, 0))

top_regions <- b12_by_region[order(-b12_by_region$B12_Pct), ][1:10, ]
barplot(top_regions$B12_Pct, names.arg = top_regions$Region,
        col = "#8d17f1", xlab = "Region", ylab = "% B12 Policies",
        main = "B12 Concentration by Region (Top 10)", las = 2)
grid()
dev.off()
