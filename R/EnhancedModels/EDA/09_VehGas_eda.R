################################################################################
# EDA (9): VehGas
################################################################################
rm(list = ls())

options(timeout = 600)

# Load required data for EDA
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load in data for GLM Spec testing
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric 
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}


################################################################################
# 1). VehGas Summary and Distribution
################################################################################
# VehGas Summary
print(table(learn$VehGas))
print(prop.table(table(learn$VehGas)))
# Nearly perfectly balanced: Diesel 48.98%, Regular 51.02%
# Good split for modeling, no sparsity concerns



################################################################################
# 2). Frequency by VehGas
################################################################################
vehgas_analysis <- data.frame(
  VehGas = sort(unique(learn$VehGas)),
  Policies = sapply(sort(unique(learn$VehGas)), function(x) sum(learn$VehGas == x)),
  Exposure = sapply(sort(unique(learn$VehGas)), function(x) sum(learn$Exposure[learn$VehGas == x])),
  Claims = sapply(sort(unique(learn$VehGas)), function(x) sum(learn$ClaimNb[learn$VehGas == x]))
)

vehgas_analysis$Frequency <- vehgas_analysis$Claims / vehgas_analysis$Exposure
vehgas_analysis$Pct_Policies <- 100 * vehgas_analysis$Policies / sum(vehgas_analysis$Policies)

# Frequency by VehGas
print(vehgas_analysis)

# Frequency difference 
round(diff(vehgas_analysis$Frequency), 4)
# Diesel: 9.70% frequency (LOWER risk)
# Regular: 10.31% frequency (HIGHER risk)
# Difference: 0.61 percentage points (6.3% relative difference)
# BY FAR smallest effect seen: 10x smaller than Area, ~35x smaller than VehAge=0
# Effect is tiny but statistically detectable with 610k policies



################################################################################
# 3). Check for confounding with other predictors
################################################################################
# VehGas by VehBrand (checking if certain brands are diesel-heavy)
vehgas_vehbrand <- table(learn$VehGas, learn$VehBrand)

# VehGas by VehBrand (proportions)
print(prop.table(vehgas_vehbrand, margin = 2))

# VehGas by Region (checking for geographic patterns)
vehgas_region <- aggregate(VehGas == "Diesel" ~ Region, data = learn, FUN = mean)
names(vehgas_region) <- c("Region", "Pct_Diesel")
vehgas_region <- vehgas_region[order(-vehgas_region$Pct_Diesel), ]

# Top 10 regions by % Diesel
print(head(vehgas_region, 10))

# VehGas by VehBrand showed minimal confounding
# Most brands 45-56% Diesel (clustered around 50%)
# B10 highest (73.6% Diesel), B14 lowest (32.0% Diesel)
# B12 rental cars: 45.4% Diesel (below average, not strongly diesel-biased)

# VehGas by Region: Minimal geographic variation
# Range 46-59% Diesel across regions (narrow compared to B12 concentration 40-70%)
# R21 highest (59.5%), most regions around 50%
# Much less variation than strong predictors like Region or VehBrand

# VehGas only weakly confounded with other predictors
# Diesel appears safer (9.70% vs 10.31%) - may proxy for driver behavior, vehicle type
# Effect tiny but worth including for replication (paper used it, 1 parameter, 27.0 deviance)



################################################################################
# GLM Specification Testing for VehGas
################################################################################
##########################################################
# Specification 1: Paper - Binary (Diesel vs Regular)
##########################################################
# Only one specification, VehGas is binary
# Paper uses as-is: Diesel vs Regular
spec1 <- glm(ClaimNb ~ VehGas, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))

# VehGas Specification
print(data.frame(
  Specification = "Binary: Diesel vs Regular",
  Params = spec1_params,
  AIC = spec1_aic,
  Train_Dev = spec1_train_dev,
  Val_Dev = spec1_val_dev
))

# Coefficient
print(summary(spec1)$coefficients)

# Effect size: Regular relative to Diesel (Diesel is reference)
regular_coef <- coef(spec1)["VehGasRegular"]

# Regular effect (log scale)
round(regular_coef, 4)

# Regular multiplier vs Diesel
round(exp(regular_coef), 4)

# Regular increases frequency by
round((exp(regular_coef) - 1), 4)


# Likely going to include VehGas as binary (Diesel vs Regular)
# Effect small (6.7% relative, 0.61pp absolute) - weakest predictor (27.0 deviance vs 3061.1 for BonusMalus)
# However, is statistically significant (p<0.001), paper used it, only 1 parameter, no spec decisions needed
# May add marginal value in full model, no cost to include
# Can also look at interactions later. 