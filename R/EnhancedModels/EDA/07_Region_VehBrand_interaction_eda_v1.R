################################################################################
# EDA (7): VehBrand × Region Interaction Test
################################################################################
rm(list = ls())

# Load required fitting and validation data for GLM specs
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# Minor Data Preparation
################################################################################
# Load k = 4 cluster assignments
cluster_lookup <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/LookupKeys/region_kmeans_k4_lookup.csv")

# Create necessary variables
train$LogDensity <- log(train$Density)
validate$LogDensity <- log(validate$Density)

# VehBrand grouping (from VehBrand EDA)
train$VehBrand_Final <- ifelse(train$VehBrand == "B12", "High",
                               ifelse(train$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))
validate$VehBrand_Final <- ifelse(validate$VehBrand == "B12", "High",
                                  ifelse(validate$VehBrand %in% c("B3","B4","B5","B11","B13"), "Medium", "Low"))

# K-means k=4 grouping
cluster_map_k4 <- setNames(cluster_lookup$Cluster_k4, cluster_lookup$Region)
train$Region_K4 <- factor(cluster_map_k4[train$Region])
validate$Region_K4 <- factor(cluster_map_k4[validate$Region])



################################################################################
# Test 1: Paper's 22 Region levels
################################################################################
# Main effects only
model1_main <- glm(ClaimNb ~ VehBrand_Final + Region + LogDensity,
                   family = poisson(), data = train, offset = log(Exposure))

# Metrics for table
m1_aic <- AIC(model1_main)
m1_train_dev <- Poisson.Deviance(fitted(model1_main), train$ClaimNb)
m1_val_dev <- Poisson.Deviance(predict(model1_main, newdata = validate, type = "response"), validate$ClaimNb)
m1_params <- length(coef(model1_main))

# With interaction
model1_int <- glm(ClaimNb ~ VehBrand_Final + Region + LogDensity + VehBrand_Final:Region,
                  family = poisson(), data = train, offset = log(Exposure))

# Metrics for table
m1int_aic <- AIC(model1_int)
m1int_train_dev <- Poisson.Deviance(fitted(model1_int), train$ClaimNb)
m1int_val_dev <- Poisson.Deviance(predict(model1_int, newdata = validate, type = "response"), validate$ClaimNb)
m1int_params <- length(coef(model1_int))



################################################################################
# Test 2: K-means k=4 Region clustering
################################################################################
# Main effects only
model2_main <- glm(ClaimNb ~ VehBrand_Final + Region_K4 + LogDensity,
                   family = poisson(), data = train, offset = log(Exposure))

# Metrics for table
m2_aic <- AIC(model2_main)
m2_train_dev <- Poisson.Deviance(fitted(model2_main), train$ClaimNb)
m2_val_dev <- Poisson.Deviance(predict(model2_main, newdata = validate, type = "response"), validate$ClaimNb)
m2_params <- length(coef(model2_main))

# With interaction
model2_int <- glm(ClaimNb ~ VehBrand_Final + Region_K4 + LogDensity + VehBrand_Final:Region_K4,
                  family = poisson(), data = train, offset = log(Exposure))

# Metrics for table
m2int_aic <- AIC(model2_int)
m2int_train_dev <- Poisson.Deviance(fitted(model2_int), train$ClaimNb)
m2int_val_dev <- Poisson.Deviance(predict(model2_int, newdata = validate, type = "response"), validate$ClaimNb)
m2int_params <- length(coef(model2_int))



################################################################################
# Comparison Table
################################################################################
comparison_interaction <- data.frame(
  Model = c("Region(22): Main effects", 
            "Region(22): + Interaction",
            "Region(k4): Main effects",
            "Region(k4): + Interaction"),
  Params = c(m1_params, m1int_params, m2_params, m2int_params),
  AIC = c(m1_aic, m1int_aic, m2_aic, m2int_aic),
  Train_Dev = c(m1_train_dev, m1int_train_dev, m2_train_dev, m2int_train_dev),
  Val_Dev = c(m1_val_dev, m1int_val_dev, m2_val_dev, m2int_val_dev)
)

comparison_interaction$Delta_AIC <- c(
  0, 
  m1int_aic - m1_aic,
  m2_aic - m1_aic,
  m2int_aic - m1_aic
)

# VehBrand × Region Interaction Test
print(comparison_interaction)

# AIC improvement from adding interaction
print(paste("22 Region levels:", round(m1_aic - m1int_aic, 2)))
print(paste("k=4 clusters:", round(m2_aic - m2int_aic, 2)))

# Best model
print(comparison_interaction[which.min(comparison_interaction$Val_Dev), ])
