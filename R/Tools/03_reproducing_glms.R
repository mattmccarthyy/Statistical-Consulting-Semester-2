rm(list = ls())

################################################################################
# Load in all data
################################################################################
train <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")

# 1). Area
# Convert to factor first, then to integer to get levels 1-6 as in paper, otherwise NA's introduced.
train$AreaGLM <- as.integer(as.factor(train$Area))
test$AreaGLM <- as.integer(as.factor(test$Area))

# 2). VehPower: Categorical, merge groups >= 9 into a single class 
train$VehPowerGLM <- as.factor(pmin(train$VehPower, 9))
test$VehPowerGLM <- as.factor(pmin(test$VehPower, 9))

# 3). VehAge: 3 categorical classes [0,1), [1, 10], (10, inf)
# Also making class "2" [1, 10] the reference level
train$VehAgeGLM <- as.factor(ifelse(train$VehAge < 1, "1", ifelse(train$VehAge <= 10, "2", "3")))
train$VehAgeGLM <- relevel(train$VehAgeGLM, ref = "2")
test$VehAgeGLM <- as.factor(ifelse(test$VehAge < 1, "1", ifelse(test$VehAge <= 10, "2", "3")))
test$VehAgeGLM <- relevel(test$VehAgeGLM, ref = "2")

# 4). DrivAge: 7 categorical classes
# Reference level is set to class "5" [41, 51) 
age_breaks <- c(18, 21, 26, 31, 41, 51, 71, Inf)
train$DrivAgeGLM <- cut(train$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
train$DrivAgeGLM <- relevel(train$DrivAgeGLM, ref = "5")
test$DrivAgeGLM <- cut(test$DrivAge, breaks = age_breaks, right = FALSE, labels = 1:7)
test$DrivAgeGLM <- relevel(test$DrivAgeGLM, ref = "5")

# 5). BonusMalus: capping at 150
train$BonusMalusGLM <- pmin(train$BonusMalus, 150)
test$BonusMalusGLM <- pmin(test$BonusMalus, 150)

# 6). Density: Log-density chosen as a cts log-linear component 
train$DensityGLM <- log(train$Density)
test$DensityGLM <- log(test$Density)

# 7). Region: Categorical component with R24 as the reference level
train$Region <- relevel(as.factor(train$Region), ref = "R24")
test$Region <- relevel(as.factor(test$Region), ref = "R24")

# 8). VehBrand: Ensure it is a factor (ensure ref level is B1 as in paper)
train$VehBrand <- relevel(as.factor(train$VehBrand), ref = "B1")
test$VehBrand <- relevel(as.factor(test$VehBrand), ref = "B1")

# 9). Log-Exposure: Create the offset variable for the Poisson model 
# Can do this in GLM call, but want to use dimensionality of feature space as a verification
train$logExposure <- log(train$Exposure)
test$logExposure <- log(test$Exposure)

# 10). VehGas is treated as Binary in the paper
train$VehGas <- as.factor(train$VehGas)
test$VehGas <- as.factor(test$VehGas)


################################################################################
# Verifying Same Dimensionality as Paper
################################################################################
# Defining Model GLM1 formula (excluded the intercept and offset)
glm_formula <- ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region

# Create design matrix for the train set
design_matrix <- model.matrix(glm_formula, data = train)

# Check dimensions
# Subtract 1 to exclude the (Intercept) column for feature space 'd'
d_dim <- ncol(design_matrix) - 1; d_dim # 48 as expected. 
# Fine to proceed in reproducing GLM's.



################################################################################
# Reproducing GLM's
################################################################################
# Model GLM1 - All features
glm1 <- glm(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region, 
              family = poisson(link = log), data = train, offset = log(Exposure))

# Model GLM2 - Dropping Area (due to collinearity with Density)
glm2 <- glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region, 
                             family = poisson(link = log), data = train, offset = log(Exposure))

# Model GLM3 - Drop Area and VehBrand
glm3 <- glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM + VehGas + DensityGLM + Region, 
              family = poisson(link = log), data = train, offset = log(Exposure))



################################################################################
# Verifying that these are identical to those in paper
################################################################################
# Copying authors average Poisson deviance loss (Formula 2.2)
# Note: predicted (type = "response") already incorporates exposure* 
# Matching logic used in Listing 6
calc_avg_deviance <- function(observed, predicted) {
  # Using sum logic to handle the log(0^0) = 0 case (as mentioned in class 27/01/2026)
  total_deviance <- 2 * (sum(predicted) - sum(observed) + sum(log((observed / predicted)^observed), na.rm = TRUE))
  
  # Return average loss in units of 10^-2 as in Table 5 
  return((total_deviance / length(observed)) * 100)
}

# List of models for iteration
models <- list("Model GLM1" = glm1, "Model GLM2" = glm2, "Model GLM3" = glm3)

# Result table initialisation
results <- data.frame(Model = names(models), In_Sample = NA, Out_of_Sample = NA)

for (i in seq_along(models)) {
  m_obj <- models[[i]]
  
  # Predict expected number of claims (lambda * exposure) 
  pred_train <- predict(m_obj, newdata = train, type = "response")
  pred_test <- predict(m_obj, newdata = test, type = "response")
  
  # Calculate losses to check if matching Table 5 values 
  results$In_Sample[i] <- calc_avg_deviance(train$ClaimNb, pred_train)
  results$Out_of_Sample[i] <- calc_avg_deviance(test$ClaimNb, pred_test)
}

# Display results
print(results) # Exactly as in Paper



################################################################################
# Saving for later comparison
################################################################################
# Save the models as RDS files to preserve the R objects exactly

# saveRDS(glm1, file = "R/GLMs/GLM1_full_model.rds")
# saveRDS(glm2, file = "R/GLMs/GLM2_no_area.rds")
# saveRDS(glm3, file = "R/GLMs/GLM3_reduced_model.rds")

# These RDS files are too large to store because the RDS stores the training data also
# Can either use Git LFS, but loading in will still be tedious on my own potato wifi
# Can strip the data from GLM object to shrink them (going with this)

strip_glm <- function(mod) {
  ### NOTE: This is taken from online, not original work.
  # 1). Removing model frame and orig. data 
  mod$data <- NULL
  mod$model <- NULL
  mod$y <- NULL
  
  # 2). Remove working vectors used for training diagnostics
  # (recalculated anyway when using predict() on new data)
  mod$residuals <- NULL
  mod$fitted.values <- NULL
  mod$effects <- NULL
  mod$linear.predictors <- NULL
  mod$weights <- NULL
  mod$prior.weights <- NULL
  
  # 3). Clean up the QR decomposition to save further space
  # Just removing memory of fitting process, don't need this matrix
  mod$qr$qr <- NULL 
  
  return(mod)
}

# Applying to models
glm1 <- strip_glm(glm1)
glm2 <- strip_glm(glm2)
glm3 <- strip_glm(glm3)

# Now saving lightweight versions of original GLMs
saveRDS(glm1, file = "R/GLMs/GLM1_full_model.rds")
saveRDS(glm2, file = "R/GLMs/GLM2_no_area.rds")
saveRDS(glm3, file = "R/GLMs/GLM3_reduced_model.rds")
