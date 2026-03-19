################################################################################
# Fitting an XGB to see how low I can get OOS Loss
################################################################################
rm(list = ls())
options(timeout = 600)



################################################################################
# Required packages
################################################################################
library(xgboost)
library(Matrix) # For sparse.model.matrix(), does the dummy encoding, means I don't have to do manually or use fastDummmies. 
library(parallel) # Only for detectCores()



################################################################################
# Data for fitting and testing
################################################################################
# Using the data formatted for the GLM. This has all updated specifications of factors etc. 
u <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/main/R/EnhancedModels/data/EnhancedGLMDataset/Train.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
train <- readRDS(f)

u <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/main/R/EnhancedModels/data/EnhancedGLMDataset/Test.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
test <- readRDS(f)

rm(u, f)



################################################################################
# Comparison Metric
################################################################################
Poisson.Deviance <- function(pred, obs) {
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}



################################################################################
# Minor Pre-Processing for XGB 
################################################################################
# Dropping the columns we won't need
str(train); str(test)

predictors_to_keep <- c(
  "ClaimNb",
  "Exposure",
  "AreaGLM",
  "VehPowerGLM",
  "VehAge_3grp",
  "DrivAge",
  "BM_is50",
  "BM_above50",
  "BM_above100",
  "VehBrand",
  "VehGas",
  "DensityGLM",
  "Region"
)


# Make train and test sets with only the columns we want to keep 
train_xgb <- train[, predictors_to_keep]
test_xgb <- test[, predictors_to_keep]


# str(train_xgb); str(test_xgb)
# min(train$Exposure); min(test$Exposure) # All > 0 so base_margin won't have any issues.

# Using sparse.model.matrix() so that factor variables are converted to 
# consistent set of dummy columns in both train and test, with one column per level.
factor_vars <- c(
  "AreaGLM",
  "VehPowerGLM",
  "VehAge_3grp",
  "VehBrand",
  "VehGas",
  "Region"
)

# Force full dummy encoding for each factor level
# Will have to account for this when using SHAP later. 
full_dummy_contrasts <- lapply(
  train_xgb[, factor_vars, drop = FALSE],
  contrasts,
  contrasts = FALSE
)

# Define the XGBoost feature set
xgb_formula <- ~ AreaGLM + VehPowerGLM + VehAge_3grp + DrivAge + BM_is50 + BM_above50 + BM_above100 + VehBrand + VehGas + DensityGLM + Region - 1
# - 1 to remove the intercept.

# Create the design matrices
X_train <- sparse.model.matrix(
  xgb_formula,
  data = train_xgb,
  contrasts.arg = full_dummy_contrasts
)

X_test <- sparse.model.matrix(
  xgb_formula,
  data = test_xgb,
  contrasts.arg = full_dummy_contrasts
)

# Check that train and test ended up with the same columns
# stopifnot(identical(colnames(X_train), colnames(X_test))) # identical as needed.



################################################################################
# Fitting first XGB
################################################################################
xgb_1 <- xgboost(
  x = X_train,
  y = train_xgb$ClaimNb,
  objective = "count:poisson",
  eval_metric = "poisson-nloglik",
  base_margin = log(train_xgb$Exposure),
  nrounds = 500,
  learning_rate = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbosity = 1,
  monitor_training = TRUE,
  print_every_n = 25
)

# Get predictions
train_pred <- predict(xgb_1, X_train, base_margin = log(train_xgb$Exposure))
test_pred <- predict(xgb_1, X_test, base_margin = log(test_xgb$Exposure))

# Display predications
xgb_results <- data.frame(
  Model = "XGBoost1",
  Parameters = NA,
  AIC = NA,
  Train_Dev = Poisson.Deviance(train_pred, train_xgb$ClaimNb),
  Test_Dev = Poisson.Deviance(test_pred, test_xgb$ClaimNb)
)

print(xgb_results)
# This already beats GLM's and took minimal effort. Seeing how far we can take this. 



################################################################################
# The biggest of guns (cannons, even)
################################################################################
# Using 5-fold cross validation
# Doing a grid search for hyperparameter optimisation
# Biasing the grid toward simpler models so I can prove it is interpretable, one issue with model matrix to discuss in report.
# Using in-built loss function, differs from ours on scale and by a constant, so will still rank models the same
# Frankly I don't back myself to mess with the loss function
# Testing 56 hyperparameter combinations, should all remain interpretable enough.
# I have learned from my mistakes, printing i every time an iteration completes.

# XGBoost's CV function works with an xgb.DMatrix.
# We include log(Exposure) as base_margin so the fit mirrors the GLM offset.
dtrain <- xgb.DMatrix(
  data = X_train,
  label = train_xgb$ClaimNb,
  base_margin = log(train_xgb$Exposure)
)

# Use all available CPU cores except 1.
nthread_use <- max(1, detectCores() - 1)

# Fix the folds once so every hyperparameter combination is judged on the same
# 5 train/validation splits.
set.seed(1106)
fold_id <- sample(rep(1:5, length.out = nrow(train_xgb)))
cv_folds <- lapply(1:5, function(k) which(fold_id == k))

# Grid biased toward more interpretable tree models.
# max_depth kept to 3, 4 or 5. Keeping trees shallow
# min_child_weight should avoid very small terminal nodes
# gamma to make it so requires a stronger gain before a split is allowed
# Keeping subsample and colsample_bytree fairly high so the model is less noisy.
grid <- expand.grid(
  learning_rate = c(0.03, 0.05),
  max_depth = 3:5,
  min_child_weight = c(10, 25, 50),
  gamma = c(0.5, 1, 2)
)

nrow(grid) # 54 combinations
# Expected as 2 x 3 x 3 x 3 = 54.

# Object to store the CV result from each grid row
grid_results <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  
  print(i)
  
  params_i <- xgb.params(
    booster = "gbtree",
    objective = "count:poisson",
    eval_metric = "poisson-nloglik",
    tree_method = "hist",   # Faster tree-building method, used here to reduce runtime.
    learning_rate = grid$learning_rate[i],
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    gamma = grid$gamma[i],  # gamma > 0 makes splitting more conservative, which helps interpretability. This is updated to min_child_weight now. 
    subsample = 1.0,
    colsample_bytree = 1.0,
    nthread = nthread_use
  )
  
  cv_i <- xgb.cv(
    params = params_i,
    data = dtrain,
    folds = cv_folds,
    nrounds = 2000,
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  best_iter_i  <- which.min(cv_i$evaluation_log$test_poisson_nloglik_mean)
  best_score_i <- min(cv_i$evaluation_log$test_poisson_nloglik_mean)
  
  grid_results[[i]] <- data.frame(
    learning_rate      = grid$learning_rate[i],
    max_depth          = grid$max_depth[i],
    min_child_weight   = grid$min_child_weight[i],
    gamma              = grid$gamma[i],
    best_nrounds       = best_iter_i,
    cv_poisson_nloglik = best_score_i
  )
}

grid_results <- do.call(rbind, grid_results)
grid_results <- grid_results[order(grid_results$cv_poisson_nloglik), ]

# Best combinations at the top
head(grid_results, 10)

# Single best setting
best_grid_row <- grid_results[1, ]
best_grid_row


xgb_final <- xgboost(
  x = X_train,
  y = train_xgb$ClaimNb,
  objective = "count:poisson",
  eval_metric = "poisson-nloglik",
  tree_method = "hist",
  base_margin = log(train_xgb$Exposure),
  nrounds = 863,
  learning_rate = 0.03,
  max_depth = 5,
  min_child_weight = 25,
  gamma = 0.5,
  subsample = 1.0,
  colsample_bytree = 1.0,
  nthread = max(1, detectCores() - 1),
  verbosity = 1
)


################################################################################
# Test final XGBoost
################################################################################
# Get predictions
train_pred <- predict(xgb_final, X_train, base_margin = log(train_xgb$Exposure))
test_pred <- predict(xgb_final, X_test, base_margin = log(test_xgb$Exposure))


################################################################################
# Compare using the same metric as the GLM
################################################################################
xgb_results <- data.frame(
  X = 3,
  Model = "Final XGBoost",
  Parameters = NA,
  AIC = NA,
  Train_Dev = Poisson.Deviance(train_pred, train_xgb$ClaimNb),
  Test_Dev = Poisson.Deviance(test_pred, test_xgb$ClaimNb)
)

print(xgb_results)



################################################################################
# Saving everything to never have to run this again
################################################################################
final_results <-  read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/FinalGLM/GLM1_v_EnhancedGLM_Comparison.csv")

final_results <- rbind(final_results, xgb_results)
final_results
# write.csv(final_results, file = "R/EnhancedModels/FinalML/ML_vs_GLM_comparison")
# write.csv(best_grid_row, file = "R/EnhancedModels/FinalML/optimised_hyperparameters")
# xgb.save(xgb_final, "R/EnhancedModels/FinalML/EnhancedXGB.ubj")
