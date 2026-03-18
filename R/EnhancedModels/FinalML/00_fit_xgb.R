rm(list = ls())

set.seed(100)

################################################################################
# Required packages
################################################################################
library(xgboost)
library(Matrix)
library(parallel) # Only for detectCores()



################################################################################
# Same data used to train and test GLM1
################################################################################
train <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test  <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")

# Comparison metrics
Poisson.Deviance <- function(pred, obs){
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}

################################################################################
# XGBoost Poisson grid search
################################################################################
xgb_vars <- c(
  "AreaGLM",
  "VehPowerGLM",
  "VehAgeCap",
  "DrivAge",
  "BonusMalusCap",
  "VehBrand",
  "VehGas",
  "DensityGLM",
  "Region"
)

xgb_all <- rbind(
  train[, xgb_vars, drop = FALSE],
  test[,  xgb_vars, drop = FALSE]
)

X_all <- sparse.model.matrix(~ . - 1, data = xgb_all)

n_train <- nrow(train)

X_train <- X_all[1:n_train, ]
X_test  <- X_all[(n_train + 1):nrow(X_all), ]

dtrain <- xgb.DMatrix(
  data = X_train,
  label = train$ClaimNb,
  base_margin = train$logExposure
)

dtest <- xgb.DMatrix(
  data = X_test,
  label = test$ClaimNb,
  base_margin = test$logExposure
)

K <- 5
fold_id <- sample(rep(1:K, length.out = nrow(X_train)))
folds <- lapply(1:K, function(k) which(fold_id == k))

xgb_grid <- expand.grid(
  eta = c(0.05, 0.10),
  max_depth = c(4, 5),
  min_child_weight = c(150, 250),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

cv_results <- vector("list", nrow(xgb_grid))

for (i in seq_len(nrow(xgb_grid))) {
  params <- list(
    objective = "count:poisson",
    eval_metric = "poisson-nloglik",
    booster = "gbtree",
    tree_method = "hist",
    grow_policy = "depthwise",
    eta = xgb_grid$eta[i],
    max_depth = xgb_grid$max_depth[i],
    min_child_weight = xgb_grid$min_child_weight[i],
    subsample = 1.0,
    colsample_bytree = 0.8,
    gamma = 0,
    lambda = 5,
    alpha = 0,
    max_delta_step = 1,
    verbosity = 0,
    nthread = max(1, detectCores(logical = FALSE) - 1),
    seed = 100 + i
  )
  
  cv_fit <- xgb.cv(
    params = params,
    data = dtrain,
    folds = folds,
    nrounds = 400,
    early_stopping_rounds = 25,
    verbose = 0
  )
  
  metric_name <- names(cv_fit$evaluation_log)[
    grepl("^test.*poisson[-_]nloglik.*mean$", names(cv_fit$evaluation_log))
  ][1]
  
  best_idx <- which.min(cv_fit$evaluation_log[[metric_name]])
  
  cv_results[[i]] <- data.frame(
    eta = xgb_grid$eta[i],
    max_depth = xgb_grid$max_depth[i],
    min_child_weight = xgb_grid$min_child_weight[i],
    best_nrounds = cv_fit$evaluation_log$iter[best_idx],
    cv_poisson_nloglik = cv_fit$evaluation_log[[metric_name]][best_idx]
  )
}

cv_results <- do.call(rbind, cv_results)
cv_results <- cv_results[order(cv_results$cv_poisson_nloglik), ]
row.names(cv_results) <- NULL

best_row <- cv_results[1, ]

best_params <- list(
  objective = "count:poisson",
  eval_metric = "poisson-nloglik",
  booster = "gbtree",
  tree_method = "hist",
  grow_policy = "depthwise",
  eta = best_row$eta,
  max_depth = best_row$max_depth,
  min_child_weight = best_row$min_child_weight,
  subsample = 1.0,
  colsample_bytree = 0.8,
  gamma = 0,
  lambda = 5,
  alpha = 0,
  max_delta_step = 1,
  verbosity = 1,
  nthread = max(1, parallel::detectCores(logical = FALSE) - 1L),
  seed = 67
)

final_xgb <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_row$best_nrounds,
  verbose = 0
)

xgb_train_pred <- predict(final_xgb, newdata = dtrain)
xgb_test_pred  <- predict(final_xgb, newdata = dtest)

xgb_results <- data.frame(
  X = 3, # May change index later.
  Model = "XGBoost Poisson",
  Parameters = NA,
  AIC = NA,
  Train_Dev = Poisson.Deviance(xgb_train_pred, train$ClaimNb),
  Test_Dev = Poisson.Deviance(xgb_test_pred, test$ClaimNb)
)


# Need to load in final
final_results <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/FinalGLM/GLM1_v_EnhancedGLM_Comparison",
                          header = TRUE)
xgb_results

comparison_all <- rbind(final_results, xgb_results)
comparison_all



################################################################################
# Saving everything so I never have to run this god awful script again
################################################################################
write.csv(comparison_all, file = "R/EnhancedModels/FinalML/ML_vs_GLM_Comparison.csv")
saveRDS(final_xgb, file = "R/EnhancedModels/FinalML/EnhancedXGB.rds")
write.csv(best_params, file = "R/EnhancedModels/FinalML/XGB_parameters.csv")

# Using ubj as well in case.
xgb.save(final_xgb, "R/EnhancedModels/FinalML/EnhancedXGB_LTS.ubj")
