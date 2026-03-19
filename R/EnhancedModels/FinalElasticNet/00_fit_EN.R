################################################################################
# Fitting and testing Elastic Net Poisson GLM
################################################################################
rm(list = ls())
options(timeout = 600)

library(splines)
library(glmnet)
library(Matrix)

# Comparison metrics
Poisson.Deviance <- function(pred, obs) {
  200 * (sum(pred) - sum(obs) + sum(log((obs / pred)^(obs)))) / length(pred)
}

################################################################################
# Load train/test data
################################################################################
# Same script as 00_fit_enhanced_glm up until the fitting stage,
# so just loading in pre-processed data
# Need to do the spline before fitting now though
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
# Only pre-processing is for DrivAge
################################################################################
# 4). DrivAge spline basis fixed on train, then applied to test
drivage_ns_train <- ns(train$DrivAge, df = 5)
colnames(drivage_ns_train) <- paste0("DrivAge_ns", 1:5)
train <- cbind(train, as.data.frame(drivage_ns_train))

drivage_ns_test <- predict(drivage_ns_train, newx = test$DrivAge)
colnames(drivage_ns_test) <- paste0("DrivAge_ns", 1:5)
test <- cbind(test, as.data.frame(drivage_ns_test))



################################################################################
# Build design matrices for glmnet
################################################################################
x_formula <- ~ AreaGLM + VehPowerGLM + VehAge_3grp +
  DrivAge_ns1 + DrivAge_ns2 + DrivAge_ns3 + DrivAge_ns4 + DrivAge_ns5 +
  BM_is50 + BM_above50 + BM_above100 +
  VehBrand + VehGas + DensityGLM + Region +
  VehAge_3grp:VehGas + VehAge_3grp:VehPowerGLM + B12_only:Region

x_train <- sparse.model.matrix(x_formula, data = train)[, -1]
x_test <- sparse.model.matrix(x_formula, data = test)[, -1]

y_train <- train$ClaimNb

# offset_train <- log(train$Exposure)
# offset_test <- log(test$Exposure)



################################################################################
# Fit Elastic Net Poisson GLM
################################################################################
alpha_enet <- 0.5   # 0 = ridge, 1 = lasso, 0.5 = elastic net
set.seed(1106)

final_enet <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "poisson",
  alpha = alpha_enet,
  offset = log(train$Exposure),
  type.measure = "deviance",
  nfolds = 10,
  standardize = FALSE
)

################################################################################
# Evaluate Elastic Net Poisson GLM
################################################################################
train_pred <- as.numeric(
  predict(
    final_enet,
    newx = x_train,
    s = "lambda.min",
    type = "response",
    newoffset = log(train$Exposure)
  )
)

test_pred <- as.numeric(
  predict(
    final_enet,
    newx = x_test,
    s = "lambda.min",
    type = "response",
    newoffset = log(test$Exposure)
  )
)

enet_coef <- coef(final_enet, s = "lambda.min")
enet_npar <- sum(as.numeric(enet_coef) != 0) # includes intercept

final_en_results <- data.frame(
  X = 3,
  Model = "Elastic Net Poisson GLM",
  Parameters = enet_npar,
  AIC = NA,
  Train_Dev = Poisson.Deviance(train_pred, train$ClaimNb),
  Test_Dev = Poisson.Deviance(test_pred, test$ClaimNb),
  Alpha = alpha_enet,
  Lambda = final_enet$lambda.min
)

final_results <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/FinalGLM/FinalGLM_vs_GLM1_comparison.csv")
final_results$Alpha <- NA
final_results$Lambda <- NA

all_results <- rbind(final_results, final_en_results)
all_results


################################################################################
# Saving everything
################################################################################
write.csv(all_results, file = "R/EnhancedModels/FinalElasticNet/EN_vs_GLM's_comparison.csv")
