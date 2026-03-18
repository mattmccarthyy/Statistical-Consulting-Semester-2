rm(list = ls())
options(timeout = 600)

################################################################################
# Libraries
################################################################################
library(rpart)

################################################################################
# Load data
################################################################################
learn <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test  <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/learn.glm.RDS"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
learn.GLM <- readRDS(f)

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/test.glm.RDS"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
test.GLM <- readRDS(f)

rm(u, f)

n_l <- nrow(learn)
n_t <- nrow(test)

Poisson.Deviance <- function(pred, obs){
  200 * (sum(pred) - sum(obs) + sum(log((obs/pred)^(obs)))) / length(pred)
}

################################################################################
# Load existing models (GLM1, RT2, RT1000)
################################################################################
u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/GLMs/GLM1_full_model.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
d.glm1 <- readRDS(f)

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/RegressionTrees/RT2_stripped.rds"
f2 <- tempfile(fileext = ".rds")
download.file(u, f2, mode = "wb")
RT2 <- readRDS(f2)

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/RegressionTrees/RT1000_stripped.rds"
f3 <- tempfile(fileext = ".rds")
download.file(u, f3, mode = "wb")
RT1000 <- readRDS(f3)

rm(u, f, f2, f3)

################################################################################
# GLM1 fitted means (needed for GLMBoost + GLM1 row in tables)
################################################################################
learn.GLM$fit <- predict(d.glm1, newdata = learn.GLM, type = "response")
test.GLM$fit  <- predict(d.glm1, newdata = test.GLM,  type = "response")

glm1_in  <- Poisson.Deviance(learn.GLM$fit, learn.GLM$ClaimNb)
glm1_out <- Poisson.Deviance(test.GLM$fit,  test.GLM$ClaimNb)

################################################################################
# Minimal runners (PBM / GLMBoost / Shrinkage)
################################################################################
run_pbm <- function(J, M, learn, test, start_learn, start_test, minbucket = 10000, cp = 1e-5){
  learn$fit0 <- start_learn
  test$fit0  <- start_test
  for (m in 1:M){
    PBM.1 <- rpart(
      cbind(fit0, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus +
        VehBrand + VehGas + Density + Region,
      data = learn, method = "poisson",
      control = rpart.control(maxdepth = J, maxsurrogate = 0, xval = 1,
                              minbucket = minbucket, cp = cp)
    )
    learn$fit0 <- learn$fit0 * predict(PBM.1)
    test$fit0  <- test$fit0  * predict(PBM.1, newdata = test)
  }
  c(in_loss = Poisson.Deviance(learn$fit0, learn$ClaimNb),
    out_loss = Poisson.Deviance(test$fit0,  test$ClaimNb))
}

run_shrink <- function(nu, learn, test, minbucket = 10000, cp = 1e-5){
  J <- 3; M <- 50
  learn$fit0 <- learn$Exposure
  test$fit0  <- test$Exposure
  
  PBM.1 <- rpart(
    cbind(fit0, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus +
      VehBrand + VehGas + Density + Region,
    data = learn, method = "poisson",
    control = rpart.control(maxdepth = J, maxsurrogate = 0, xval = 1,
                            minbucket = minbucket, cp = cp)
  )
  learn$fit0 <- learn$fit0 * predict(PBM.1)
  test$fit0  <- test$fit0  * predict(PBM.1, newdata = test)
  
  for (m in 2:M){
    PBM.1 <- rpart(
      cbind(fit0, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus +
        VehBrand + VehGas + Density + Region,
      data = learn, method = "poisson",
      control = rpart.control(maxdepth = J, maxsurrogate = 0, xval = 1,
                              minbucket = minbucket, cp = cp)
    )
    learn$fit0 <- learn$fit0 * (predict(PBM.1) ^ nu)
    test$fit0  <- test$fit0  * (predict(PBM.1, newdata = test) ^ nu)
  }
  
  c(in_loss = Poisson.Deviance(learn$fit0, learn$ClaimNb),
    out_loss = Poisson.Deviance(test$fit0,  test$ClaimNb))
}

################################################################################
# RT losses (use explicit newdata)
################################################################################
rt2_in  <- Poisson.Deviance(learn$Exposure * predict(RT2, newdata = learn), learn$ClaimNb)
rt2_out <- Poisson.Deviance(test$Exposure  * predict(RT2, newdata = test),  test$ClaimNb)

rt1000_in  <- Poisson.Deviance(learn$Exposure * predict(RT1000, newdata = learn), learn$ClaimNb)
rt1000_out <- Poisson.Deviance(test$Exposure  * predict(RT1000, newdata = test),  test$ClaimNb)

################################################################################
# Table 8
################################################################################
pbm1 <- run_pbm(J = 1, M = 30, learn = learn, test = test,
                start_learn = learn$Exposure, start_test = test$Exposure)

pbm2 <- run_pbm(J = 2, M = 50, learn = learn, test = test,
                start_learn = learn$Exposure, start_test = test$Exposure)

pbm3 <- run_pbm(J = 3, M = 50, learn = learn, test = test,
                start_learn = learn$Exposure, start_test = test$Exposure)

Table8 <- data.frame(
  Model = c("PBM1 (J=1,M=30)", "PBM2 (J=2,M=50)", "PBM3 (J=3,M=50)",
            "RT2 (minbucket=10000)", "RT 1000 (minbucket=1000)", "GLM1"),
  InSample_1e2  = round(c(pbm1["in_loss"], pbm2["in_loss"], pbm3["in_loss"],
                          rt2_in, rt1000_in, glm1_in), 5),
  OutSample_1e2 = round(c(pbm1["out_loss"], pbm2["out_loss"], pbm3["out_loss"],
                          rt2_out, rt1000_out, glm1_out), 5)
)
print(Table8, row.names = FALSE)

################################################################################
# Table 9 (Shrinkage)
################################################################################
sh075 <- run_shrink(nu = 0.75, learn = learn, test = test)
sh050 <- run_shrink(nu = 0.50, learn = learn, test = test)

Table9 <- data.frame(
  Model = c("PBM3 (nu=1)", "Shrinkage (nu=0.75)", "Shrinkage (nu=0.50)"),
  InSample_1e2  = round(c(pbm3["in_loss"],  sh075["in_loss"],  sh050["in_loss"]), 5),
  OutSample_1e2 = round(c(pbm3["out_loss"], sh075["out_loss"], sh050["out_loss"]), 5)
)
print(Table9, row.names = FALSE)

################################################################################
# Table 10 (GLMBoost)
################################################################################
glmboost <- run_pbm(J = 3, M = 50, learn = learn.GLM, test = test.GLM,
                    start_learn = learn.GLM$fit, start_test = test.GLM$fit)

Table10 <- data.frame(
  Model = c("GLMBoost", "PBM3", "RT2", "RT 1000", "GLM1"),
  InSample_1e2  = round(c(glmboost["in_loss"], pbm3["in_loss"], rt2_in, rt1000_in, glm1_in), 5),
  OutSample_1e2 = round(c(glmboost["out_loss"], pbm3["out_loss"], rt2_out, rt1000_out, glm1_out), 5)
)
print(Table10, row.names = FALSE)

