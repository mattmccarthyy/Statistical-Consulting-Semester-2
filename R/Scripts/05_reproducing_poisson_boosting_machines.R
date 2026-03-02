rm(list = ls())
options(timeout = 600) # Wifi can't load in data within 60s default window.

################################################################################
# Load Required Package 
################################################################################
library(rpart)



################################################################################
# Load required data
################################################################################
learn <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/test_set.csv")

# Loading as RDS in an attempt to bypass factor specification bugs. ADD COMMENT IF IT WORKS.
u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/learn.glm.RDS"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
learn.GLM <- readRDS(f)

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/test.glm.RDS"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
test.GLM <- readRDS(f)

train <- learn.GLM # Have to do this when using model.frame() testing GLM1. Same dataset, renamed to align with paper convention.

# Remove temp objects
rm(u, f)


# They defined in Tools, I didn't look ahead as far so defining here
n_l <- nrow(learn)
n_t <- nrow(test)

# As above
Poisson.Deviance <- function(pred, obs){
  200 * (sum(pred) - sum(obs) + sum(log((obs/pred)^(obs)) ) ) / length(pred)
}



################################################################################
# Poisson Boosting Machines
################################################################################
### Model BM1/2/3
J0 <- 2 #depth of tree
M0 <- 50 #iterations

learn.GLM$fit <- predict(d.glm1, newdata = learn.GLM, type = "response")
test.GLM$fit  <- predict(d.glm1, newdata = test.GLM,  type = "response")

learn.GLM$fit0 <- learn.GLM$fit
test.GLM$fit0  <- test.GLM$fit

{t1 <- proc.time()
  for (m in 1:M0){
    PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, 
                   data = learn, method = "poisson",
                   control = rpart.control(maxdepth = J0, maxsurrogate = 0, xval = 1, minbucket = 10000, cp = 0.00001))   
    # maxsurrogate set max number of backup bariables the model calculates to handle missing data at each node
    # 0 gives faster training, and no msising values here so safe to run
    # Note: verified no NA's in initial data scripts. 
    learn$fit0 <- learn$fit0 * predict(PBM.1)
    learn[,paste("PBM_",m, sep="")] <-  learn$fit0
    test$fit0 <- test$fit0 * predict(PBM.1, newdata = test)
    test[,paste("PBM_", m, sep = "")] <-  test$fit0
  }
  (proc.time()-t1)[3]}


losses <- array(NA, c(2, M0))

for (m in 1:M0){
  losses[1,m] <- 200 * (sum(learn[ , paste("PBM_", m, sep = "")]) - sum(learn$ClaimNb) + sum(log((learn$ClaimNb / learn[,paste("PBM_", m, sep = "")])^(learn$ClaimNb)))) / n_l
  losses[2,m] <- 200 * (sum(test[ ,paste("PBM_", m, sep = "")]) - sum(test$ClaimNb) + sum(log((test$ClaimNb / test[ , paste("PBM_", m, sep = "")])^(test$ClaimNb)))) / n_t
}

losses[ ,M0]       

plot(x=c(0:M0), y=c(32.93518, losses[1,]), type='l', col="red", ylim=c(30,33.5), xlab="number of iterations", ylab="average in-sample loss (in 10^(-2))", main=paste("decrease of in-sample loss (depth=", J0,")", sep=""))
points(x=c(0:M0), y=c(32.93518, losses[1,]), pch=19, col="red")
abline(h=c(30.70841), col="blue", lty=2)
abline(h=c(31.26738), col="green", lty=2)
J1 <- J0
legend(x="topright", col=c("red", "blue", "green"), lty=c(1,2,2), lwd=c(1,1,1), pch=c(19,-1,-1), legend=c(paste("Model PBM", J1, sep=""), "Model RT2", "Model GLM1"))


plot(x=c(1:M0), y=losses[2,], type='l', lwd=2, col="red", ylim=c(30.5,33.5), xlab="number of iterations", ylab="average out-of-sample loss (in 10^(-2))", main="decrease of out-of-sample loss")
abline(h=c(32.17123), col="green", lty=2)
legend(x="topright", col=c("red", "green"), lty=c(1,2), lwd=c(1,1), pch=c(19,-1), legend=c(paste("Model PBM", J0, sep=""), "Model GLM1"))



################################################################################
# GLM Boost
################################################################################
### Model GLM1
# Loading in from GitHub
u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/GLMs/GLM1_full_model.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
d.glm1 <- readRDS(f)

# Remove temp objects
rm(u, f)

learn.GLM$fit <- predict(d.glm1, newdata = learn.GLM, type = "response")
test.GLM$fit  <- predict(d.glm1, newdata = test.GLM,  type = "response")
c(Poisson.Deviance(learn.GLM$fit, learn.GLM$ClaimNb), Poisson.Deviance(test.GLM$fit, test.GLM$ClaimNb))



### Model GLMBoost
J0 <- 3 # tree depth
M0 <- 50 # iterations

learn.GLM$fit0 <- learn.GLM$fit
test.GLM$fit0 <- test.GLM$fit

{t1 <- proc.time()
  for (m in 1:M0){
    PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, 
                   data=learn.GLM, method="poisson",
                   control=rpart.control(maxdepth=J0, maxsurrogate=0, xval=1, minbucket=10000, cp=0.00001))     
    learn.GLM$fit0 <- learn.GLM$fit0 * predict(PBM.1)
    learn.GLM[,paste("PBM_",m, sep="")] <-  learn.GLM$fit0
    test.GLM$fit0 <- test.GLM$fit0 * predict(PBM.1, newdata=test.GLM)
    test.GLM[,paste("PBM_",m, sep="")] <-  test.GLM$fit0
  }
  (proc.time()-t1)[3]}

losses <- array(NA, c(2,M0))

for (m in 1:M0){
  losses[1,m] <- 200 * (sum(learn.GLM[ , paste("PBM_",m , sep = "")])-sum(learn.GLM$ClaimNb)+sum(log((learn.GLM$ClaimNb/learn.GLM[,paste("PBM_",m, sep="")])^(learn.GLM$ClaimNb))))/n_l
  losses[2,m] <- 200 * (sum(test.GLM[ , paste("PBM_", m, sep = "")])-sum(test.GLM$ClaimNb)+sum(log((test.GLM$ClaimNb/test.GLM[,paste("PBM_",m, sep="")])^(test.GLM$ClaimNb))))/n_t
}

losses[ ,M0]       

plot(x=c(0:M0), y=c(31.26738, losses[1,]), type='l', col="magenta", ylim=c(30,32), xlab="number of iterations", ylab="average in-sample loss (in 10^(-2))", main=paste("GLM Boost: decrease of in-sample loss (depth=", J0,")", sep=""))
points(x=c(0:M0), y=c(31.26738, losses[1,]), pch=19, col="magenta")
abline(h=c(30.13151), col="red", lty=2)
abline(h=c(30.70841), col="blue", lty=2)
abline(h=c(31.26738), col="green", lty=2)
J1 <- J0
legend(x="topright", col=c("magenta", "red", "blue", "green"), lty=c(1,2,2,2), lwd=c(1,1,1,1), pch=c(19,-1,-1,-1), legend=c(paste("Model GLMBoost", sep=""), "Model PBM3","Model RT2", "Model GLM1"))


plot(x=c(1:M0), y=losses[2,], type='l', lwd=2, col="magenta", ylim=c(30.5,33.5), xlab="number of iterations", ylab="average out-of-sample loss (in 10^(-2))", main="decrease of out-of-sample loss")
abline(h=c(31.46842), col="red", lty=2)
abline(h=c(32.17123), col="green", lty=2)
legend(x="topright", col=c("magenta", "red", "green"), lty=c(1,2,2), lwd=c(1,1,1), pch=c(-1,-1,-1), legend=c(paste("Model GLMBoost", sep=""), "Model PBM3", "Model GLM1"))



################################################################################
# Shrinkage Portion
################################################################################
### Model BM3

J0 <- 3 #depth of tree
M0 <- 50 #iterations
nu <- .75
minbucket0 <- 10000

learn$fit0 <- learn$Exposure
test$fit0  <- test$Exposure

{ m <- 1            
  PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, 
                 data=learn, method="poisson",
                 control=rpart.control(maxdepth=J0, maxsurrogate=0, xval=1, minbucket=minbucket0, cp=0.00001))     
  learn$fit0 <- learn$fit0 * predict(PBM.1)
  learn[,paste("PBM_",m, sep="")] <-  learn$fit0
  test$fit0 <- test$fit0 * predict(PBM.1, newdata=test)
  test[,paste("PBM_",m, sep="")] <-  test$fit0
  for (m in 2:M0){
    PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, 
                   data=learn, method="poisson",
                   control=rpart.control(maxdepth=J0, maxsurrogate=0, xval=1, minbucket=minbucket0, cp=0.00001))     
    learn$fit0 <- learn$fit0 * (predict(PBM.1)^nu)
    learn[,paste("PBM_",m, sep="")] <-  learn$fit0
    test$fit0 <- test$fit0 * (predict(PBM.1, newdata=test)^nu)
    test[,paste("PBM_",m, sep="")] <-  test$fit0
  }
} 

losses <- array(NA, c(2, M0))

for (m in 1:M0){
  losses[1,m] <- 200 * (sum(learn[ , paste("PBM_", m, sep = "")]) - sum(learn$ClaimNb) + sum(log((learn$ClaimNb / learn[ , paste("PBM_", m, sep = "")])^(learn$ClaimNb)))) / n_l
  losses[2,m] <- 200 * (sum(test[ , paste("PBM_", m, sep = "")]) - sum(test$ClaimNb) + sum(log((test$ClaimNb / test[ , paste("PBM_", m, sep = "")])^(test$ClaimNb)))) / n_t
}

losses[ , M0]  



################################################################################
# All tables for report
# Chapter 5 outputs (Figures 17–18, Tables 8–10)
################################################################################
#### Loading in required models for calculations 
# (Above isn't necessary, I just like to generate tables in one go to keep everything understandable to an outsider)
# For RT2
u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/RegressionTrees/RT2_stripped.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
RT2 <- readRDS(f)

# For RT1000
u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/RegressionTrees/RT1000_stripped.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
RT1000 <- readRDS(f)

# Remove temp objects
rm(u, f)


################################################################################
# Minimal runners for PBM, Shrinkage, GLMBoost (same rpart settings as report)
################################################################################
run_pbm <- function(J, M, learn, test, start_learn, start_test){
  learn$fit0 <- start_learn
  test$fit0  <- start_test
  for (m in 1:M){
    PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge +
                     BonusMalus + VehBrand + VehGas + Density + Region,
                   data=learn, method="poisson",
                   control=rpart.control(maxdepth=J, maxsurrogate=0, xval=1, minbucket=10000, cp=0.00001))
    learn$fit0 <- learn$fit0 * predict(PBM.1)
    test$fit0  <- test$fit0  * predict(PBM.1, newdata=test)
  }
  c(in_loss  = Poisson.Deviance(learn$fit0, learn$ClaimNb),
    out_loss = Poisson.Deviance(test$fit0,  test$ClaimNb))
}

run_shrink <- function(nu, learn, test){
  J <- 3; M <- 50
  learn$fit0 <- learn$Exposure
  test$fit0  <- test$Exposure
  
  # m=1 no shrink (matches report code)
  PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge +
                   BonusMalus + VehBrand + VehGas + Density + Region,
                 data=learn, method="poisson",
                 control=rpart.control(maxdepth=J, maxsurrogate=0, xval=1, minbucket=10000, cp=0.00001))
  learn$fit0 <- learn$fit0 * predict(PBM.1)
  test$fit0  <- test$fit0  * predict(PBM.1, newdata=test)
  
  for (m in 2:M){
    PBM.1 <- rpart(cbind(fit0,ClaimNb) ~ Area + VehPower + VehAge + DrivAge +
                     BonusMalus + VehBrand + VehGas + Density + Region,
                   data=learn, method="poisson",
                   control=rpart.control(maxdepth=J, maxsurrogate=0, xval=1, minbucket=10000, cp=0.00001))
    learn$fit0 <- learn$fit0 * (predict(PBM.1)^nu)
    test$fit0  <- test$fit0  * (predict(PBM.1, newdata=test)^nu)
  }
  c(in_loss  = Poisson.Deviance(learn$fit0, learn$ClaimNb),
    out_loss = Poisson.Deviance(test$fit0,  test$ClaimNb))
}

################################################################################
# Table 8: PBM1/PBM2/PBM3 + RT2 + RT1000 + GLM1
################################################################################
pbm1 <- run_pbm(J=1, M=30, learn=learn, test=test, start_learn=learn$Exposure, start_test=test$Exposure)
pbm2 <- run_pbm(J=2, M=50, learn=learn, test=test, start_learn=learn$Exposure, start_test=test$Exposure)
pbm3 <- run_pbm(J=3, M=50, learn=learn, test=test, start_learn=learn$Exposure, start_test=test$Exposure)

rt2_in  <- Poisson.Deviance(learn$Exposure * predict(RT2), learn$ClaimNb)
rt2_out <- Poisson.Deviance(test$Exposure  * predict(RT2, newdata=test), test$ClaimNb)

rt1000_in  <- Poisson.Deviance(learn$Exposure * predict(RT1000), learn$ClaimNb)
rt1000_out <- Poisson.Deviance(test$Exposure  * predict(RT1000, newdata=test), test$ClaimNb)

glm1_in  <- Poisson.Deviance(learn.GLM$fit, learn.GLM$ClaimNb)
glm1_out <- Poisson.Deviance(test.GLM$fit,  test.GLM$ClaimNb)

Table8 <- data.frame(
  Model=c("PBM1 (J=1,M=30)","PBM2 (J=2,M=50)","PBM3 (J=3,M=50)",
          "RT2 (minbucket=10000)","RT 1000 (minbucket=1000)","GLM1"),
  InSample_1e2 = round(c(pbm1["in_loss"], pbm2["in_loss"], pbm3["in_loss"],
                         rt2_in, rt1000_in, glm1_in), 5),
  OutSample_1e2= round(c(pbm1["out_loss"],pbm2["out_loss"],pbm3["out_loss"],
                         rt2_out, rt1000_out, glm1_out), 5)
)
print(Table8, row.names=FALSE)

################################################################################
# Table 9: Shrinkage (baseline is PBM3 = nu=1)
################################################################################
sh075 <- run_shrink(nu = 0.75, learn = learn, test = test)
sh050 <- run_shrink(nu = 0.50, learn = learn, test = test)

Table9 <- data.frame(
  Model=c("PBM3 (nu = 1)","Shrinkage (nu = 0.75)","Shrinkage (nu = 0.50)"),
  InSample_1e2 = round(c(pbm3["in_loss"], sh075["in_loss"],  sh050["in_loss"]), 5),
  OutSample_1e2= round(c(pbm3["out_loss"], sh075["out_loss"], sh050["out_loss"]), 5)
)
print(Table9, row.names = FALSE)

################################################################################
# Table 10: GLMBoost + PBM3 + RT2 + RT1000 + GLM1
################################################################################
glmboost <- run_pbm(J = 3, M = 50, learn = learn.GLM, test = test.GLM,
                    start_learn = learn.GLM$fit, start_test = test.GLM$fit)

Table10 <- data.frame(
  Model = c("GLMBoost", "PBM3", "RT2", "RT 1000", "GLM1"),
  InSample_1e2 = round(c(glmboost["in_loss"], pbm3["in_loss"], rt2_in, rt1000_in, glm1_in), 5),
  OutSample_1e2 = round(c(glmboost["out_loss"], pbm3["out_loss"], rt2_out, rt1000_out, glm1_out), 5)
)
print(Table10, row.names=FALSE)






