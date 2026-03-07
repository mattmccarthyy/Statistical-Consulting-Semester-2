rm(list = ls())

###########################################################################################
# Load required data
###########################################################################################
data_url <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/main/data/freMTPL2freq_raw.csv"
beta_url <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/main/Parameters_NN/beta_neurons_20_V1.csv"
w1_url   <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/main/Parameters_NN/W1_neurons_20_V1.csv"

dat <- read.csv(data_url, stringsAsFactors = FALSE)

# match original factor handling from CASdatasets object
dat$Area     <- factor(dat$Area)
dat$VehGas   <- factor(dat$VehGas)
dat$VehBrand <- factor(dat$VehBrand)
dat$Region   <- factor(dat$Region)

dat$n <- 1
dat$ClaimNb <- pmin(dat$ClaimNb, 4) # correct unreasonable observations
dat$Exposure <- pmin(dat$Exposure, 1) # correct unreasonable observations

str(dat)


###########################################################################################
# Feature pre-processing functions
###########################################################################################
PreProcess.Continuous <- function(var1, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  dat2$X <- as.numeric(dat2$V1)
  dat2$X <- 2 * (dat2$X - min(dat2$X)) / (max(dat2$X) - min(dat2$X)) - 1
  names(dat2)[names(dat2) == "V1"]  <- var1
  names(dat2)[names(dat2) == "X"]   <- paste(var1, "X", sep = "")
  dat2
}

PreProcess.CatDummy <- function(var1, short, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  n2 <- ncol(dat2)
  dat2$X <- as.integer(dat2$V1)
  n0 <- length(unique(dat2$X))
  for (n1 in 2:n0){
    dat2[, paste(short, n1, sep = "")] <- as.integer(dat2$X == n1)
  }
  names(dat2)[names(dat2) == "V1"]  <- var1
  dat2[, c(1:n2, (n2 + 2):ncol(dat2))]
}

Features.PreProcess <- function(dat2){
  dat2 <- PreProcess.Continuous("Area", dat2)
  dat2 <- PreProcess.Continuous("VehPower", dat2)
  
  dat2$VehAge <- pmin(dat2$VehAge, 20)
  dat2 <- PreProcess.Continuous("VehAge", dat2)
  
  dat2$DrivAge <- pmin(dat2$DrivAge, 90)
  dat2 <- PreProcess.Continuous("DrivAge", dat2)
  
  dat2$BonusMalus <- pmin(dat2$BonusMalus, 150)
  dat2 <- PreProcess.Continuous("BonusMalus", dat2)
  
  dat2 <- PreProcess.CatDummy("VehBrand", "Br", dat2)
  
  dat2$VehGasX <- as.integer(dat2$VehGas) - 1.5
  
  dat2$Density <- round(log(dat2$Density), 2)
  dat2 <- PreProcess.Continuous("Density", dat2)
  
  dat2 <- PreProcess.CatDummy("Region", "R", dat2)
  
  dat2
}


###########################################################################################
# Feature pre-processing and building learning and test samples
###########################################################################################

dat2 <- Features.PreProcess(dat)

# use original RNG for exact split reproduction
RNGversion("3.5.0")
set.seed(100)

ll <- sample(c(1:nrow(dat2)), round(0.9 * nrow(dat2)), replace = FALSE)
learn <- dat2[ll, ]
test  <- dat2[setdiff(c(1:nrow(dat2)), ll), ]

(n_l <- nrow(learn))
(n_t <- nrow(test))


###########################################################################################
# Load parameter matrices
###########################################################################################
Read.Parameter.Matrix <- function(url, expected_cols = NULL){
  out <- as.matrix(read.table(file = url, header = FALSE, sep = ","))
  if (!is.null(expected_cols) && ncol(out) != expected_cols){
    out <- as.matrix(read.table(file = url, header = FALSE, sep = ";"))
  }
  out
}


###########################################################################################
# Neural network regression function
###########################################################################################
NN.lambda.regression <- function(W1, beta1, n1, X){
  z1 <- array(1, c(nrow(W1) + 1, n1))
  z1[-1, ] <- tanh(W1 %*% t(X))
  exp(t(beta1) %*% z1)
}


###########################################################################################
# Neural network scoring
###########################################################################################
features <- c(13:ncol(dat2))
(d1 <- length(features))
q1 <- 20

(MLE_hom <- sum(learn$ClaimNb) / sum(learn$Exposure))

Xlearn <- as.matrix(learn[, features])  # design matrix learning sample
Ylearn <- cbind(learn$Exposure, as.numeric(learn$ClaimNb))
Xtest  <- as.matrix(test[, features])   # design matrix test sample
Ytest  <- cbind(test$Exposure, as.numeric(test$ClaimNb))


###########################################################################################
# Load final NN parameters from GitHub
###########################################################################################
beta.0 <- Read.Parameter.Matrix(beta_url, expected_cols = 1)
W1.0   <- Read.Parameter.Matrix(w1_url, expected_cols = d1)

# sanity checks
stopifnot(nrow(beta.0) == q1 + 1)
stopifnot(ncol(beta.0) == 1)
stopifnot(nrow(W1.0) == q1)
stopifnot(ncol(W1.0) == d1)


###########################################################################################
# Fit final model only (no training)
###########################################################################################

learn$fit <- as.numeric(t(NN.lambda.regression(W1.0, beta.0, n_l, Xlearn)))
(Cali1.IS <- 200 * (
  sum(learn$fit * Ylearn[, 1]) -
    sum(Ylearn[, 2]) +
    sum(log((Ylearn[, 2] / (learn$fit * Ylearn[, 1]))^(Ylearn[, 2])))
) / n_l)

test$fit <- as.numeric(t(NN.lambda.regression(W1.0, beta.0, n_t, Xtest)))
(Cali1.OOS <- 200 * (
  sum(test$fit * Ytest[, 1]) -
    sum(Ytest[, 2]) +
    sum(log((Ytest[, 2] / (test$fit * Ytest[, 1]))^(Ytest[, 2])))
) / n_t)


###########################################################################################
# Comparison to paper target
###########################################################################################
cat("\nPaper NN target:\n")
cat("In-sample  : 30.45048\n")
cat("Out-of-sample: 31.58770\n")
# Numbers match :)