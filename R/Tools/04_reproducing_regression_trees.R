rm(list = ls())

# Using same randomisation as authors.
RNGversion("3.5.0")
set.seed(100)

################################################################################
# Necessary Libraries
################################################################################
library(rpart)
library(rpart.plot)

################################################################################
# Load in Data
################################################################################
learn <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")
test <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/test_set.csv")


################################################################################
#  Regression Tree Fitting (Model RT1)
################################################################################
## Model RT1
tree1 <- rpart(cbind(Exposure,ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, 
               data = learn, 
               method="poisson",
               control = rpart.control(xval = 1, 
                                       minbucket = 10000, 
                                       cp = 0.0005)
)
tree1
printcp(tree1)
rpart.plot(tree1, 
           digits = 2,# To match their exact formatting from paper 
           roundint = FALSE,
           fallen.leaves = TRUE,
           tweak = 1.05)
tree1$splits # Tree matches exactly, minor labelling inconsistencies on plot.

##############################################
# Losses to confirm we have the right model
##############################################
predL <- predict(tree1, newdata = learn) * learn$Exposure
predT <- predict(tree1, newdata = test) * test$Exposure

loss_learn <- 2 * sum(ifelse(
  learn$ClaimNb > 0, learn$ClaimNb * log(learn$ClaimNb / predL), 0
) - (learn$ClaimNb - predL)) / nrow(learn) * 100

loss_test <- 2 * sum(ifelse(
  test$ClaimNb > 0, test$ClaimNb * log(test$ClaimNb / predT), 0
) - (test$ClaimNb - predT)) / nrow(test) * 100

c(loss_learn, loss_test)

##############################################
# No. Parameters for Table
##############################################
num_params <- sum(tree1$frame$var == "<leaf>"); num_params

################################################################################
#  Regression Tree Fitting (Model RT2) (10-Fold CV + min.cv rule)
################################################################################
# Setting RNG again for peace of mind
RNGversion("3.5.0")
set.seed(100)

K <- 10

# Grow a large CV tree as authors' did
tree2_full <- rpart(
  cbind(Exposure, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region,
  data = learn,
  method = "poisson",
  control = rpart.control(xval = K, 
                          minbucket = 10000, 
                          cp = 1e-5)
)
printcp(tree2_full)

# Pick cp using the "min CV rule", using rpart's CV table
cp_minCV <- tree2_full$cptable[ which.min(tree2_full$cptable[, "xerror"]), "CP" ]

# Prune to cp above, giving RT2 from paper.
tree2 <- prune(tree2_full, cp = cp_minCV)

tree2
printcp(tree2)
rpart.plot(tree2, digits = 2, roundint = FALSE)

##############################################
# Losses to confirm we have the right model
##############################################
predL <- predict(tree2, newdata = learn) * learn$Exposure
predT <- predict(tree2, newdata = test) * test$Exposure

loss_learn <- 2 * sum(ifelse(
  learn$ClaimNb > 0, learn$ClaimNb * log(learn$ClaimNb / predL), 0
) - (learn$ClaimNb - predL)) / nrow(learn) * 100

loss_test <- 2 * sum(ifelse(
  test$ClaimNb > 0, test$ClaimNb * log(test$ClaimNb / predT), 0
) - (test$ClaimNb - predT)) / nrow(test) * 100

c(loss_learn, loss_test)

##############################################
# No. Parameters for Table
##############################################
num_params <- sum(tree2$frame$var == "<leaf>"); num_params # This is 34, not 33, indicating possible issue with cp.
# Re-fitting RT2 using some of their notes to see if I can reproduce that way

#################################################
# Updating Fit for RT2
#################################################
cp_tab <- tree2_full$cptable

target_nsplit <- 32 # Paper has 33 parameters => 32 splits

cp_target <- cp_tab[cp_tab[, "nsplit"] == target_nsplit, "CP"][1]

tree2_paper <- prune(tree2_full, cp = cp_target)

# Confirming correct parameters
sum(tree2_paper$frame$var == "<leaf>") # should be 33 according to authors.

##############################################
# Losses to confirm we have the right model
##############################################
predL <- predict(tree2_paper, newdata = learn) * learn$Exposure
predT <- predict(tree2_paper, newdata = test) * test$Exposure

loss_learn <- 2 * sum(ifelse(
  learn$ClaimNb > 0, learn$ClaimNb * log(learn$ClaimNb / predL), 0
) - (learn$ClaimNb - predL)) / nrow(learn) * 100

loss_test <- 2 * sum(ifelse(
  test$ClaimNb > 0, test$ClaimNb * log(test$ClaimNb / predT), 0
) - (test$ClaimNb - predT)) / nrow(test) * 100

c(loss_learn, loss_test)
# Concluding that they misreported #params for RT2 in Table 6.
# This specification does not return the same loss values. 


################################################################################
#  Regression Tree Fitting (Model RT3) (using paper's 1-SD rule choice)
################################################################################
# Setting RNG again for peace of mind
RNGversion("3.5.0")
set.seed(100)

tree3 <- prune(tree2_full, cp = 0.003)

tree3
printcp(tree3)
rpart.plot(tree3, digits = 3, roundint = FALSE)

##############################################
# Losses to confirm we have the right model
##############################################
predL <- predict(tree3, newdata = learn) * learn$Exposure
predT <- predict(tree3, newdata = test) * test$Exposure

loss_learn <- 2 * sum(ifelse(
  learn$ClaimNb > 0, learn$ClaimNb * log(learn$ClaimNb / predL), 0
) - (learn$ClaimNb - predL)) / nrow(learn) * 100

loss_test <- 2 * sum(ifelse(
  test$ClaimNb > 0, test$ClaimNb * log(test$ClaimNb / predT), 0
) - (test$ClaimNb - predT)) / nrow(test) * 100

c(loss_learn, loss_test)

##############################################
# No. Parameters for Table
##############################################
num_params <- sum(tree3$frame$var == "<leaf>"); num_params



