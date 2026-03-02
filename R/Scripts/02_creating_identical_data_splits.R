rm(list = ls())

options(timeout = 200) # Accounting for slow internet



################################################################################
# Load in data
################################################################################
freMTPL2freq_raw <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/freMTPL2freq_raw.csv")



################################################################################
# Pre-Processing Outlined in Paper
################################################################################
freMTPL2freq_raw$Exposure <- pmin(freMTPL2freq_raw$Exposure, 1) # Cap Exposure at 1 year
freMTPL2freq_raw$ClaimNb <- pmin(freMTPL2freq_raw$ClaimNb, 4) # Cap ClaimNb at 4.



################################################################################
# Reproducing Data Split (From Paper)
################################################################################
# Revert RNG to version used in the paper for reproducibility
RNGversion("3.5.0")
set.seed(100)

# 90/10 Learning (D)/ Test (T) Split
indices <- sample(c(1:nrow(freMTPL2freq_raw)), round(0.9 * nrow(freMTPL2freq_raw)), replace = FALSE)
train <- freMTPL2freq_raw[indices,]
test <- freMTPL2freq_raw[-indices,]



################################################################################
# Ensuring Data Identical
################################################################################
freq_train <- sum(train$ClaimNb) / sum(train$Exposure)
freq_test <- sum(test$ClaimNb) / sum(test$Exposure)
# Both identical to that used in the paper, concluding these are correct.



################################################################################
# Saving both train and Test Set for Easy Loading
################################################################################
write.csv(train, file = "data/train_set.csv", row.names = FALSE)
write.csv(test, file = "data/test_set.csv", row.names = FALSE)



