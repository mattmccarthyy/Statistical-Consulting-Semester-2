################################################################################
# Create Train/Validation Split
# Splitting paper's "learn" set into train (80%) and validation (20%)
# Need this for model selection decisions, will discuss in EDA
################################################################################
rm(list = ls())

#######################################################################
# Load full learn set
#######################################################################
learn <- read.csv("https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-Semester-2/refs/heads/main/data/train_set.csv")

#######################################################################
# Create 80/20 split
#######################################################################
RNGversion("3.5.0")
set.seed(100)
val_indices <- sample(1:nrow(learn), round(0.2 * nrow(learn)), replace = FALSE)

validation <- learn[val_indices, ]
train <- learn[-val_indices, ]

#######################################################################
# Check frequencies match (very roughly)
#######################################################################
## For TRAIN set
print(paste("Policies:", nrow(train)))
print(paste("Exposure:", round(sum(train$Exposure), 0)))
print(paste("Frequency:", round(100 * sum(train$ClaimNb) / sum(train$Exposure), 2), "%"))

# Repeating above
## For VALIDATION set
print("Validation set:")
print(paste("Policies:", nrow(validation)))
print(paste("Exposure:", round(sum(validation$Exposure), 0)))
print(paste("Frequency:", round(100 * sum(validation$ClaimNb) / sum(validation$Exposure), 2), "%"))

# All within 0.4% (know test is 10.41% from paper - is this cheating ahahah?)



#######################################################################
# Save to EnhancedGLM/data folder
#######################################################################
write.csv(train, file = "R/EnhancedGLM/data/train.csv", row.names = FALSE)
write.csv(validation, file = "R/EnhancedGLM/data/validation.csv", row.names = FALSE)
# These are what I'll use throughout EDA.