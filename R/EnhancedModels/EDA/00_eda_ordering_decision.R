################################################################################
# Testing which predictors seem most important to order EDA
################################################################################
# Doing this in case don't get time to do every single predictor.
# Focusing on the most high impact first. 
# Screening predictors only on the train set. Will then test them in respecitive scripts on validation set. 
rm(list = ls())



################################################################################
# Load Data
################################################################################
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
str(train)

################################################################################
# Fit univariate GLM for each predictor
################################################################################
predictors <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", 
                "VehBrand", "VehGas", "Area", "Density", "Region")

univariate_results <- data.frame(
  Predictor = predictors,
  AIC = NA,
  Deviance_Reduction = NA
)

# Null model baseline
null_model <- glm(ClaimNb ~ 1, family = poisson(), data = train, offset = log(Exposure))
null_dev <- deviance(null_model)

# Fit each predictor
for(i in 1:length(predictors)) {
  formula <- as.formula(paste("ClaimNb ~", predictors[i]))
  model <- glm(formula, family = poisson(), data = train, offset = log(Exposure))
  
  univariate_results$AIC[i] <- AIC(model)
  univariate_results$Deviance_Reduction[i] <- null_dev - deviance(model)
}

univariate_results <- univariate_results[order(-univariate_results$Deviance_Reduction), ] # Largest to smallest decrease
print(univariate_results)

write.table(univariate_results, "R/EnhancedModels/EDA/00_univariate_results.csv", sep = ",", row.names = TRUE, col.names = NA)

