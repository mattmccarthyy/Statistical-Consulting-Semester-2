################################################################################
# EDA (5): Region
################################################################################
rm(list = ls())

# Load required EDA data
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

# Load train and validate for GLM spec testing
train <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/train.csv")
validate <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/EnhancedModels/data/validation.csv")

# Comparison Metric
Poisson.Deviance <- function(pred, obs){200*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)}



################################################################################
# 1). Frequency by Region
################################################################################
region_analysis <- data.frame(
  Region = sort(unique(learn$Region)),
  Policies = sapply(sort(unique(learn$Region)), function(x) sum(learn$Region == x)),
  Exposure = sapply(sort(unique(learn$Region)), function(x) sum(learn$Exposure[learn$Region == x])),
  Claims = sapply(sort(unique(learn$Region)), function(x) sum(learn$ClaimNb[learn$Region == x]))
)

region_analysis$Frequency <- region_analysis$Claims / region_analysis$Exposure
region_analysis$Pct_Policies <- 100 * region_analysis$Policies / sum(region_analysis$Policies)

# Frequency by Region (ordered by frequency)
print(region_analysis[order(-region_analysis$Frequency), ])

# Summary stats
#Regions with highest frequency
region_analysis$Region[which.max(region_analysis$Frequency)] 
round(max(region_analysis$Frequency), 4)

#Regions with lowest frequency
region_analysis$Region[which.min(region_analysis$Frequency)]
round(min(region_analysis$Frequency), 4)

#Frequency range
round(max(region_analysis$Frequency) - min(region_analysis$Frequency), 4)



################################################################################
# 2). Check for VehBrand x Region interaction (from VehBrand EDA)
################################################################################
# Already saw B12 concentrated in R94 (69.9%), R21 (66.3%), R11 (52.8%)
# Check if these regions have elevated overall frequency (incase interaction is here, will look at this in way more detail later)

high_b12_regions <- c("R94", "R21", "R11", "R83")
# Frequency in high-B12 regions vs others
print(region_analysis[region_analysis$Region %in% high_b12_regions, c("Region", "Frequency", "Pct_Policies")])

# INTERACTION DETECTED (FINALLY SOMETHING OVER THE PAPER LETS GO): VehBrand x Region
# High-B12 regions R94 (13.80%), R11 (13.02%), R21 (12.78%) are TOP 3 for frequency
# But R83 has 54.5% B12 yet only 8.42% frequency, this breaks that pattern
# Suggests B12 rental effect varies by region (different operators? urban density?, loads of content to look into)
# Will need to test VehBrand x Region interaction in final model






################################################################################
# PART 2: GLM Specification Testing for Region
################################################################################
##########################################################
# Specification 1: Paper - All 22 categorical levels
##########################################################
# Paper's used as baseline again: Region as categorical factor with all 22 levels.
spec1 <- glm(ClaimNb ~ Region, family = poisson(), data = train, offset = log(Exposure))
spec1_aic <- AIC(spec1)
spec1_train_dev <- Poisson.Deviance(fitted(spec1), train$ClaimNb)
spec1_val_dev <- Poisson.Deviance(predict(spec1, newdata = validate, type = "response"), validate$ClaimNb)
spec1_params <- length(coef(spec1))



##########################################################
# Specification 2: Frequency Tiers
##########################################################
# Group by observed frequencies: High (>11%: R94,R11,R21,R22,R82), Medium (9-11%), Low (<9%).
# Tests if risk-based grouping captures signal with fewer parameters.
# Doing by eye first, if this is really good might try k-means
train$Region_Freq <- ifelse(train$Region %in% c("R94","R11","R21","R22","R82"), "High",
                            ifelse(train$Region %in% c("R93","R74","R42","R43","R31","R91","R26","R73"), "Medium", "Low"))
validate$Region_Freq <- ifelse(validate$Region %in% c("R94","R11","R21","R22","R82"), "High",
                               ifelse(validate$Region %in% c("R93","R74","R42","R43","R31","R91","R26","R73"), "Medium", "Low"))

spec2 <- glm(ClaimNb ~ Region_Freq, family = poisson(), data = train, offset = log(Exposure))
spec2_aic <- AIC(spec2)
spec2_train_dev <- Poisson.Deviance(fitted(spec2), train$ClaimNb)
spec2_val_dev <- Poisson.Deviance(predict(spec2, newdata = validate, type = "response"), validate$ClaimNb)
spec2_params <- length(coef(spec2))



##########################################################
# Specification 3: High-B12 Regions vs Rest
##########################################################
# VehBrand EDA showed R94,R11,R21 have high B12 concentration AND high frequency.
# Tests if B12-driven regions are distinct (interaction hypothesis).
train$Region_B12 <- ifelse(train$Region %in% c("R94","R11","R21"), "High_B12", "Other")
validate$Region_B12 <- ifelse(validate$Region %in% c("R94","R11","R21"), "High_B12", "Other")

spec3 <- glm(ClaimNb ~ Region_B12, family = poisson(), data = train, offset = log(Exposure))
spec3_aic <- AIC(spec3)
spec3_train_dev <- Poisson.Deviance(fitted(spec3), train$ClaimNb)
spec3_val_dev <- Poisson.Deviance(predict(spec3, newdata = validate, type = "response"), validate$ClaimNb)
spec3_params <- length(coef(spec3))



##########################################################
# Specification 4: Major Region R24 vs Rest
##########################################################
# R24 contains 23.7% of all policies with mid-range frequency (8.93%).
# Tests if volume concentration warrants separate treatment.
train$Region_Vol <- ifelse(train$Region == "R24", "R24", "Other")
validate$Region_Vol <- ifelse(validate$Region == "R24", "R24", "Other")

spec4 <- glm(ClaimNb ~ Region_Vol, family = poisson(), data = train, offset = log(Exposure))
spec4_aic <- AIC(spec4)
spec4_train_dev <- Poisson.Deviance(fitted(spec4), train$ClaimNb)
spec4_val_dev <- Poisson.Deviance(predict(spec4, newdata = validate, type = "response"), validate$ClaimNb)
spec4_params <- length(coef(spec4))



################################################################################
# Comparison Table
################################################################################
comparison <- data.frame(
  Specification = c("Paper: All 22 categorical", "Frequency Tiers", "High-B12 Regions vs Rest", "R24 vs Rest"),
  Params = c(spec1_params, spec2_params, spec3_params, spec4_params),
  AIC = c(spec1_aic, spec2_aic, spec3_aic, spec4_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec3_train_dev, spec4_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec3_val_dev, spec4_val_dev)
)

comparison$Delta_AIC <- comparison$AIC - spec1_aic
print(comparison)
print(comparison[which.min(comparison$Val_Dev), ])

# Finally an interesting one for the report. MAKE SURE TO INCLUDE ALL OF THIS.
# Region main effect optimal at 22 categorical levels,
# However, VehBrand x Region Interaction hyptohesis (B12 concentration in R94/R11/R21 drives elevated frquency) requires a testable parameterisation.
# Grouping would get rid of a lot of terms, so interaction could actually be included (and I am almost certain it has to be).
# Freq grouping done above only loses 0.021 on validation deviance. 
# Grouping is actually going to be needed for parisomy for once.
# And finally a place k-means may actually be optimal
# Time for a billion comments below.



################################################################################
# K-Means Clustering for Region Grouping
################################################################################
# WHY K-MEANS FOR REGION (but not the other predictors):
## 22 regions is too many for VehBrand x Region interaction (would create 66 terms)
## Unlike Area (clear A-F progression) or VehBrand (B12 obvious outlier), regions have no clear structure
## VehBrand EDA revealed interaction hypothesis: B12 concentration varies by region
## Going to try and group regions by "similar B12 sensitivity" not just "similar frequency"

# FEATURES FOR CLUSTERING:
# Going to try cluster on 3 features: Frequency, B12_concentration, log(Volume)
# 1). Frequency (marginal claim rate):
#### Primary risk signal for each region
#### Ranges from 7.63% (R41) to 13.80% (R94)

# 2). B12 concentration (% of policies that are VehBrand B12):
#### Interaction driver - this is why we're doing k-means instead of simple frequency bins
#### R11: 13.02% freq, 52.8% B12 (high risk DRIVEN by rentals)
#### R82: 11.05% freq, 20.9% B12 (high risk from OTHER factors)
#### These should NOT be in same group for interaction purposes

# 3). Log(Volume) (credibility weighting) :
#### Prevents tiny regions (R94: 0.66% of policies) from dominating clusters
#### Ensures groups have sufficient volume for stable estimates
# This idea was taken from Jennings, P. J. (2008). "Using Cluster Analysis for Territory Design." Casualty Actuarial Society Discussion Paper Program.
# Link: https://www.casact.org/sites/default/files/database/dpp_dpp08_08dpp34.pdf


# WHY NOT AREA:
# Area is already a predictor in our model (urbanisation gradient A-F)
# Including Area in Region clustering = double-counting
## Area captures: urbanization, density (applies everywhere)
## Region captures: Paris vs Marseille vs Brittany (local effects, where rental fleets operate)
# Region grouping should capture geographic effects BEYOND what Area explains

# GOAL:
# Create interpretable groups like:
## "High-risk, high-rental regions" (R94, R11, R21) - B12 drives risk
## "Medium-risk, low-rental regions" - risk from other factors
## "Low-risk regions"
# This enables clean VehBrand x Region interaction: effect of B12 varies by region group

# PRACTICAL DECISION:
# Need to balance:
## Model fit (22 categorical is best for main effect)
## Interaction feasibility (3-5 groups needed for parsimony)
## Interpretability (clusters must make business sense)
## Statistical power (groups need sufficient volume)
# K-means provides data-driven grouping that serves all these needs



################################################################################
# K-Means Clustering on [Frequency, B12_pct, log(Volume)]
# Citation above MKAE SURE TO INCLUDE IN REPORT
################################################################################
# Calculate region-level features for clustering
region_features <- data.frame(
  Region = region_analysis$Region,
  Frequency = region_analysis$Frequency,
  B12_pct = sapply(region_analysis$Region, function(r) {
    100 * sum(learn$VehBrand[learn$Region == r] == "B12") / sum(learn$Region == r)
  }),
  Log_Volume = log(region_analysis$Policies)
)

print("Region features for clustering:")
print(region_features[order(-region_features$Frequency), ])

# Standardize features (mean=0, sd=1) for k-means
features_scaled <- scale(region_features[, c("Frequency", "B12_pct", "Log_Volume")])
rownames(features_scaled) <- region_features$Region

# Test k=3 to 10 clusters
set.seed(100)
k_values <- 3:11
within_ss <- numeric(length(k_values))

for(i in 1:length(k_values)) {
  km <- kmeans(features_scaled, centers = k_values[i], nstart = 25)
  within_ss[i] <- km$tot.withinss
}

# Elbow plot
par(mfrow = c(1, 1), 
    mar = c(5.5, 5.5, 3, 1), 
    tcl = -0.25,
    cex.main = 1.5, 
    cex.lab = 1.3, 
    cex.axis = 1.2, 
    col = "black", 
    mgp = c(3.5, 0.7, 0))

plot(k_values, 
     within_ss, 
     type = "b", 
     pch = 19, 
     col = "#8d17f1",
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares",
     main = "K-Means Elbow Plot for Region Clustering", 
     lwd = 2, 
     cex = 1.2)

grid()

# Fit k = 3, 4, 5 for examination
km3 <- kmeans(features_scaled, centers = 3, nstart = 25)
km4 <- kmeans(features_scaled, centers = 4, nstart = 25)
km5 <- kmeans(features_scaled, centers = 5, nstart = 25)

# Add cluster assignments to region_features
region_features$Cluster_k3 <- km3$cluster
region_features$Cluster_k4 <- km4$cluster
region_features$Cluster_k5 <- km5$cluster

# K=3 Cluster Assignments
print(region_features[order(region_features$Cluster_k3), c("Region", "Frequency", "B12_pct", "Log_Volume", "Cluster_k3")])

# K=4 Cluster Assignments
print(region_features[order(region_features$Cluster_k4), c("Region", "Frequency", "B12_pct", "Log_Volume", "Cluster_k4")])

# K = 5 Cluster Assignments
print(region_features[order(region_features$Cluster_k5), c("Region", "Frequency", "B12_pct", "Log_Volume", "Cluster_k5")])



################################################################################
# Compare K-Means Groupings vs Manual Frequency Tiers
################################################################################
# Apply k-means cluster assignments to train and validate sets
# K-means k=3
cluster_map_k3 <- setNames(region_features$Cluster_k3, region_features$Region)
train$Region_KM3 <- factor(cluster_map_k3[train$Region])
validate$Region_KM3 <- factor(cluster_map_k3[validate$Region])

spec_km3 <- glm(ClaimNb ~ Region_KM3, family = poisson(), data = train, offset = log(Exposure))
spec_km3_aic <- AIC(spec_km3)
spec_km3_train_dev <- Poisson.Deviance(fitted(spec_km3), train$ClaimNb)
spec_km3_val_dev <- Poisson.Deviance(predict(spec_km3, newdata = validate, type = "response"), validate$ClaimNb)
spec_km3_params <- length(coef(spec_km3))

# K-means k=4
cluster_map_k4 <- setNames(region_features$Cluster_k4, region_features$Region)
train$Region_KM4 <- factor(cluster_map_k4[train$Region])
validate$Region_KM4 <- factor(cluster_map_k4[validate$Region])

spec_km4 <- glm(ClaimNb ~ Region_KM4, family = poisson(), data = train, offset = log(Exposure))
spec_km4_aic <- AIC(spec_km4)
spec_km4_train_dev <- Poisson.Deviance(fitted(spec_km4), train$ClaimNb)
spec_km4_val_dev <- Poisson.Deviance(predict(spec_km4, newdata = validate, type = "response"), validate$ClaimNb)
spec_km4_params <- length(coef(spec_km4))

# K-means k=5
cluster_map_k5 <- setNames(region_features$Cluster_k5, region_features$Region)
train$Region_KM5 <- factor(cluster_map_k5[train$Region])
validate$Region_KM5 <- factor(cluster_map_k5[validate$Region])

spec_km5 <- glm(ClaimNb ~ Region_KM5, family = poisson(), data = train, offset = log(Exposure))
spec_km5_aic <- AIC(spec_km5)
spec_km5_train_dev <- Poisson.Deviance(fitted(spec_km5), train$ClaimNb)
spec_km5_val_dev <- Poisson.Deviance(predict(spec_km5, newdata = validate, type = "response"), validate$ClaimNb)
spec_km5_params <- length(coef(spec_km5))



################################################################################
# Comparison: Paper vs Manual vs K-Means
################################################################################
comparison_kmeans <- data.frame(
  Specification = c("Paper: All 22 categorical", "Manual: Frequency Tiers", "K-Means k=3", "K-Means k=4", "K-Means k=5"),
  Params = c(spec1_params, spec2_params, spec_km3_params, spec_km4_params, spec_km5_params),
  AIC = c(spec1_aic, spec2_aic, spec_km3_aic, spec_km4_aic, spec_km5_aic),
  Train_Dev = c(spec1_train_dev, spec2_train_dev, spec_km3_train_dev, spec_km4_train_dev, spec_km5_train_dev),
  Val_Dev = c(spec1_val_dev, spec2_val_dev, spec_km3_val_dev, spec_km4_val_dev, spec_km5_val_dev)
)

comparison_kmeans$Delta_AIC <- comparison_kmeans$AIC - spec1_aic

# Paper vs Manual vs K-Means Clustering
print(comparison_kmeans)

# Best specification (lowest validation deviance)
print(comparison_kmeans[which.min(comparison_kmeans$Val_Dev), ])


# Looking at if this actually clustered high B12 regions (R94/R11/R21/R22/R83) as desired
# K=4 Cluster Composition
print(region_features[order(region_features$Cluster_k4), c("Region", "Frequency", "B12_pct", "Cluster_k4")])

# All regions in cluster 3
region_features[region_features$Cluster_k4 == 3, c("Region", "Frequency", "B12_pct", "Log_Volume")]
# This missed R83, but even though R83 has a high B12 % (54.5%), it has a lower frequency (8.42%) so it not being in cluster 3 makes sense!!
# More detail below.

# K-means k=4 Cluster 3 Analysis:
# Cluster 3 contains: R94 (69.9% B12, 13.80% freq), R11 (52.8% B12, 13.02% freq), 
#                     R21 (66.3% B12, 12.78% freq), R22 (42.1% B12, 12.33% freq)
# Cluster 3 EXCLUDES: R83 (54.5% B12, 8.42% freq)

# k-means found a real pattern:
# R83 has high B12 concentration but LOW frequency (8.42% vs 13%+ in cluster 3)
# The rental car (B12) effect drives high frequency in R94/R11/R21/R22 but NOT in R83
# Means can discuss things such as: different rental operators? different urban dynamics? different enforcement?

# K-means identified "regions where B12 predicts high risk" vs "regions where it doesn't"
# This is exactly what I wanted for VehBrand x Region interaction
# Cluster 3 = "high-B12, high-risk rental regions" where interaction will be strongest

# K = 4 clustering seems to capture interaction hypothesis better than manual grouping
# Even though it increases AIC by 14, it should be huge when interaction is included later. 


# FINAL DECISION: Using k- 4 k-means for grouping:
## Performs almost as well as paper
## Performs better than manual groupings
## Uses only 4 parameters (instead of papers 18)
## Is incredibly interpretable, have plenty to talk about here.
## Is a strategic trade-off (finally some fun), higher AIC now, but interaction should perform so much better. 



################################################################################
# K-Means k=4 Cluster Lookup Table
################################################################################
# Create lookup table sorted by cluster
cluster_lookup <- region_features[order(region_features$Cluster_k4), 
                                  c("Region", "Frequency", "B12_pct", "Cluster_k4")]

#K-Means k=4 Cluster Assignments
print(cluster_lookup)

# Save to CSV for reference in Final GLM 
write.csv(cluster_lookup, "R/EnhancedModels/LookupKeys/region_kmeans_k4_lookup.csv", row.names = FALSE)

# Cluster Summaries
for(i in 1:4) {
  cluster_regions <- cluster_lookup$Region[cluster_lookup$Cluster_k4 == i]
  avg_freq <- mean(cluster_lookup$Frequency[cluster_lookup$Cluster_k4 == i])
  avg_b12 <- mean(cluster_lookup$B12_pct[cluster_lookup$Cluster_k4 == i])
  
  print(paste("Cluster", i, "- Regions:", paste(cluster_regions, collapse=", ")))
  print(paste("  Avg Frequency:", round(avg_freq, 4), "| Avg B12%:", round(avg_b12, 1)))
}


################################################################################
# Save Essential Plots for Report
################################################################################
# Plot 1: Frequency by Region
png("figs/Region_figs/01_frequency_by_region.png", width = 800, height = 600)
par(mar = c(7, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

region_sorted <- region_analysis[order(region_analysis$Frequency), ]

barplot(region_sorted$Frequency, names.arg = region_sorted$Region,
        col = "#8d17f1", xlab = "", ylab = "Frequency",
        las = 2, ylim = c(0, 0.15))

mtext("Region", side = 1, line = 5.5, cex = 1.3)

abline(h = sum(learn$ClaimNb) / sum(learn$Exposure), col = "red", lty = 2, lwd = 2)

dev.off()

# Plot 2: K-Means Elbow Plot
png("figs/Region_figs/02_kmeans_elbow.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

plot(k_values, within_ss, type = "b", pch = 19, col = "#8d17f1",
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares",
     main = "K-Means Elbow Plot for Region Clustering", lwd = 2, cex = 1.2)
grid()
dev.off()

# Plot 3: Cluster Visualisation (Frequency vs B12%)
png("figs/Region_figs/03_cluster_visualisation.png", width = 800, height = 600)
par(mar = c(5.5, 5.5, 3, 1), tcl = -0.25, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2, mgp = c(3.5, 0.7, 0))

cluster_colors <- c("lightblue", "lightgreen", "#8d17f1", "orange")
plot(region_features$B12_pct, region_features$Frequency, 
     col = cluster_colors[region_features$Cluster_k4], pch = 19, cex = 1.5,
     xlab = "B12 Concentration (%)", ylab = "Frequency",
     main = "K-Means k=4 Clusters: Frequency vs B12 Concentration")
legend("topleft", legend = paste("Cluster", 1:4), col = cluster_colors, pch = 19, cex = 1.1)
text(region_features$B12_pct, region_features$Frequency, labels = region_features$Region, 
     pos = 3, cex = 0.7)
grid()

# DENSITY HAS AN EFFECT, THIS HAS TO BE UPDATED.
# THIS IS DEFINITELY AN ENORMOUS WASTE OF A WEEK. 


