################################################################################
# EDA: VehPower
################################################################################
rm(list = ls())

################################################################################
# Load data
################################################################################
learn <- read.csv("https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/data/train_set.csv")

################################################################################
# 1). Basic distribution
################################################################################
summary(learn$VehPower)
table(learn$VehPower, useNA = "ifany")
# VehPower only takes a small number of integer values.
# Most of the portfolio sits between 4 and 8.
# There is still a reasonable amount of data at 9+, but the counts thin out after that.


################################################################################
# 2). Frequency by exact VehPower
################################################################################
vehpower_analysis <- data.frame(
  VehPower = sort(unique(learn$VehPower)),
  Policies = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$VehPower == x)),
  Exposure = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$Exposure[learn$VehPower == x])),
  Claims = sapply(sort(unique(learn$VehPower)), function(x) sum(learn$ClaimNb[learn$VehPower == x]))
)

vehpower_analysis$Frequency <- vehpower_analysis$Claims / vehpower_analysis$Exposure
vehpower_analysis$SE <- sqrt(vehpower_analysis$Frequency / vehpower_analysis$Exposure)
vehpower_analysis$CI_lower <- pmax(0, vehpower_analysis$Frequency - 1.96 * vehpower_analysis$SE)
vehpower_analysis$CI_upper <- vehpower_analysis$Frequency + 1.96 * vehpower_analysis$SE

vehpower_analysis
# The pattern is not smooth enough to justify a continuous term.
# Powers 4 to 8 move around, rather than following a clean monotone trend.
# Above 9, the frequencies are noisier, especially in the highest powers.



################################################################################
# 3). Paper grouping, for reference
################################################################################
vehpower_paper_groups <- data.frame(
  group = c("4", "5", "6", "7", "8", "9plus"),
  exposure = c(
    sum(learn$Exposure[learn$VehPower == 4]),
    sum(learn$Exposure[learn$VehPower == 5]),
    sum(learn$Exposure[learn$VehPower == 6]),
    sum(learn$Exposure[learn$VehPower == 7]),
    sum(learn$Exposure[learn$VehPower == 8]),
    sum(learn$Exposure[learn$VehPower >= 9])
  ),
  claims = c(
    sum(learn$ClaimNb[learn$VehPower == 4]),
    sum(learn$ClaimNb[learn$VehPower == 5]),
    sum(learn$ClaimNb[learn$VehPower == 6]),
    sum(learn$ClaimNb[learn$VehPower == 7]),
    sum(learn$ClaimNb[learn$VehPower == 8]),
    sum(learn$ClaimNb[learn$VehPower >= 9])
  )
)

vehpower_paper_groups$frequency <- vehpower_paper_groups$claims / vehpower_paper_groups$exposure
vehpower_paper_groups
# The current grouping looks fine as is.
# Keeping 8 separate from 9+ looks justified.
# Pooling 9+ is still sensible, since higher powers too noisy.


################################################################################
# EDA conclusion
################################################################################
# Main points:
# 1). VehPower does not show a clean smooth trend.
# 2). The current grouped-factor treatment already matches the data fairly well.
# 3). The 9+ pooling is sensible because the upper tail is sparse.
# 4). So keeping VehPowerGLM unchanged before testing interactions.