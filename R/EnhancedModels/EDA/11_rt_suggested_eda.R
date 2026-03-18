rm(list = ls())
options(timeout = 200)

u <- "https://github.com/mattmccarthyy/Statistical-Consulting-Semester-2/raw/refs/heads/main/R/RegressionTrees/RT1000_stripped.rds"
f <- tempfile(fileext = ".rds")
download.file(u, f, mode = "wb")
RT1000 <- readRDS(f)


##############################################################################
# Brief overview and planning
##############################################################################
# Why I think I can use rt1000 as an explanatory device.
# rt1000_out = 31.50; glm1_out = 32.17, so rt1000 is seems to capture structure the glm is missing.
# training and test deviance are similiar, so it is not incredibly overfit.
# only has 64 splits, and minbucket = 1000, so it can be looked through in a reasonable amount of time.
# so it is probably fine to use as a hypothesis generator for interactions
# but is not proof of interactions. Will have to test those in the actual glm. 
# may also use this to see if some effects are REALLY non-linear, would then completely change them in the GLM. 

# Extract internal splits and their depth
fr <- RT1000$frame
fr$node  <- as.integer(row.names(fr))
fr$depth <- floor(log(fr$node, base = 2))

splits <- fr[fr$var != "<leaf>", c("node", "depth", "n", "var", "dev")]


# And looking at the top splits
top_splits <- subset(splits, depth <= 3)
top_splits[order(top_splits$depth, top_splits$node), ]
# The top of the tree is the most useful part for us, since these are the first
# and strongest structural decisions made by the model.
# If a variable appears very high in the tree, this suggests that it is an
# important predictor of claim frequency.
# Repeated early splits on the same variable can also suggest that the GLM may
# be misspecifying its main effect, for example by treating a nonlinear effect
# too simply.
# At this stage, mainly using the tree to identify candidates for
# improved main effects and if this works, will look at possible interactions.

# Examining actual decision paths
path.rpart(RT1000, nodes = c(1, 2, 3, 4, 5, 6, 7), print.it = TRUE)
# The decision paths show how the tree is partitioning the data at the top.
# This helps us see whether the role of one predictor changes depending on the
# value of another.
# For example, if the tree first splits on VehAge and then uses different
# predictors on each side, this suggests that the effect structure may differ
# by VehAge.
# That is useful both for detecting possible misspecification of main effects
# and for suggesting candidate interactions to test later in the GLM.

# Which variables recur near the top
sort(table(top_splits$var), decreasing = TRUE)
# Counting which variables recur near the top gives a simple summary of which
# predictors the tree relies on most heavily in its upper structure.
# Variables that recur early may either have strong main effects or effects that
# are not being captured well by the current GLM specification.
# In particular, repeated early use of numeric predictors can point to
# nonlinearity, while repeated use of different predictors within the same upper
# branches can suggest interactions.

# Left branch near top:
fr[fr$node %in% c(2,4,5,8,9,10,11), c("node","var","n","dev")]

# Right branch near top:
fr[fr$node %in% c(3,6,7,12,13,14,15), c("node","var","n","dev")]

# So far:
# The upper part of the tree suggests that VehAge, BonusMalus, and DrivAge are
# important predictors whose effects may not be fully captured by the current
# GLM specification. In particular, repeated early splits on these variables may
# indicate nonlinear main effects. In addition, because the variables used after
# the root split differ by VehAge branch, the tree also suggests that
# interactions involving VehAge may be worth testing once the main effects have
# been improved.