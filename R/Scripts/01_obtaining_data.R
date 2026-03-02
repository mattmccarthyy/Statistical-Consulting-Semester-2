rm(list = ls())

install.packages(c("OpenML", "farff"))

library(OpenML) # Contains desired dataset
library(farff) # Needed to load in from OpenML

ds <- getOMLDataSet(data.id = 41214)
freMTPL2freq_raw <- ds$data # Correct dimensions, is the dataset we need

# Pushing csv to GitHub for easy read-in.
write.csv(freMTPL2freq_raw, "freMTPL2freq_raw.csv", row.names = FALSE)
