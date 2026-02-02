rm(list = ls())

install.packages("OpenML") # Contains old dataset
install.packages("farff") # Need for reading in from OpenML

library(OpenML)
library(farff)

ds <- getOMLDataSet(data.id = 41214)
freMTPL2freq_raw <- ds$data # Right dimensions, is the dataset we need

# Pushing csv to GitHub for easy read-in.
write.csv(freMTPL2freq_raw, "freMTPL2freq_raw.csv", row.names = FALSE)
