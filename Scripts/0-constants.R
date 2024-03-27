## a312
## Carina Rauen Firkowski
## March 26, 2024
##
## This script is used to define constants and load necessary functions in a
## reproducible way across scripts.



# Workspace -------------------------------------------------------------------

# Load packages
library(terra)
library(sf)
library(tidyverse)



# Directories ------------------------------------------------------------------

# Core directories
rootDir <- "."
dataDir <- file.path(rootDir, "Data")
intermediatesDir <- file.path(rootDir, "Intermediates")
outputDir <- file.path(rootDir, "Outputs")

# Composite directories
tabularDataDir <- file.path(dataDir, "Tabular")
spatialDataDir <- file.path(dataDir, "Spatial")


