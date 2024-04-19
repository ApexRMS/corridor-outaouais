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
library(rgrass)



# Directories ------------------------------------------------------------------

# Core directories
rootDir <- "."
dataDir <- file.path(rootDir, "Data")
intermediatesDir <- file.path(rootDir, "Intermediates")
outputDir <- file.path(rootDir, "Output")

# Composite directories
tabularDataDir <- file.path(dataDir, "Tabular")
spatialDataDir <- file.path(dataDir, "Spatial")

# GRASS Parameters -------------------------------------------------------------
gisBase <- "C:/Program Files/GRASS GIS 7.8"
gisDbase <- getwd()
grassLocation <- "a312"

doGRASSSetup <- TRUE

# Other Parameters -------------------------------------------------------------
# List of species codes
speciesList <- c("MAAM", "BLBR", "URAM", "PLCI", "RASY")

# Species that should have gaps within patches filled
speciesFillGaps <- c('MAAM','URAM')
