## a312
## Carina Rauen Firkowski 
## March 26, 2024
##
## This script prepares the resistance layers for the corridor delimitation
## analyses, at the local and regional scales.
##
## First, it combines:
##    - The original resistance layers from Rayfield et al. 2022
##    - The extended area resistance to cover the full regional analysis extent
##
## Then, it creates the resistance layer for area with missing data in Ontario.
##
## Lastly it combines the original, extended and missing area data and re-scales
## the resistance values.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
# Shapefile
regionalArea <- st_read(dsn = file.path(intermediatesDir, "Boundaries"),
                        layer = "regionalFullStudyArea")
localArea <- st_read(dsn = file.path(intermediatesDir, "Boundaries"),
                     layer = "localStudyArea")
# Raster
regionalArea_raster <- rast(file.path(intermediatesDir, "Boundaries",
                                      "regionalArea_full.tif"))

# Ontario LULC 
LULContario <- rast(file.path(spatialDataDir, 
                              "LULC", 
                              "Occ_sol_2014_recl_FED_10m_aout2017.tif")) %>%
  project("EPSG: 32198")

# Resistance layer
# Original
resistanceBLBR <- rast(file.path(intermediatesDir, "Resistance", 
                                 "resistanceBLBR_projected.tif"))
resistanceMAAM <- rast(file.path(intermediatesDir, "Resistance", 
                                 "resistanceMAAM_projected.tif"))
resistancePLCI <- rast(file.path(intermediatesDir, "Resistance", 
                                 "resistancePLCI_projected.tif"))
resistanceRASY <- rast(file.path(intermediatesDir, "Resistance", 
                                 "resistanceRASY_projected.tif"))
resistanceURAM <- rast(file.path(intermediatesDir, "Resistance", 
                                 "resistanceURAM_projected.tif"))
# Extended
resistanceExtBLBR <- rast(file.path(intermediatesDir, "Resistance", "Extended",
                                    "BLBR_resistance_extended_30m_update.tif"))
resistanceExtMAAM <- rast(file.path(intermediatesDir, "Resistance", "Extended",
                                    "MAAM_resistance_extended_30m.tif"))
resistanceExtPLCI <- rast(file.path(intermediatesDir, "Resistance", "Extended",
                                    "PLCI_resistance_extended_30m_update.tif")) 
resistanceExtRASY <- rast(file.path(intermediatesDir, "Resistance", "Extended",
                                    "RASY_resistance_extended_30m_update.tif"))
resistanceExtURAM <- rast(file.path(intermediatesDir, "Resistance", "Extended",
                                    "URAM_resistance_extended_30m_update.tif")) 



# Load tabular data ------------------------------------------------------------

# Re-classification tables
LULCreclass <- read.csv(file.path(tabularDataDir, 
                                  "landcoverBufferReclass.csv")) 
sppLULCreclass <- read.csv(file.path(tabularDataDir, 
                                     "speciesLandcoverReclass.csv")) 
sppHabitat <- read.csv(file.path(tabularDataDir, 
                                 "speciesHabitatPatchParameters.csv"))
sppResistance <- read.csv(file.path(tabularDataDir, 
                                    "speciesResistanceReclass.csv"))



# Combine original and extended resistance -------------------------------------

# Function to crop & mask raster to study extent
crop_mask_raster <- function(resistanceRaster, studyExtent){
  
  targetResistance <- resistanceRaster %>%
    crop(studyExtent) %>%
    mask(studyExtent)
  
  return(targetResistance)
}

# Combine original and extended data
resistanceBLBR_combined <- merge(resistanceBLBR, 
                                 resistanceExtBLBR)
resistanceMAAM_combined <- merge(resistanceMAAM, 
                                 resistanceExtMAAM)
resistancePLCI_combined <- merge(resistancePLCI, 
                                 resistanceExtPLCI)
resistanceRASY_combined <- merge(resistanceRASY, 
                                 resistanceExtRASY)
resistanceURAM_combined <- merge(resistanceURAM, 
                                 resistanceExtURAM)

# Crop to study extent
# Regional
regionalResBLBR <- crop_mask_raster(resistanceRaster = resistanceBLBR_combined, 
                                    studyExtent = regionalArea)
regionalResMAAM <- crop_mask_raster(resistanceRaster = resistanceMAAM_combined, 
                                    studyExtent = regionalArea)
regionalResPLCI <- crop_mask_raster(resistanceRaster = resistancePLCI_combined, 
                                    studyExtent = regionalArea)
regionalResRASY <- crop_mask_raster(resistanceRaster = resistanceRASY_combined, 
                                    studyExtent = regionalArea)
regionalResURAM <- crop_mask_raster(resistanceRaster = resistanceURAM_combined, 
                                    studyExtent = regionalArea)
# Local
localResBLBR <- crop_mask_raster(resistanceRaster = resistanceBLBR, 
                                 studyExtent = localArea)
localResMAAM <- crop_mask_raster(resistanceRaster = resistanceMAAM, 
                                 studyExtent = localArea)
localResPCLI <- crop_mask_raster(resistanceRaster = resistancePLCI, 
                                 studyExtent = localArea)
localResRASY <- crop_mask_raster(resistanceRaster = resistanceRASY, 
                                 studyExtent = localArea)
localResURAM <- crop_mask_raster(resistanceRaster = resistanceURAM, 
                                 studyExtent = localArea)



# Create missing resistance for Ontario ----------------------------------------

# Generate missing area --------------------------

# Create resistance template
resistanceTemplate <- templateRaster <- regionalResBLBR

# Template raster
values(templateRaster) <- 0

# Placeholder missing data area
dataExtent <- regionalArea_raster
dataExtent <- resample(dataExtent, templateRaster, method = "near")

# Set all values to 1
resistanceTemplate[!is.na(resistanceTemplate)] <- 1

# Set NA to 0
resistanceTemplate[is.na(resistanceTemplate)] <- 0
dataExtent[is.na(dataExtent)] <- 0

# Subtract analysis and data extents
missingExtent <- resistanceTemplate - dataExtent 

# Reclassify & mask
missingExtent[missingExtent == -1] <- 1
missingExtent[missingExtent == 0] <- NA
#plot(missingExtent)

# Detect patches and keep largest
missingPatches <- patches(missingExtent)
missingFreq <- freq(missingPatches)
missingFreq <- missingFreq[missingFreq$count != 486139,]
missingExtent[which.lyr(
  missingPatches %in% missingFreq$value)] <- NA

# Crop and mask data to missing data area
LULCmissing <- LULContario %>%
  crop(missingExtent) %>%
  resample(missingExtent, method = "near") %>%
  mask(missingExtent)
#plot(LULCmissing)


# Reclassify data --------------------------------

# Create matrix
LULCmatrix <- matrix(c(LULCreclass$Value, 
                       LULCreclass$Code), ncol = 2, byrow = FALSE)

# Update LULC codes
LULCcodes <- classify(LULCmissing, LULCmatrix)

# Function to create LULC suitability map for each species
LULCsuitability <- function(reclassTable = sppLULCreclass, 
                            spp, 
                            LULCraster = LULCcodes){
  
  # Create matrix
  SuitMatrix <- matrix(c(reclassTable$LandcoverCode, 
                         reclassTable[,spp]), ncol = 2, byrow = FALSE)
  
  # Update LULC codes
  LULC_spp <- classify(LULCraster, SuitMatrix)
  
  return(LULC_spp)
  
}

# Create LULC suitability map for each species
BLBR_LULC <- LULCsuitability(spp = "BLBR")
MAAM_LULC <- LULCsuitability(spp = "MAAM")
PLCI_LULC <- LULCsuitability(spp = "PLCI")
RASY_LULC <- LULCsuitability(spp = "RASY")
URAM_LULC <- LULCsuitability(spp = "URAM")

# Function to create binary suitability map
habitatSuitability <- function(habitatTable = sppHabitat,
                               spp, 
                               sppLULC){
  
  # Get threshold value
  targetData <- subset(habitatTable, Species == spp)
  thresholdValue <- targetData$SuitabilityThreshold
  
  # Reclassify values based on habitat suitability threshold
  sppLULC[sppLULC < thresholdValue] <- NA
  sppLULC[sppLULC >= thresholdValue] <- 1
  
  return(sppLULC)
  
}

# Create binary suitability map
BLBR_suitable <- habitatSuitability(spp = "BLBR", sppLULC = BLBR_LULC)
MAAM_suitable <- habitatSuitability(spp = "MAAM", sppLULC = MAAM_LULC)
PLCI_suitable <- habitatSuitability(spp = "PLCI", sppLULC = PLCI_LULC)
RASY_suitable <- habitatSuitability(spp = "RASY", sppLULC = RASY_LULC)
URAM_suitable <- habitatSuitability(spp = "URAM", sppLULC = URAM_LULC)

# Function to detect patches, filter suitable patches and reclassify
patchesSuitability <- function(sppSuitability,
                               spp,
                               habitatTable = sppHabitat){
  
  # Detect patches
  sppPatches <- patches(sppSuitability)
  sppFreq <- freq(sppPatches)
  
  # Get minimum patch size
  targetData <- subset(habitatTable, Species == spp)
  MinPatchSize <- targetData$MinPatchSize
  
  # Filter patches smaller then threshold
  sppFilter <- sppFreq[
    sppFreq$count >= MinPatchSize,]
  
  # Habitat patches
  sppHabitatPatches <- sppSuitability
  sppHabitatPatches[which.lyr(
    sppPatches %in% sppFreq$value)] <- 900
  sppHabitatPatches[!which.lyr(
    sppPatches %in% sppFreq$value)] <- 910
  
  return(sppHabitatPatches)
  
}

# Create suitable habitat patches maps
BLBR_habitatPatches <- patchesSuitability(sppSuitability = BLBR_suitable,
                                          spp = "BLBR")
MAAM_habitatPatches <- patchesSuitability(sppSuitability = MAAM_suitable,
                                          spp = "MAAM")
PLCI_habitatPatches <- patchesSuitability(sppSuitability = PLCI_suitable,
                                          spp = "PLCI")
RASY_habitatPatches <- patchesSuitability(sppSuitability = RASY_suitable,
                                          spp = "RASY")
URAM_habitatPatches <- patchesSuitability(sppSuitability = URAM_suitable,
                                          spp = "URAM")

# Combine LULC and suitable habitat patches
BLBR_combined <- merge(BLBR_habitatPatches, LULCcodes)
MAAM_combined <- merge(MAAM_habitatPatches, LULCcodes)
PLCI_combined <- merge(PLCI_habitatPatches, LULCcodes)
RASY_combined <- merge(RASY_habitatPatches, LULCcodes)
URAM_combined <- merge(URAM_habitatPatches, LULCcodes)

# Function to create resistance layers
reclassResistance <- function(reclassTable = sppResistance,
                              spp, 
                              sppCombined){
  
  # Create matrix
  resistanceMatrix <- matrix(c(reclassTable$LandcoverCode, 
                               reclassTable[,spp]), ncol = 2, byrow = FALSE)
  
  # Update LULC codes
  Resistance_spp <- classify(sppCombined, resistanceMatrix)
  
  return(Resistance_spp)
  
}

# Create resistance layers
BLBR_resistance <- reclassResistance(spp = "BLBR", sppCombined = BLBR_combined)
MAAM_resistance <- reclassResistance(spp = "MAAM", sppCombined = MAAM_combined)
PLCI_resistance <- reclassResistance(spp = "PLCI", sppCombined = PLCI_combined)
RASY_resistance <- reclassResistance(spp = "RASY", sppCombined = RASY_combined)
URAM_resistance <- reclassResistance(spp = "URAM", sppCombined = URAM_combined)



# Combine resistance and re-scale ----------------------------------------------

# Combine original, extended, and missing area data
regionalAllResBLBR <- merge(regionalResBLBR, BLBR_resistance)
regionalAllResMAAM <- merge(regionalResMAAM, MAAM_resistance)
regionalAllResPLCI <- merge(regionalResPLCI, PLCI_resistance)
regionalAllResRASY <- merge(regionalResRASY, RASY_resistance)
regionalAllResURAM <- merge(regionalResURAM, URAM_resistance)

localResBLBR_scaledNorm <- classify(localResBLBR, reclassTable)
localResMAAM_scaled <- classify(localResMAAM, reclassTable)
localResPLCI_scaled <- classify(localResPLCI, reclassTable)
localResRASY_scaled <- classify(localResRASY, reclassTable)
localResURAM_scaled <- classify(localResURAM, reclassTable)



# Write to file ----------------------------------------------------------------

# Resistance layers
# Regional
writeRaster(regionalAllResBLBR, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_regional_BLBR.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalAllResMAAM, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_regional_MAAM.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalAllResPLCI, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_regional_PLCI.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalAllResRASY, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_regional_RASY.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalAllResURAM, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_regional_URAM.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)

# Local
writeRaster(localResBLBR, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_local_BLBR.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResMAAM, 
            file.path(intermediatesDir, "Linkage Mapper", "Final", 
                      "resistance_local_MAAM.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResPCLI, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_local_PCLI.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResRASY, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_local_RASY.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResURAM, 
            file.path(intermediatesDir, "Linkage Mapper", "Final",
                      "resistance_local_URAM.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)


