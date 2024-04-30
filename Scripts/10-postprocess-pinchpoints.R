## a312
## Carina Rauen Firkowski 
## April 29, 2024
##
## This script loads the species-specific corridors and pinchpoints from Linkage 
## Mapper and masks values to distance threshold cut-off.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Local corridors
BLBRlocal <- rast(file.path(outputDir, "Final",
                            "localBLBR_corridors.tif"))
MAAMlocal <- rast(file.path(outputDir, "Final",
                            "localMAAM_corridors.tif"))
PLCIlocal <- rast(file.path(outputDir, "Final",
                            "localPLCI_corridors.tif"))
RASYlocal <- rast(file.path(outputDir, "Final",
                            "localRASY_corridors.tif"))
URAMlocal <- rast(file.path(outputDir, "Final",
                            "localURAM_corridors.tif"))

# Regional corridors
BLBRregional_corridors <- rast(file.path(outputDir, "Final",
                                         "regionalBLBR_corridors.tif"))
MAAMregional_corridors <- rast(file.path(outputDir, "Final",
                                         "regionalMAAM_corridors.tif"))
PLCIregional_corridors <- rast(file.path(outputDir, "Final",
                                         "regionalPLCI_corridors.tif"))
RASYregional_corridors <- rast(file.path(outputDir, "Final",
                                         "regionalRASY_corridors.tif"))
URAMregional_corridors <- rast(file.path(outputDir, "Final",
                                         "regionalURAM_corridors.tif"))


# Regional pinchpoints
BLBRregional_pinchpoints <- rast(file.path(outputDir, "Final",
                                           "regionalBLBR_pinchpoints.tif"))
MAAMregional_pinchpoints <- rast(file.path(outputDir, "Final",
                                           "regionalMAAM_pinchpoints.tif"))
PLCIregional_pinchpoints <- rast(file.path(outputDir, "Final",
                                           "regionalPLCI_pinchpoints.tif"))
RASYregional_pinchpoints <- rast(file.path(outputDir, "Final",
                                           "regionalRASY_pinchpoints.tif"))
URAMregional_pinchpoints <- rast(file.path(outputDir, "Final",
                                           "regionalURAM_pinchpoints.tif"))

# Analysis area
regionalArea <- rast(file.path(intermediatesDir, "Boundaries",
                               "regionalArea_full.tif"))


# Corridor cut-off -------------------------------------------------------------

# Threshold
longDist <- 2500
shortDist <- 825

# Cut-off
# Local
BLBRlocal[BLBRlocal > shortDist] <- NA
MAAMlocal[MAAMlocal > longDist] <- NA
PLCIlocal[PLCIlocal > shortDist] <- NA
RASYlocal[RASYlocal > shortDist] <- NA
URAMlocal[URAMlocal > longDist] <- NA
# Regional
BLBRregional_corridors[BLBRregional_corridors > shortDist] <- NA
MAAMregional_corridors[MAAMregional_corridors > longDist] <- NA
PLCIregional_corridors[PLCIregional_corridors > shortDist] <- NA
RASYregional_corridors[RASYregional_corridors > shortDist] <- NA
URAMregional_corridors[URAMregional_corridors > longDist] <- NA



# Mask pinchpoints to corridors ------------------------------------------------

# BLBR
corridorsBLBRregional_resampled <- BLBRregional_corridors %>%
  resample(regionalArea)
pinchpointsBLBRregional_masked <- BLBRregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsBLBRregional_resampled)
# MAAM
corridorsMAAMregional_resampled <- MAAMregional_corridors %>%
  resample(regionalArea)
pinchpointsMAAMregional_masked <- MAAMregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsMAAMregional_resampled)
# PLCI
corridorsPLCIregional_resampled <- PLCIregional_corridors %>%
  resample(regionalArea)
pinchpointsPLCIregional_masked <- PLCIregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsPLCIregional_resampled)
# RASY
corridorsRASYregional_resampled <- RASYregional_corridors %>%
  resample(regionalArea)
pinchpointsRASYregional_masked <- RASYregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsRASYregional_resampled)
# URAM
corridorsURAMregional_resampled <- URAMregional_corridors %>%
  resample(regionalArea)
pinchpointsURAMregional_masked <- URAMregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsURAMregional_resampled)


# Write to file ----------------------------------------------------------------

writeRaster(pinchpointsBLBRregional_masked, 
            file.path(outputDir, "Final",
                      "regionalBLBR_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsMAAMregional_masked, 
            file.path(outputDir, "Final",
                      "regionalMAAM_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsPLCIregional_masked, 
            file.path(outputDir, "Final",
                      "regionalPLCI_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsRASYregional_masked, 
            file.path(outputDir, "Final",
                      "regionalRASY_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsURAMregional_masked, 
            file.path(outputDir, "Final",
                      "regionalURAM_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
