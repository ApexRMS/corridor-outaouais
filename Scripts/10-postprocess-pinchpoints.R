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
# Report
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
# Appendix
BLBRlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localBLBR_corridors_noNCC_masked.tif"))
MAAMlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localMAAM_corridors_noNCC_masked.tif"))
PLCIlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localPLCI_corridors_noNCC_masked.tif"))
RASYlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localRASY_corridors_noNCC_masked.tif"))
URAMlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localURAM_corridors_noNCC_masked.tif"))

# Regional corridors
BLBRregional_corridors <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                         "regionalBLBR_corridors.tif"))
MAAMregional_corridors <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                         "MAAM_corridors_truncated_.tif"))
PLCIregional_corridors <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                         "regionalPLCI_corridors.tif"))
RASYregional_corridors <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                         "regionalRASY_corridors.tif"))
URAMregional_corridors <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                         "regionalURAM_corridors.tif"))

# Pinchpoints
# Regional
BLBRregional_pinchpoints <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                           "regionalBLBR_pinchpoints.tif"))
MAAMregional_pinchpoints <- rast(file.path(outputDir, "Final", 
                                          "2024-11 regional sources update",
                                          "MAAM_current_adjacentPair.tif"))
PLCIregional_pinchpoints <- rast(file.path(outputDir, "Final", 
                                          "2024-11 regional sources update",
                                          "regionalPLCI_pinchpoints.tif"))
RASYregional_pinchpoints <- rast(file.path(outputDir, "Final", 
                                         "2024-11 regional sources update",
                                           "regionalRASY_pinchpoints.tif"))
URAMregional_pinchpoints <- rast(file.path(outputDir, "Final", 
                                          "2024-11 regional sources update",
                                          "regionalURAM_pinchpoints.tif"))
# Local
BLBRlocal_pinchpoints <- rast(file.path(outputDir, "Final",
                                        "localBLBR_pinchpoints_appendix.tif"))
MAAMlocal_pinchpoints <- rast(file.path(outputDir, "Final",
                                        "localMAAM_pinchpoints_appendix.tif"))
PLCIlocal_pinchpoints <- rast(file.path(outputDir, "Final",
                                        "localPLCI_pinchpoints_appendix.tif"))
RASYlocal_pinchpoints <- rast(file.path(outputDir, "Final",
                                        "localRASY_pinchpoints_appendix.tif"))
URAMlocal_pinchpoints <- rast(file.path(outputDir, "Final",
                                        "localURAM_pinchpoints_appendix.tif"))

# Analysis area
regionalArea <- rast(file.path(intermediatesDir, "Boundaries",
                               "regionalArea_full.tif"))
localArea <- rast(file.path(intermediatesDir, "Boundaries",
                            "localArea.tif"))


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
BLBRlocal_appendix_resampled <- BLBRlocal_appendix %>%
  resample(localArea)
pinchpointsBLBRlocal_masked <- BLBRlocal_pinchpoints %>%
  resample(localArea) %>%
  mask(BLBRlocal_appendix_resampled)
# MAAM
corridorsMAAMregional_resampled <- MAAMregional_corridors %>%
  resample(regionalArea)
pinchpointsMAAMregional_masked <- MAAMregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsMAAMregional_resampled)
MAAMlocal_appendix_resampled <- MAAMlocal_appendix %>%
  resample(localArea)
pinchpointsMAAMlocal_masked <- MAAMlocal_pinchpoints %>%
  resample(localArea) %>%
  mask(MAAMlocal_appendix_resampled)
# PLCI
corridorsPLCIregional_resampled <- PLCIregional_corridors %>%
  resample(regionalArea)
pinchpointsPLCIregional_masked <- PLCIregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsPLCIregional_resampled)
PLCIlocal_appendix_resampled <- PLCIlocal_appendix %>%
  resample(localArea)
pinchpointsPLCIlocal_masked <- PLCIlocal_pinchpoints %>%
  resample(localArea) %>%
  mask(PLCIlocal_appendix_resampled)
# RASY
corridorsRASYregional_resampled <- RASYregional_corridors %>%
  resample(regionalArea)
pinchpointsRASYregional_masked <- RASYregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsRASYregional_resampled)
RASYlocal_appendix_resampled <- RASYlocal_appendix %>%
  resample(localArea)
pinchpointsRASYlocal_masked <- RASYlocal_pinchpoints %>%
  resample(localArea) %>%
  mask(RASYlocal_appendix_resampled)
# URAM
corridorsURAMregional_resampled <- URAMregional_corridors %>%
  resample(regionalArea)
pinchpointsURAMregional_masked <- URAMregional_pinchpoints %>%
  resample(regionalArea) %>%
  mask(corridorsURAMregional_resampled)
URAMlocal_appendix_resampled <- URAMlocal_appendix %>%
  resample(localArea)
pinchpointsURAMlocal_masked <- URAMlocal_pinchpoints %>%
  resample(localArea) %>%
  mask(URAMlocal_appendix_resampled)


# Write to file ----------------------------------------------------------------

# Regional
writeRaster(pinchpointsBLBRregional_masked, 
            file.path(outputDir, "Final",
                      "2024-11 regional sources update",
                      "regionalBLBR_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsMAAMregional_masked,
            file.path(outputDir, "Final",
                      "2024-11 regional sources update",
                      "regionalMAAM_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsPLCIregional_masked, 
            file.path(outputDir, "Final",
                      "2024-11 regional sources update",
                      "regionalPLCI_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsRASYregional_masked, 
            file.path(outputDir, "Final",
                      "2024-11 regional sources update",
                      "regionalRASY_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsURAMregional_masked, 
            file.path(outputDir, "Final",
                      "2024-11 regional sources update",
                      "regionalURAM_pinchpoints_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)

# Local
writeRaster(pinchpointsBLBRlocal_masked, 
            file.path(outputDir, "Final",
                      "localBLBR_pinchpoints_appendix_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsMAAMlocal_masked, 
            file.path(outputDir, "Final",
                      "localMAAM_pinchpoints_appendix_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsPLCIlocal_masked, 
            file.path(outputDir, "Final",
                      "localPLCI_pinchpoints_appendix_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsRASYlocal_masked, 
            file.path(outputDir, "Final",
                      "localRASY_pinchpoints_appendix_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
writeRaster(pinchpointsURAMlocal_masked, 
            file.path(outputDir, "Final",
                      "localURAM_pinchpoints_appendix_masked.tif"), 
            overwrite = TRUE, datatype = "FLT4S", NAflag = -9999)
