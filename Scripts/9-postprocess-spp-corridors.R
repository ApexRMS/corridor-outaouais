## a312
## Carina Rauen Firkowski 
## April 8, 2024
##
## This script loads the species-specific corridors from Linkage Mapper and 
## combines results into a single map per spatial scale.



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
                                     "localBLBRnoNCC_corridors.tif"))
MAAMlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localMAAMnoNCC_corridors.tif"))
PLCIlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localPLCInoNCC_corridors.tif"))
RASYlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localRASYnoNCC_corridors.tif"))
URAMlocal_appendix <- rast(file.path(outputDir, "Final",
                                     "localURAMnoNCC_corridors.tif"))

# Regional corridors
BLBRregional <- rast(file.path(outputDir, "Final",
                               "regionalBLBR_corridors.tif"))
MAAMregional <- rast(file.path(outputDir, "Final",
                               "regionalMAAM_corridors.tif"))
PLCIregional <- rast(file.path(outputDir, "Final",
                               "regionalPLCI_corridors.tif"))
RASYregional <- rast(file.path(outputDir, "Final",
                               "regionalRASY_corridors.tif"))
URAMregional <- rast(file.path(outputDir, "Final",
                               "regionalURAM_corridors.tif"))



# Corridor cut-off -------------------------------------------------------------

# Threshold
longDist <- 2500
shortDist <- 825

# Cut-off

# Local
# Report
BLBRlocal[BLBRlocal > shortDist] <- NA
MAAMlocal[MAAMlocal > longDist] <- NA
PLCIlocal[PLCIlocal > shortDist] <- NA
RASYlocal[RASYlocal > shortDist] <- NA
URAMlocal[URAMlocal > longDist] <- NA
# Appendix
BLBRlocal_appendix[BLBRlocal_appendix > shortDist] <- NA
MAAMlocal_appendix[MAAMlocal_appendix > longDist] <- NA
PLCIlocal_appendix[PLCIlocal_appendix > shortDist] <- NA
RASYlocal_appendix[RASYlocal_appendix > shortDist] <- NA
URAMlocal_appendix[URAMlocal_appendix > longDist] <- NA

# Regional
BLBRregional[BLBRregional > shortDist] <- NA
MAAMregional[MAAMregional > longDist] <- NA
PLCIregional[PLCIregional > shortDist] <- NA
RASYregional[RASYregional > shortDist] <- NA
URAMregional[URAMregional > longDist] <- NA



# Combine corridors (continuous) -----------------------------------------------

# Re-scale corridor
# Local
# Report
BLBRlocal_scaled <- BLBRlocal/shortDist
MAAMlocal_scaled <- MAAMlocal/longDist
PLCIlocal_scaled <- PLCIlocal/shortDist
RASYlocal_scaled <- RASYlocal/shortDist
URAMlocal_scaled <- URAMlocal/longDist
# Appendix
BLBRlocal_appendix_scaled <- BLBRlocal_appendix/shortDist
MAAMlocal_appendix_scaled <- MAAMlocal_appendix/longDist
PLCIlocal_appendix_scaled <- PLCIlocal_appendix/shortDist
RASYlocal_appendix_scaled <- RASYlocal_appendix/shortDist
URAMlocal_appendix_scaled <- URAMlocal_appendix/longDist
# Regional
BLBRregional_scaled <- BLBRregional/shortDist
MAAMregional_scaled <- MAAMregional/longDist
PLCIregional_scaled <- PLCIregional/shortDist
RASYregional_scaled <- RASYregional/shortDist
URAMregional_scaled <- URAMregional/longDist

# Invert scale 
# NOTE: 1 to represent closest to least-cost path and 
#       0 to represent furthest distance from least-cost path
# Local
# Report
BLBRlocal_inverted <- 0 - BLBRlocal_scaled + 1
MAAMlocal_inverted <- 0 - MAAMlocal_scaled + 1
PLCIlocal_inverted <- 0 - PLCIlocal_scaled + 1
RASYlocal_inverted <- 0 - RASYlocal_scaled + 1
URAMlocal_inverted <- 0 - URAMlocal_scaled + 1
# Appendix
BLBRlocal_appendix_inverted <- 0 - BLBRlocal_appendix_scaled + 1
MAAMlocal_appendix_inverted <- 0 - MAAMlocal_appendix_scaled + 1
PLCIlocal_appendix_inverted <- 0 - PLCIlocal_appendix_scaled + 1
RASYlocal_appendix_inverted <- 0 - RASYlocal_appendix_scaled + 1
URAMlocal_appendix_inverted <- 0 - URAMlocal_appendix_scaled + 1
# Regional
BLBRregional_inverted <- 0 - BLBRregional_scaled + 1
MAAMregional_inverted <- 0 - MAAMregional_scaled + 1
PLCIregional_inverted <- 0 - PLCIregional_scaled + 1
RASYregional_inverted <- 0 - RASYregional_scaled + 1
URAMregional_inverted <- 0 - URAMregional_scaled + 1

# Set NA values to 0
# Local
# Report
BLBRlocal_inverted[is.na(BLBRlocal_inverted)] <- 0
MAAMlocal_inverted[is.na(MAAMlocal_inverted)] <- 0
PLCIlocal_inverted[is.na(PLCIlocal_inverted)] <- 0
RASYlocal_inverted[is.na(RASYlocal_inverted)] <- 0
URAMlocal_inverted[is.na(URAMlocal_inverted)] <- 0
# Appendix
BLBRlocal_appendix_inverted[is.na(BLBRlocal_appendix_inverted)] <- 0
MAAMlocal_appendix_inverted[is.na(MAAMlocal_appendix_inverted)] <- 0
PLCIlocal_appendix_inverted[is.na(PLCIlocal_appendix_inverted)] <- 0
RASYlocal_appendix_inverted[is.na(RASYlocal_appendix_inverted)] <- 0
URAMlocal_appendix_inverted[is.na(URAMlocal_appendix_inverted)] <- 0
# Regional
BLBRregional_inverted[is.na(BLBRregional_inverted)] <- 0
MAAMregional_inverted[is.na(MAAMregional_inverted)] <- 0
PLCIregional_inverted[is.na(PLCIregional_inverted)] <- 0
RASYregional_inverted[is.na(RASYregional_inverted)] <- 0
URAMregional_inverted[is.na(URAMregional_inverted)] <- 0

# Sum corridors
localSum_inverted <- sum(c(BLBRlocal_inverted,
                           MAAMlocal_inverted,
                           PLCIlocal_inverted,
                           RASYlocal_inverted,
                           URAMlocal_inverted))
localSum_appendix_inverted <- sum(c(BLBRlocal_appendix_inverted,
                                    MAAMlocal_appendix_inverted,
                                    PLCIlocal_appendix_inverted,
                                    RASYlocal_appendix_inverted,
                                    URAMlocal_appendix_inverted))
regionalSum_inverted <- sum(c(BLBRregional_inverted,
                              MAAMregional_inverted,
                              PLCIregional_inverted,
                              RASYregional_inverted,
                              URAMregional_inverted))

# Set 0 to NA
localSum_inverted[localSum_inverted == 0] <- NA
localSum_appendix_inverted[localSum_appendix_inverted == 0] <- NA
regionalSum_inverted[regionalSum_inverted == 0] <- NA



# Combine corridors (binary) ---------------------------------------------------

# BLBR
BLBRlocal_binary <- BLBRlocal
BLBRlocal_binary[!is.na(BLBRlocal_binary)] <- 1
BLBRlocal_binary[is.na(BLBRlocal_binary)] <- 0
BLBRlocal_appendix_binary <- BLBRlocal_appendix
BLBRlocal_appendix_binary[!is.na(BLBRlocal_appendix_binary)] <- 1
BLBRlocal_appendix_binary[is.na(BLBRlocal_appendix_binary)] <- 0
BLBRregional_binary <- BLBRregional
BLBRregional_binary[!is.na(BLBRregional_binary)] <- 1
BLBRregional_binary[is.na(BLBRregional_binary)] <- 0
# MAAM
MAAMlocal_binary <- MAAMlocal
MAAMlocal_binary[!is.na(MAAMlocal_binary)] <- 1
MAAMlocal_binary[is.na(MAAMlocal_binary)] <- 0
MAAMlocal_appendix_binary <- MAAMlocal_appendix
MAAMlocal_appendix_binary[!is.na(MAAMlocal_appendix_binary)] <- 1
MAAMlocal_appendix_binary[is.na(MAAMlocal_appendix_binary)] <- 0
MAAMregional_binary <- MAAMregional
MAAMregional_binary[!is.na(MAAMregional_binary)] <- 1
MAAMregional_binary[is.na(MAAMregional_binary)] <- 0
# PLCI
PLCIlocal_binary <- PLCIlocal
PLCIlocal_binary[!is.na(PLCIlocal_binary)] <- 1
PLCIlocal_binary[is.na(PLCIlocal_binary)] <- 0
PLCIlocal_appendix_binary <- PLCIlocal_appendix
PLCIlocal_appendix_binary[!is.na(PLCIlocal_appendix_binary)] <- 1
PLCIlocal_appendix_binary[is.na(PLCIlocal_appendix_binary)] <- 0
PLCIregional_binary <- PLCIregional
PLCIregional_binary[!is.na(PLCIregional_binary)] <- 1
PLCIregional_binary[is.na(PLCIregional_binary)] <- 0
# RASY
RASYlocal_binary <- RASYlocal
RASYlocal_binary[!is.na(RASYlocal_binary)] <- 1
RASYlocal_binary[is.na(RASYlocal_binary)] <- 0
RASYlocal_appendix_binary <- RASYlocal_appendix
RASYlocal_appendix_binary[!is.na(RASYlocal_appendix_binary)] <- 1
RASYlocal_appendix_binary[is.na(RASYlocal_appendix_binary)] <- 0
RASYregional_binary <- RASYregional
RASYregional_binary[!is.na(RASYregional_binary)] <- 1
RASYregional_binary[is.na(RASYregional_binary)] <- 0
# URAM
URAMlocal_binary <- URAMlocal
URAMlocal_binary[!is.na(URAMlocal_binary)] <- 1
URAMlocal_binary[is.na(URAMlocal_binary)] <- 0
URAMlocal_appendix_binary <- URAMlocal_appendix
URAMlocal_appendix_binary[!is.na(URAMlocal_appendix_binary)] <- 1
URAMlocal_appendix_binary[is.na(URAMlocal_appendix_binary)] <- 0
URAMregional_binary <- URAMregional
URAMregional_binary[!is.na(URAMregional_binary)] <- 1
URAMregional_binary[is.na(URAMregional_binary)] <- 0

# Sum corridors
localSum_binary <- sum(c(BLBRlocal_binary,
                         MAAMlocal_binary,
                         PLCIlocal_binary,
                         RASYlocal_binary,
                         URAMlocal_binary))
localSum_appendix_binary <- sum(c(BLBRlocal_appendix_binary,
                                  MAAMlocal_appendix_binary,
                                  PLCIlocal_appendix_binary,
                                  RASYlocal_appendix_binary,
                                  URAMlocal_appendix_binary))
regionalSum_binary <- sum(c(BLBRregional_binary,
                            MAAMregional_binary,
                            PLCIregional_binary,
                            RASYregional_binary,
                            URAMregional_binary))

# Set 0 to NA
localSum_binary[localSum_binary == 0] <- NA
localSum_appendix_binary[localSum_appendix_binary == 0] <- NA
regionalSum_binary[regionalSum_binary == 0] <- NA



# Write to file ----------------------------------------------------------------

# Masked corridors
# Local
# Report
writeRaster(BLBRlocal, 
            file.path(outputDir, "Final", 
                      "localBLBR_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(MAAMlocal, 
            file.path(outputDir, "Final", 
                      "localMAAM_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(PLCIlocal, 
            file.path(outputDir, "Final", 
                      "localPLCI_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(RASYlocal, 
            file.path(outputDir, "Final", 
                      "localRASY_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(URAMlocal, 
            file.path(outputDir, "Final", 
                      "localURAM_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
# Appendix
writeRaster(BLBRlocal_appendix, 
            file.path(outputDir, "Final", 
                      "localBLBR_corridors_noNCC_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(MAAMlocal_appendix, 
            file.path(outputDir, "Final", 
                      "localMAAM_corridors_noNCC_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(PLCIlocal_appendix, 
            file.path(outputDir, "Final", 
                      "localPLCI_corridors_noNCC_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(RASYlocal_appendix, 
            file.path(outputDir, "Final", 
                      "localRASY_corridors_noNCC_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(URAMlocal_appendix, 
            file.path(outputDir, "Final", 
                      "localURAM_corridors_noNCC_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
# Regional
writeRaster(BLBRregional, 
            file.path(outputDir, "Final", 
                      "regionalBLBR_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(MAAMregional, 
            file.path(outputDir, "Final", 
                      "regionalMAAM_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(PLCIregional, 
            file.path(outputDir, "Final", 
                      "regionalPLCI_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(RASYregional, 
            file.path(outputDir, "Final", 
                      "regionalRASY_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(URAMregional, 
            file.path(outputDir, "Final", 
                      "regionalURAM_corridors_masked.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)

# Combined corridors (continuous version)
writeRaster(localSum_inverted, 
            file.path(outputDir, "Final", 
                      "local_sppCorridors_continuous.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(localSum_appendix_inverted, 
            file.path(outputDir, "Final", 
                      "local_sppCorridors_noNCC_continuous.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(regionalSum_inverted, 
            file.path(outputDir, "Final", 
                      "regional_sppCorridors_continuous.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)

# Combined corridors (discrete version)
writeRaster(localSum_binary, 
            file.path(outputDir, "Final", 
                      "local_sppCorridors_binary.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(localSum_appendix_binary, 
            file.path(outputDir, "Final", 
                      "local_sppCorridors_noNCC_binary.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)
writeRaster(regionalSum_binary, 
            file.path(outputDir, "Final", 
                      "regional_sppCorridors_binary.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)


