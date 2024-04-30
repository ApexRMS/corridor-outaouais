## a312
## Carina Rauen Firkowski 
## March 26, 2024
##
## This script loads the resistance layers from Rayfield et al. 2022 and 
## projects them to the target CRS.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Resistance layer
resistanceBLBR <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "BLBR_resistance_30m.tif")) %>%
  project("EPSG: 32198")
resistanceMAAM <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "MAAM_resistance_30m.tif")) %>%
  project("EPSG: 32198")
resistancePLCI <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "PLCI_resistance_30m.tif")) %>%
  project("EPSG: 32198")
resistanceRASY <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "RASY_resistance_30m.tif")) %>%
  project("EPSG: 32198")
resistanceURAM <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "URAM_resistance_30m.tif")) %>%
  project("EPSG: 32198")



# Write to file ----------------------------------------------------------------

writeRaster(resistanceBLBR, 
            file.path(intermediatesDir, "Resistance", 
                      "resistanceBLBR_projected.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(resistanceMAAM, 
            file.path(intermediatesDir, "Resistance", 
                      "resistanceMAAM_projected.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(resistancePLCI, 
            file.path(intermediatesDir, "Resistance", 
                      "resistancePLCI_projected.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(resistanceRASY, 
            file.path(intermediatesDir, "Resistance", 
                      "resistanceRASY_projected.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(resistanceURAM, 
            file.path(intermediatesDir, "Resistance", 
                      "resistanceURAM_projected.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)



