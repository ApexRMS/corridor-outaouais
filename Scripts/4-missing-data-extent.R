## a312
## Carina Rauen Firkowski 
## April 12, 2024
##
## This script creates a study area extent to be used to extend the data 
## coverage for the resistance layer.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Missing data area
missingData_sf <- st_read(dsn = file.path(intermediatesDir),
              layer = "missingDataArea") %>%
  st_transform(crs("EPSG: 32198"))

# MRC
allMRCs <- st_read(dsn = file.path(spatialDataDir, 
                                   "MRCs"),
                   layer = "MRC_s_2021_02") %>%
  st_transform(crs("EPSG: 32198"))

# Resistance layer
# NOTE: Used as template 
resistanceBLBR <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "BLBR_resistance_30m.tif")) %>%
  project("EPSG: 32198")



# Study area -------------------------------------------------------------------

# Crop missing area to resistance
resistance_missingData <- crop(resistanceBLBR, missingData_raster, 
                               extend = TRUE)

# Set parameters for template raster
templateCRS <- "EPSG: 32198"
templateExt <- ext(resistance_missingData)
templateExt <- c(-729390.343, -499186.867851853, 
                 262950.819959596, 465976.887053872 )
templateRes <- 30

# Create template raster
templateRaster <- rast()
crs(templateRaster) <- templateCRS
ext(templateRaster) <- templateExt
res(templateRaster) <- templateRes

# Filter target MRC
regionalMRC <- allMRCs %>%
  filter(MRS_NM_MRC == "Gatineau" | 
           MRS_NM_MRC == "La Vallée-de-la-Gatineau" |
           MRS_NM_MRC == "Les Collines-de-l'Outaouais" |
           MRS_NM_MRC == "Papineau" |
           MRS_NM_MRC == "Pontiac") 

# Merge boundaries
regionalMRC_merged <- regionalMRC %>%
  st_union() %>%
  st_as_sf() %>%
  st_set_geometry("geometry")

# Add 10 km buffer around MRC
regionalMRC_buffered <- st_buffer(regionalMRC_merged, 10000) 

# Rasterize missing data area
missingData_raster <- rasterize(regionalMRC_buffered, templateRaster)



# Write to file ----------------------------------------------------------------

writeRaster(missingData_raster, 
            file.path(intermediatesDir, "missingData_studyArea.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)



