## a312
## Carina Rauen Firkowski 
## March 26, 2024
##
## This script prepares the analysis area for the corridor delimitation
## analyses, at the local and regional scales.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# MRC
allMRCs <- st_read(dsn = file.path(spatialDataDir, 
                                   "MRCs"),
                   layer = "MRC_s_2021_02") %>%
  st_transform(crs("EPSG: 32198"))

# LULC
LULC <- rast(file.path(spatialDataDir,
                       "LULC",
                       "b03-landcover-30m.tif")) %>%
  project("EPSG: 32198")



# Study area -------------------------------------------------------------------

# Regional scale analysis ------------------------

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

# Set parameters for template raster
regionalTemplateCRS <- "EPSG: 32198"
regionalTemplateExt <- c(-729203.039385656, -469850.581966365,
                         172366.188408253, 464978.217458996)
regionalTemplateRes <- 30

# Create template raster
regionalTemplate <- rast()
crs(regionalTemplate) <- regionalTemplateCRS
ext(regionalTemplate) <- regionalTemplateExt
res(regionalTemplate) <- regionalTemplateRes

# Rasterize local study area
regionalArea <- rasterize(regionalMRC_buffered, regionalTemplate)

# Crop & mask LULC to regional extent
regionalLULC <- LULC %>%
  crop(regionalArea) %>%
  resample(regionalArea) %>%
  mask(regionalArea)

# Subset water
regionalWater <- regionalLULC
regionalWater[regionalWater != 700] <- NA
regionalWater[is.na(regionalWater)] <- 0



# Local scale analysis ---------------------------

# Filter target MRC
localMRC <- regionalMRC %>%
  filter(MRS_NM_MRC == "Les Collines-de-l'Outaouais")

# Add 10 km buffer around MRC
localMRC_buffered <- st_buffer(localMRC, 10000) 

# Set parameters for template raster
localTemplateCRS <- "EPSG: 32198"
localTemplateExt <- c(-618919, -524331, 176931, 243819)
localTemplateRes <- 30

# Create template raster
localTemplate <- rast()
crs(localTemplate) <- localTemplateCRS
ext(localTemplate) <- localTemplateExt
res(localTemplate) <- localTemplateRes

# Rasterize local study area
localArea <- rasterize(localMRC_buffered, localTemplate)

# Crop & mask LULC to local extent
localLULC <- LULC %>%
  crop(localArea) %>%
  resample(localArea) %>%
  mask(localArea)

# Subset water
localWater <- localLULC
localWater[localWater != 700] <- NA
localWater[is.na(localWater)] <- 0



# Write to file ----------------------------------------------------------------

# Regional analysis area
# Raster
writeRaster(regionalArea, 
            file.path(intermediatesDir, "Boundaries", "regionalArea_full.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
# Shapefile
st_write(obj = regionalMRC_buffered,
         dsn = file.path(intermediatesDir, "Boundaries"),
         layer = "regionalFullStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

# Local analysis area
st_write(obj = localMRC_buffered,
         dsn = file.path(intermediatesDir, "Boundaries"),
         layer = "localStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
