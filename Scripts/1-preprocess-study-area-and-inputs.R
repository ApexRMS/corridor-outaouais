## a312
## Carina Rauen Firkowski 
## March 26, 2024
##
## This script prepares the following inputs for the corridor delimitation
## analyses, at the local and regional scales:
##    - Analysis area
##    - Resistance raster
##    - Sources



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Protected area
PA <- st_read(dsn = file.path(spatialDataDir, 
                              "AiresProtegees_outaouais2023"),
              layer = "AiresProtegees_outaouais2023") %>%
  st_transform(crs("EPSG: 32198"))

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

# NOTE: There's not enough data to cover the entire extent of the MRC. Therefore
#       the available data is used to further refine the regional study area.

# Transform resistance into polygon, fill holes and merge overlapping shapes 
regionalExtent <- resistanceBLBR
regionalExtent[!is.na(regionalExtent)] <- 1
regionalExtent_polygon <- regionalExtent %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Keep overlapping areas
regionalMRC_extent <- st_intersection(regionalMRC_buffered,
                                      regionalExtent_polygon[1,])

# Set parameters for template raster
regionalTemplateCRS <- "EPSG: 32198"
regionalTemplateExt <- c(-691380, -469852, 172380, 403680)
regionalTemplateRes <- 30

# Create template raster
regionalTemplate <- rast()
crs(regionalTemplate) <- regionalTemplateCRS
ext(regionalTemplate) <- regionalTemplateExt
res(regionalTemplate) <- regionalTemplateRes

# Rasterize local study area
regionalArea <- rasterize(regionalMRC_extent, regionalTemplate)

# Crop & mask LULC to regional extent
regionalLULC <- LULC %>%
  crop(regionalArea) %>%
  resample(regionalArea) %>%
  mask(regionalArea)

# Subset water
regionalWater <- regionalLULC
regionalWater[regionalWater != 700] <- NA
regionalWater[is.na(regionalWater)] <- 0

# Individual MRC boundaries within the study area
regionalMRC_boundary <- st_intersection(regionalMRC, regionalMRC_extent)


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



# Protected areas --------------------------------------------------------------

# Regional ---------------------------------------

# Filter PA within local study area and merge boundaries
regionalPA <- PA %>%
  st_intersection(regionalMRC_extent) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Create raster
regionalPAraster <- rasterize(regionalPA, regionalTemplate)
regionalPAraster[is.na(regionalPAraster)] <- 0

# Combine PA and water
regionalPAandWater <- regionalPAraster + regionalWater

# Remove aquatic protected areas
regionalPAraster[regionalPAandWater == 701] <- NA

# Set NA values
regionalPAraster[regionalPAraster == 0] <- NA

# Detect patches
regionalPApatches <- patches(regionalPAraster)
patchesFreq <- freq(regionalPApatches)

# Filter small patches to remove artifacts
patchesFreq <- patchesFreq[patchesFreq$count < 100,]
regionalPAraster[which.lyr(regionalPApatches %in% patchesFreq$value)] <- NA

# Transform back into polygons, fill holes and merge overlapping shapes 
regionalPAnoWater <- regionalPAraster %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Set threshold distance to combine PA
distThreshold <- 500      # 0.5 km

# Calculate buffer around PA and union polygons
regionalPA_buffered <- regionalPAnoWater %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
regionalPA_combined <- regionalPA_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Calculate area of each polygon
regionalPA_combined$area <- as.vector(st_area(regionalPA_combined))

# Add unique ID to each PA
regionalPA_combined$PAid <- c(1:dim(regionalPA_combined)[1])

# Filter out small PA
targetRegionalPA <- regionalPA_combined %>%
  filter(area >= 1250*10000)   # 1250 hectares




# Local ------------------------------------------

# With aquatic PA -------------

# Filter PA within local study area and merge boundaries
localPA <- PA %>%
  st_intersection(localMRC_buffered) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Create raster
localPAraster <- rasterize(localPA, localTemplate)
localPAraster[is.na(localPAraster)] <- 0

# Create copy of PA raster to filter
localPAonLand <- localPAonWater <- localPAraster

# Combine PA and water
localPAandWater <- localPAraster + localWater

# Remove aquatic protected areas from terrestrial set
localPAonLand[localPAandWater == 701] <- NA
# Remove terrestrial protected areas from aquatic set
localPAonWater[localPAandWater != 701] <- NA

# Set NA values
localPAonLand[localPAonLand == 0] <- NA

# Detect patches
localPAonLand_patches <- patches(localPAonLand)
localPAonWater_patches <- patches(localPAonWater)
localPAonLand_patchesFreq <- freq(localPAonLand_patches)
localPAonWater_patchesFreq <- freq(localPAonWater_patches)

# Filter small patches to remove artifacts
localPAonLand_patchesFreq <- localPAonLand_patchesFreq[localPAonLand_patchesFreq$count < 100,]
localPAonLand[which.lyr(localPAonLand_patches %in% localPAonLand_patchesFreq$value)] <- NA
localPAonWater_patchesFreq <- localPAonWater_patchesFreq[localPAonWater_patchesFreq$count < 100,]
localPAonWater[which.lyr(localPAonWater_patches %in% localPAonWater_patchesFreq$value)] <- NA

# Transform back into polygons, fill holes and merge overlapping shapes 
localPAonLand_polygon <- localPAonLand %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")
localPAonWater_polygon <- localPAonWater %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Set threshold distance to combine PA
distThreshold <- 400      # 0.5 km

# Calculate buffer around PA and union polygons
localPAonLand_buffered <- localPAonLand_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)
localPAonWater_buffered <- localPAonWater_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
localPAonLand_combined <- localPAonLand_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")
localPAonWater_combined <- localPAonWater_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Combine terrestrial and aquatic PA
localPA_union <- st_union(localPAonLand_combined, localPAonWater_combined)

# Merge boundaries
localPA_all <- localPA_union %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry") %>%
  smoothr::fill_holes(threshold = Inf)

# Calculate area of each polygon
localPA_all$area <- as.vector(st_area(localPA_all))

# Add unique ID to each PA
localPA_all$PAid <- c(1:dim(localPA_all)[1])



# Without aquatic PA ----------

# Filter PA within local study area and merge boundaries
localPA <- PA %>%
  st_intersection(localMRC_buffered) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Transform back into polygons, fill holes and merge overlapping shapes 
localPAwithWater <- localPA %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Create raster
localPAraster <- rasterize(localPA, localTemplate)
localPAraster[is.na(localPAraster)] <- 0

# Combine PA and water
localPAandWater <- localPAraster + localWater

# Remove aquatic protected areas
localPAraster[localPAandWater == 701] <- NA

# Set NA values
localPAraster[localPAraster == 0] <- NA

# Detect patches
localPApatches <- patches(localPAraster)
patchesFreq <- freq(localPApatches)

# Filter small patches to remove artifacts
patchesFreq <- patchesFreq[patchesFreq$count < 100,]
localPAraster[which.lyr(localPApatches %in% patchesFreq$value)] <- NA

# Transform back into polygons, fill holes and merge overlapping shapes 
localPAnoWater <- localPAraster %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")
  
# Set threshold distance to combine PA
distThreshold <- 500      # 0.5 km

# Calculate buffer around PA and union polygons
localPA_buffered <- localPAnoWater %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
localPA_combined <- localPA_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Calculate area of each polygon
localPA_combined$area <- as.vector(st_area(localPA_combined))

# Add unique ID to each PA
localPA_combined$PAid <- c(1:dim(localPA_combined)[1])

# Filter out small PA
targetLocalPA <- localPA_combined %>%
  filter(area >= 750*10000)   # 750 hectares



# Resistance -------------------------------------------------------------------

# Function to crop & mask raster to study extent
crop_mask_raster <- function(resistanceRaster, studyExtent){
  
  localResistance <- resistanceRaster %>%
    crop(studyExtent) %>%
    resample(studyExtent) %>%
    mask(studyExtent)
  
  return(localResistance)
}

# Crop to study extent
# Regional
regionalResBLBR <- crop_mask_raster(resistanceRaster = resistanceBLBR, 
                                    studyExtent = regionalArea)
regionalResMAAM <- crop_mask_raster(resistanceRaster = resistanceMAAM, 
                                    studyExtent = regionalArea)
regionalResPCLI <- crop_mask_raster(resistanceRaster = resistancePLCI, 
                                    studyExtent = regionalArea)
regionalResRASY <- crop_mask_raster(resistanceRaster = resistanceRASY, 
                                    studyExtent = regionalArea)
regionalResURAM <- crop_mask_raster(resistanceRaster = resistanceURAM, 
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

# Re-scale resistance values
# Regional
regionalResBLBR_scaled1000 <- (regionalResBLBR*1000)/32
regionalResMAAM_scaled1000 <- (regionalResMAAM*1000)/32
regionalResPCLI_scaled1000 <- (regionalResPCLI*1000)/32
regionalResRASY_scaled1000 <- (regionalResRASY*1000)/32
regionalResURAM_scaled1000 <- (regionalResURAM*1000)/32
# Local
localResBLBR_scaled1000 <- (localResBLBR*1000)/32
localResMAAM_scaled1000 <- (localResMAAM*1000)/32
localResPCLI_scaled1000 <- (localResPCLI*1000)/32
localResRASY_scaled1000 <- (localResRASY*1000)/32
localResURAM_scaled1000 <- (localResURAM*1000)/32


# Write to file ----------------------------------------------------------------

# Study area extent
# Regional
st_write(obj = regionalMRC_buffered,
         dsn = file.path(intermediatesDir),
         layer = "regionalFullStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
st_write(obj = regionalMRC_boundary,
         dsn = file.path(intermediatesDir),
         layer = "regionalMRCinStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
st_write(obj = regionalMRC_extent,
         dsn = file.path(intermediatesDir),
         layer = "regionalStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Local
st_write(obj = localMRC_buffered,
         dsn = file.path(intermediatesDir),
         layer = "localStudyArea",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

# PA sources
# Regional
# All terrestrial
st_write(obj = regionalPA_combined[,-2], # Remove area attribute
         dsn = file.path(intermediatesDir, "Linkage Mapper"),
         layer = "regionalPAsources_allterr",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Filtered
st_write(obj = targetRegionalPA[,-2], # Remove area attribute
         dsn = file.path(intermediatesDir, "Linkage Mapper"),
         layer = "regionalPAsources_largeterr",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Local
# All terrestrial and aquatic
st_write(obj = localPA_all[,-2], # Remove area attribute
         dsn = file.path(intermediatesDir, "Linkage Mapper"),
         layer = "localPAsources_aquaterr",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# All terrestrial
st_write(obj = localPA_combined[,-2], # Remove area attribute
         dsn = file.path(intermediatesDir, "Linkage Mapper"),
         layer = "localPAsources_allterr",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Filtered
st_write(obj = targetLocalPA[,-2], # Remove area attribute
         dsn = file.path(intermediatesDir, "Linkage Mapper"),
         layer = "localPAsources_largeterr",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")


# Resistance layers
# Regional
writeRaster(regionalResBLBR_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "regionalResBLBR1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalResMAAM_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "regionalResMAAM1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalResPCLI_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "regionalResPCLI1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalResRASY_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "regionalResRASY1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(regionalResURAM_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "regionalResURAM1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
# Local
writeRaster(localResBLBR_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "localResBLBR1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResMAAM_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "localResMAAM1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResPCLI_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "localResPCLI1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResRASY_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "localResRASY1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)
writeRaster(localResURAM_scaled1000, 
            file.path(intermediatesDir, "Linkage Mapper", "localResURAM1000.tif"), 
            overwrite = TRUE, datatype = "INT2S", NAflag = -9999)


