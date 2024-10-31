## a312
## Carina Rauen Firkowski 
## October 30, 2024
##
## This script adds additional sources to the reginal scale analysis, while
## filtering the U-shaped source to the north-west of the territory.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
regionalArea <- st_read(dsn = file.path(intermediatesDir, "Boundaries"),
                        layer = "regionalFullStudyArea")

# Original sources
originalSources <- st_read(dsn = file.path(intermediatesDir, 
                                           "Linkage Mapper", "Final"),
         layer = "regionalSourcesFull")

# Additional protected areas
additionalPA <- st_read(dsn = file.path(spatialDataDir, 
                                        "sources_add_oct2024_CREDDO"),
                        layer = "sources_add_oct2024_CREDDO") %>%
  st_transform(crs("EPSG: 32198"))

# LULC
LULC <- rast(file.path(spatialDataDir,
                       "LULC",
                       "b03-landcover-30m.tif")) %>%
  project("EPSG: 32198")

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

# Create raster
regionalPAraster <- rasterize(regionalPA, regionalTemplate)
regionalPAraster[is.na(regionalPAraster)] <- 0



# Prepare original sources -----------------------------------------------------

# Remove U-shaped source
originalSourcesFilter <- originalSources %>%
  filter(PAid != "7")

# Rasterize
originalSourcesRaster <- rasterize(originalSourcesFilter,
                                   regionalTemplate)
originalSourcesRaster[is.na(originalSourcesRaster)] <- 0



# Prepare water raster ---------------------------------------------------------

# Create raster
regionalArearaster <- rasterize(regionalArea, regionalTemplate)

# Crop & mask LULC to regional extent
regionalLULC <- LULC %>%
  crop(regionalArearaster) %>%
  resample(regionalArearaster) %>%
  mask(regionalArearaster)

# Subset water
regionalWater <- regionalLULC
regionalWater[regionalWater != 700] <- NA
regionalWater[is.na(regionalWater)] <- 0



# Prepare additional sources ---------------------------------------------------

# Filter PA within analysis area and merge boundaries
regionalAdd <- additionalPA %>%
  st_intersection(regionalArea) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Create raster for additional sources
regionalAddraster <- rasterize(regionalAdd, regionalTemplate)
regionalAddraster[is.na(regionalAddraster)] <- 0

# Combine sources and water
regionalAddandWater <- regionalAddraster + regionalWater

# Create copy of sources raster to filter
regionalAddonLand <- regionalAddonWater <- regionalAddraster

# Remove aquatic protected areas
regionalAddonLand[regionalAddandWater == 701] <- NA
# Remove terrestrial protected areas from aquatic set
regionalAddonWater[regionalAddandWater != 701] <- NA

# Set NA values
regionalAddonLand[regionalAddonLand == 0] <- NA

# Detect patches
regionalAddonLand_patches <- patches(regionalAddonLand)
regionalAddonLand_freq <- freq(regionalAddonLand_patches)
regionalAddonWater_patches <- patches(regionalAddonWater)
regionalAddsonWater_freq <- freq(regionalAddonWater_patches)

# Filter small patches to remove artifacts
regionalAddonLand_freq <- regionalAddonLand_freq[
  regionalAddonLand_freq$count < 100,]
regionalAddonLand[which.lyr(
  regionalAddonLand_patches %in% regionalAddonLand_freq$value)] <- NA
regionalAddsonWater_freq <- regionalAddsonWater_freq[
  regionalAddsonWater_freq$count < 100,]
regionalAddonWater[which.lyr(
  regionalAddonWater_patches %in% regionalAddsonWater_freq$value)] <- NA

# Transform back into polygons, fill holes and merge overlapping shapes 
regionalAddonLand_polygon <- regionalAddonLand %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")
regionalAddonWater_polygon <- regionalAddonWater %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Set threshold distance to combine PA
distThreshold <- 400      # 0.4 km

# Calculate buffer around PA and union polygons
regionalAddonLand_buffered <- regionalAddonLand_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)
regionalAddonWater_buffered <- regionalAddonWater_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
regionalAddonLand_combined <- regionalAddonLand_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")
regionalAddonWater_combined <- regionalAddonWater_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Calculate area of each polygon
regionalAddonLand_combined$area <- as.vector(
  st_area(regionalAddonLand_combined))
regionalAddonWater_combined$area <- as.vector(
  st_area(regionalAddonWater_combined))

# Filter out small aquatic PA
regionalAddonLand_filtered <- regionalAddonLand_combined %>%
  filter(area >= 600*10000)   # 600 hectares
regionalAddonWater_filtered <- regionalAddonWater_combined %>%
  filter(area >= 750*10000)   # 750 hectares

# Rasterize
regionalAddLandRaster <- rasterize(regionalAddonLand_filtered,
                                   regionalTemplate)
regionalAddLandRaster[is.na(regionalAddLandRaster)] <- 0
regionalAddWaterRaster <- rasterize(regionalAddonWater_filtered,
                                    regionalTemplate)
regionalAddWaterRaster[is.na(regionalAddWaterRaster)] <- 0

# Combine terrestrial and aquatic sources 
regionalAddRaster <- regionalAddLandRaster + regionalAddWaterRaster
regionalAddRaster[regionalAddRaster == 2] <- 1



# Combine original and additional sources --------------------------------------

# Combine sources
regionalSourceUpdatedRaster <- originalSourcesRaster + regionalAddRaster
regionalSourceUpdatedRaster[regionalSourceUpdatedRaster == 0] <- NA

# Transform back into polygons
regionalSourceUpdated <- regionalSourceUpdatedRaster %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Add unique ID to each source
regionalSourceUpdated$PAid <- c(1:dim(regionalSourceUpdated)[1])



# Save to file -----------------------------------------------------------------

# Regional sources
# All
st_write(obj = regionalSourceUpdated,
         dsn = file.path(intermediatesDir, "Linkage Mapper", "Final"),
         layer = "regionalSourcesFullUpdated",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Terrestrial sources only for mapping
st_write(obj = regionalAddonLand_filtered,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "regionalSourcesTerrestrialAdd",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Aquatic sources only for mapping
st_write(obj = regionalAddonWater_filtered,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "regionalSourcesAquaticAdd",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

