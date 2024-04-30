## a312
## Carina Rauen Firkowski 
## March 26, 2024
##
## This script prepares the sources for the corridor delimitation
## analyses, at the local and regional scales.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
regionalArea <- st_read(dsn = file.path(intermediatesDir, "Boundaries"),
                        layer = "regionalFullStudyArea")
localArea <- st_read(dsn = file.path(intermediatesDir, "Boundaries"),
                     layer = "localStudyArea")

# Public protected areas
PA <- st_read(dsn = file.path(spatialDataDir, 
                              "AiresProtegees_outaouais2023"),
              layer = "AiresProtegees_outaouais2023") %>%
  st_transform(crs("EPSG: 32198"))

# Kenauk Nature
KN <- st_read(dsn = file.path(spatialDataDir, 
                              "KenaukNature_Complete"),
              layer = "KenaukNature_Complete") %>%
  st_transform(crs("EPSG: 32198"))

# RMNAT
RMNAT <- st_read(dsn = file.path(spatialDataDir, 
                                 "RMNATSitesRepertoire_Outaouais_20240411"),
                 layer = "SitesRepertoire_Outaouais_20240411") %>%
  st_transform(crs("EPSG: 32198"))

# Algonquin Park
algonquin <- st_read(dsn = file.path(spatialDataDir, 
                                     "Ontario"),
                     layer = "PROV_PARK_REGULATED") %>%
  st_transform(crs("EPSG: 32198")) %>%
  filter(SHORT_NAME == "ALGONQUIN")

# LULC
LULC <- rast(file.path(spatialDataDir,
                       "LULC",
                       "b03-landcover-30m.tif")) %>%
  project("EPSG: 32198")



# Protected areas --------------------------------------------------------------

# RMNAT ------------------------------------------

# Filter RMNAT areas close to Gatineau Park so they don't get merged
RMNAT_filtered <- RMNAT %>%
  filter(ID_SITE != "A0342" & 
           ID_SITE != "A0782" &
           ID_SITE != "A0783" &
           ID_SITE != "A1414" &
           ID_SITE != "A1414-1" &
           ID_SITE != "A1493" &
           ID_SITE != "A1526" &
           ID_SITE != "A1547" &
           ID_SITE != "A1584" &
           ID_SITE != "A1611" &
           ID_SITE != "A1641" &
           ID_SITE != "H0004" &
           ID_SITE != "H0005")

# Save them as a subset to add to local scale
RMNAT_subset <- RMNAT %>%
  filter(ID_SITE == "A0342" | 
           ID_SITE == "A0782" |
           ID_SITE == "A0783" |
           #ID_SITE == "A1414" |    # These two are excluded for being to small
           #ID_SITE == "A1414-1" |
           ID_SITE == "A1493" |
           ID_SITE == "A1526" |
           ID_SITE == "A1547" |
           ID_SITE == "A1584" |
           ID_SITE == "A1611" |
           ID_SITE == "A1641" |
           ID_SITE == "H0004" |
           ID_SITE == "H0005")

# Merge boundaries
RMNAT_merged <- RMNAT_filtered %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")



# Regional ---------------------------------------

# Filter PA within analysis area and merge boundaries
regionalPA <- PA %>%
  filter(MACODE != "7881") %>%
  st_intersection(regionalArea) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

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

# Rasterize RMNAT & Kenauk Nature 
regionalRMNATraster <- rasterize(RMNAT_merged, regionalTemplate)
regionalRMNATraster[is.na(regionalRMNATraster)] <- 0
regionalKNraster <- rasterize(KN, regionalTemplate)
regionalKNraster[is.na(regionalKNraster)] <- 0

# Crop section of Algonquin within study area
regionalAlgonquin <- algonquin %>%
  st_intersection(regionalArea)

# Rasterize Algonquin Park
algonquinRaster <- rasterize(regionalAlgonquin, regionalTemplate)
algonquinRaster[is.na(algonquinRaster)] <- 0

# Combine PA, Kenauk Nature and RMNAT
regionalSources <- regionalPAraster + regionalRMNATraster + regionalKNraster
regionalSources[regionalSources == 2] <- 1

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

# Combine sources and water
regionalPAandWater <- regionalSources + regionalWater

# Create copy of sources raster to filter
regionalSourcesonLand <- regionalSourcesonWater <- regionalSources

# Remove aquatic protected areas
regionalSourcesonLand[regionalPAandWater == 701] <- NA
# Remove terrestrial protected areas from aquatic set
regionalSourcesonWater[regionalPAandWater != 701] <- NA

# Set NA values
regionalSourcesonLand[regionalSourcesonLand == 0] <- NA

# Detect patches
regionalSourcesonLand_patches <- patches(regionalSourcesonLand)
regionalSourcesonLand_freq <- freq(regionalSourcesonLand_patches)
regionalSourcesonWater_patches <- patches(regionalSourcesonWater)
regionalSourcesonWater_freq <- freq(regionalSourcesonWater_patches)

# Filter small patches to remove artifacts
regionalSourcesonLand_freq <- regionalSourcesonLand_freq[
  regionalSourcesonLand_freq$count < 100,]
regionalSourcesonLand[which.lyr(
  regionalSourcesonLand_patches %in% regionalSourcesonLand_freq$value)] <- NA
regionalSourcesonWater_freq <- regionalSourcesonWater_freq[
  regionalSourcesonWater_freq$count < 100,]
regionalSourcesonWater[which.lyr(
  regionalSourcesonWater_patches %in% regionalSourcesonWater_freq$value)] <- NA

# Transform back into polygons, fill holes and merge overlapping shapes 
regionalSourcesonLand_polygon <- regionalSourcesonLand %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")
regionalSourcesonWater_polygon <- regionalSourcesonWater %>%
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
regionalSourcesonLand_buffered <- regionalSourcesonLand_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)
regionalSourcesonWater_buffered <- regionalSourcesonWater_polygon %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
regionalSourcesonLand_combined <- regionalSourcesonLand_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")
regionalSourcesonWater_combined <- regionalSourcesonWater_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Calculate area of each polygon
regionalSourcesonLand_combined$area <- as.vector(
  st_area(regionalSourcesonLand_combined))
regionalSourcesonWater_combined$area <- as.vector(
  st_area(regionalSourcesonWater_combined))

# Filter out small aquatic PA
regionalSourcesonLand_filtered <- regionalSourcesonLand_combined %>%
  filter(area >= 1180*10000)   # 1250 hectares 1180
regionalSourcesonWater_filtered <- regionalSourcesonWater_combined %>%
  filter(area >= 750*10000)   # 750 hectares

# Rasterize
regionalLandRaster <- rasterize(regionalSourcesonLand_filtered,
                                regionalTemplate)
regionalLandRaster[is.na(regionalLandRaster)] <- 0
regionalWaterRaster <- rasterize(regionalSourcesonWater_filtered,
                                 regionalTemplate)
regionalWaterRaster[is.na(regionalWaterRaster)] <- 0

# Combine terrestrial and aquatic sources with Algonquin Park 
regionalSourceRaster <- regionalLandRaster + regionalWaterRaster + 
  algonquinRaster
regionalSourceRaster[regionalSourceRaster == 0] <- NA
regionalSourceRaster[regionalSourceRaster == 2] <- 1

# Transform back into polygons
regionalSource <- regionalSourceRaster %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Add unique ID to each source
regionalSource$PAid <- c(1:dim(regionalSource)[1])



# Local ------------------------------------------

# Filter PA within local study area and merge boundaries
# NOTE: Remove area close to Gatineau so it doesn't get merged
localPA_filtered <- PA %>%
  filter(MACODE != "7881") %>%
  st_intersection(localArea) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")
# NOTE: Subset area close to Gatineau to add to sources after merge step
localPA_subset <- PA %>%
  filter(MACODE == "7881") %>%
  st_intersection(localArea) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Set parameters for template raster
localTemplateCRS <- "EPSG: 32198"
localTemplateExt <- c(-618919, -524331, 176931, 243819)
localTemplateRes <- 30

# Create template raster
localTemplate <- rast()
crs(localTemplate) <- localTemplateCRS
ext(localTemplate) <- localTemplateExt
res(localTemplate) <- localTemplateRes

# Create raster
localPAraster <- rasterize(localPA_filtered, localTemplate)
localPAraster[is.na(localPAraster)] <- 0
# Create raster
localArearaster <- rasterize(localArea, localTemplate)

# Intersect RMNAT areas with analysis area
localRMNAT <- RMNAT_merged %>%
  st_intersection(localArea)

# Rasterize RMNAT
localRMNATraster <- rasterize(localRMNAT, localTemplate)
localRMNATraster[is.na(localRMNATraster)] <- 0

# Combine PA and RMNAT
localSources <- localPAraster + localRMNATraster
localSources[localSources == 2] <- 1

# Crop & mask LULC to local extent
localLULC <- LULC %>%
  crop(localArearaster) %>%
  resample(localArearaster) %>%
  mask(localArearaster)

# Subset water
localWater <- localLULC
localWater[localWater != 700] <- NA
localWater[is.na(localWater)] <- 0

# Combine PA and water
localPAandWater <- localSources + localWater

# Create copy of PA raster to filter
localPAonLand <- localPAonWater <- localSources

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
localPAonLand_patchesFreq <- localPAonLand_patchesFreq[
  localPAonLand_patchesFreq$count < 100,]
localPAonLand[which.lyr(localPAonLand_patches %in% 
                          localPAonLand_patchesFreq$value)] <- NA
localPAonWater_patchesFreq <- localPAonWater_patchesFreq[
  localPAonWater_patchesFreq$count < 100,]
localPAonWater[which.lyr(localPAonWater_patches %in% 
                           localPAonWater_patchesFreq$value)] <- NA

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
distThreshold <- 400      # 0.4 km

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

# Rasterize RMNAT and turn back into polygon to merge contiguous areas
RMNAT_raster <- rasterize(RMNAT_subset, localTemplate)
RMNAT_polygon <- RMNAT_raster %>%
  as.polygons(values = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf) %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry")

# Add in PA near Gatineau Park 
localPAonLand_recombined <- rbind(localPAonLand_combined, 
                                  localPA_subset, RMNAT_polygon)

# Combine terrestrial and aquatic PA
localPA_union <- st_union(localPAonLand_recombined, localPAonWater_combined)

# Merge boundaries
localPA_all <- localPA_union %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  st_set_geometry("geometry") %>%
  smoothr::fill_holes(threshold = Inf)

# Add unique ID to each PA
localPA_all$PAid <- c(1:dim(localPA_all)[1])



# Write to file ----------------------------------------------------------------

# Regional sources
# All
st_write(obj = regionalSource,
         dsn = file.path(intermediatesDir, "Linkage Mapper", "Final"),
         layer = "regionalSourcesFull",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Terrestrial sources only for mapping
st_write(obj = regionalSourcesonLand_filtered,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "regionalSourcesTerrestrial",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Aquatic sources only for mapping
st_write(obj = regionalSourcesonWater_filtered,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "regionalSourcesAquatic",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

# Local sources
# All
st_write(obj = localPA_all,
         dsn = file.path(intermediatesDir, "Linkage Mapper", "Final"),
         layer = "localSourcesAll",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Terrestrial sources only for mapping
st_write(obj = localPAonLand_recombined,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "localSourcesTerrestrial",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# Aquatic sources only for mapping
st_write(obj = localPAonWater_combined,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "localSourcesAquatic",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")
# RMNAT sources only for mapping
st_write(obj = RMNAT_polygon,
         dsn = file.path(intermediatesDir, "QGIS"),
         layer = "localSourcesRMNAT",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")



