# a312
# Bronwyn Rayfield, Carina Firkowski, & Sarah Chisholm (ApexRMS)
# Run with R-4.2.2

# Load constants, functions, etc
source("./Scripts/0-constants.R")

# Workspace ----

# Read in data
# Spatial
studyAreaRast <- rast(file.path(intermediatesDir, "Boundaries", "missingData_studyArea.tif"))
landcoverRastRaw <- rast(file.path(spatialDataDir, "LULC", "utilisation_territoire_2018.tif"))
aqRoadsRaw <- vect(x = file.path(spatialDataDir, "AQreseau_SHP", "AQ_ROUTES.shp"))
ecoForestryVectRaw <- vect(x = file.path(spatialDataDir, "Ecoforestry", "CARTE_ECO_MAJ_PROV_10_GDB", "CARTE_ECO_MAJ_PROV_10.gdb"), 
                           layer = "PEE_MAJ_PROV",
                           filter = as.polygons(studyAreaRast)) 

# Tabular
forestAgeReclass <- read_csv(file.path(tabularDataDir, "forestAgeReclassSIEF.csv"))
depositReclass <- read_csv(file.path(tabularDataDir, "depositReclass.csv"))
forestDensityReclass <- read_csv(file.path(tabularDataDir, "densityReclass.csv"))
drainageReclass <- read_csv(file.path(tabularDataDir, "drainageReclass.csv"))

# Reformatting data ----
## Clip rasters ----
landcoverRastClip <- landcoverRastRaw %>% 
  terra::project(y = crs(studyAreaRast),
                 method = "near") %>% 
  crop(y = studyAreaRast,
       snap = "near") %>% 
  mask(mask = studyAreaRast)

landcoverRastClip[landcoverRastClip == -1] <- 0

## Clip and/or rasterize shapefiles ----
### AQ Roads ----
aqRoadsCrop <- aqRoadsRaw %>% 
  terra::project(y = crs(studyAreaRast)) %>% 
  crop(as.polygons(studyAreaRast))

### Forest age ----
# Rasterize ecoForestryVectRaw based on field "CL_AGE"
forestAgeRaster <- ecoForestryVectRaw %>%
  # Join ecoForestryVectRaw with forestAgeCrosswalk
  terra::merge(forestAgeReclass, all.x = TRUE) %>%
  rasterize(y = studyAreaRast,
            field = "Code")

# Assign NoData cells a value of 0
forestAgeRaster[is.na(forestAgeRaster)] <- 0

# Mask by analysis area
forestAgeRaster <- forestAgeRaster %>% 
  mask(mask = studyAreaRast)

### Surficial deposits ----
surficialDepositsRaster <- ecoForestryVectRaw %>%
  terra::merge(depositReclass, all.x = TRUE) %>%
  rasterize(y = studyAreaRast,
            field = "Recode")

# Assign NoData cells a value of 0
surficialDepositsRaster[is.na(surficialDepositsRaster)] <- 0

# Mask by analysis area
surficialDepositsRaster <- surficialDepositsRaster %>% 
  mask(mask = studyAreaRast)

### Drainage ----
drainageRaster <- ecoForestryVectRaw %>%
  terra::merge(drainageReclass, all.x = TRUE) %>%
  rasterize(y = studyAreaRast,
            field = "CODE")

# Assign NoData cells a value of 0
drainageRaster[is.na(drainageRaster)] <- 0

# Mask by analysis area
drainageRaster <- drainageRaster %>% 
  mask(mask = studyAreaRast)

### Forest density ----
# Rasterize ecoForestryVectRaw based on field "CL_DENS"
forestDensityRaster <- ecoForestryVectRaw %>% 
  # Join ecoForestryVectRaw with forestDensityCrosswalk
  terra::merge(forestDensityReclass, all.x = TRUE) %>% 
  rasterize(y = studyAreaRast,
            field = "Code")

# Assign NoData cells a value of 0 (fill in missing CL_DENS values)
forestDensityRaster[is.na(forestDensityRaster)] <- 0

# Mask by analysis area
forestDensityRaster <- forestDensityRaster %>% 
  mask(mask = studyAreaRast) 

# Save outputs ----
# Landcover
writeRaster(landcoverRastClip, 
            file.path(intermediatesDir, "landcover.tif"), 
            overwrite = TRUE)
# Forest age
writeRaster(forestAgeRaster, 
            file.path(intermediatesDir, "forest-age.tif"), 
            overwrite = TRUE)
# Surficial deposits
writeRaster(surficialDepositsRaster, 
            file.path(intermediatesDir, "surficial-deposits-update.tif"), 
            overwrite = TRUE)
# Drainage
writeRaster(drainageRaster, 
            file.path(intermediatesDir, "drainage.tif"), 
            overwrite = TRUE)
# Forest density
writeRaster(forestDensityRaster, 
            file.path(intermediatesDir, "forest-density.tif"), 
            overwrite = TRUE)
#AQ Roads
writeVector(aqRoadsCrop,
            file.path(intermediatesDir, "AQ Roads", "aq-roads.shp"),
            overwrite = TRUE)
