## a312
## Carina Rauen Firkowski 
## April 10, 2024
##
## This script loads the species-specific current density maps from the A254 
## project and summarizes connectivity within the CCN corridors.



# Set working directory
setwd("C:/gitprojects/a312")

# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Current density
BLBRcurrent <- rast(file.path(spatialDataDir, "Current density",
                              "BLBR_currentdensity_30m.tif"))
MAAMcurrent <- rast(file.path(spatialDataDir, "Current density",
                              "MAAM_currentdensity_30m.tif"))
PLCIcurrent <- rast(file.path(spatialDataDir, "Current density",
                              "PLCI_currentdensity_30m.tif"))
RASYcurrent <- rast(file.path(spatialDataDir, "Current density",
                              "RASY_currentdensity_30m.tif"))
URAMcurrent <- rast(file.path(spatialDataDir, "Current density",
                              "URAM_currentdensity_30m.tif"))

# Resistance
templateRaster <- rast(file.path(spatialDataDir,
                                 "Resistance",
                                 "BLBR_resistance_30m.tif")) %>%
  project("EPSG: 32198")

# CCN corridors
CCNcorridors <- st_read(dsn = file.path(intermediatesDir),
              layer = "localCCNcorridors") %>%
  st_transform(crs("EPSG: 32198"))

# PA
PA <- st_read(dsn = file.path(spatialDataDir, 
                              "AiresProtegees_outaouais2023"),
              layer = "AiresProtegees_outaouais2023") %>%
  st_transform(crs("EPSG: 32198"))

# MRC
allMRCs <- st_read(dsn = file.path(spatialDataDir, 
                                   "MRCs"),
                   layer = "MRC_s_2021_02") %>%
  st_transform(crs("EPSG: 32198"))



# Fix CRS in current density maps ----------------------------------------------

# Get raster values as a vector
BLBRvalues <- BLBRcurrent[] %>% as.vector()
MAAMvalues <- MAAMcurrent[] %>% as.vector()
PLCIvalues <- PLCIcurrent[] %>% as.vector()
RASYvalues <- RASYcurrent[] %>% as.vector()
URAMvalues <- URAMcurrent[] %>% as.vector()

# Create raster from template with target values
BLBRcurrent_crs <- rast(templateRaster, vals = BLBRvalues)
MAAMcurrent_crs <- rast(templateRaster, vals = MAAMvalues)
PLCIcurrent_crs <- rast(templateRaster, vals = PLCIvalues)
RASYcurrent_crs <- rast(templateRaster, vals = RASYvalues)
URAMcurrent_crs <- rast(templateRaster, vals = URAMvalues)

# Set NA to 0
BLBRcurrent_crs[is.na(BLBRcurrent_crs)] <- 0
MAAMcurrent_crs[is.na(MAAMcurrent_crs)] <- 0
PLCIcurrent_crs[is.na(PLCIcurrent_crs)] <- 0
RASYcurrent_crs[is.na(RASYcurrent_crs)] <- 0
URAMcurrent_crs[is.na(URAMcurrent_crs)] <- 0



# Summarize current density within and outside CCN corridors -------------------

# Minimum convex polygon -------------------------

# Fix shapefile dimensions
CCNcorridors_polygons <- CCNcorridors %>%
  st_zm() %>%
  st_cast("POLYGON")

# Set threshold distance to combine corridors 12-14
distThreshold <- 100      # 100 m

# Calculate buffer around corridors and union polygons
CCNcorridors_buffered <- CCNcorridors_polygons[12:16,] %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
CCNcorridors_unique <- CCNcorridors_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Get attributes
attributes_df <- as.data.frame(CCNcorridors_polygons[12,])
CCNcorridors_unique <- cbind(CCNcorridors_unique, attributes_df[,-14])

# Drop split corridors from shapefile
CCNcorridors_continuous <- CCNcorridors_polygons[c(-12:-16),]
CCNcorridors_continuous <- rbind(CCNcorridors_continuous,
                                 CCNcorridors_unique)

# Set threshold distance to combine all corridors
distThreshold <- 10000      # 10 km

# Calculate buffer around corridors and union polygons
CCNcorridors_buffered <- CCNcorridors_polygons %>%
  st_buffer(dist = distThreshold) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  smoothr::fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
CCNcorridors_combined <- CCNcorridors_buffered %>%
  st_buffer(dist = -distThreshold) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Minimum convex polygon around CCN corridors
minConvexPolygon <- st_convex_hull(CCNcorridors_combined)

# Filter target MRC
targetMRC <- allMRCs %>%
  filter(MRS_NM_MRC == "Gatineau" | 
         MRS_NM_MRC == "Les Collines-de-l'Outaouais" |
         MRS_NM_MRC == "Pontiac")

# Remove areas beyond the MRC boundary
minConvexPolygon_mask <- st_intersection(minConvexPolygon, targetMRC)

# Merge boundaries
minConvexPolygon_merged <- minConvexPolygon_mask %>%
  st_union() %>%
  st_as_sf() %>%
  st_set_geometry("geometry")

# Mask and crop current density to minimum convex polygon 
BLBRcurrent_ext <- BLBRcurrent_crs %>%
  mask(minConvexPolygon_merged) %>%
  crop(minConvexPolygon_merged)
MAAMcurrent_ext <- MAAMcurrent_crs %>%
  mask(minConvexPolygon_merged) %>%
  crop(minConvexPolygon_merged)
PLCIcurrent_ext <- PLCIcurrent_crs %>%
  mask(minConvexPolygon_merged) %>%
  crop(minConvexPolygon_merged)
RASYcurrent_ext <- RASYcurrent_crs %>%
  mask(minConvexPolygon_merged) %>%
  crop(minConvexPolygon_merged)
URAMcurrent_ext <- URAMcurrent_crs %>%
  mask(minConvexPolygon_merged) %>%
  crop(minConvexPolygon_merged)



# Mask current density to CCN corridors ----------------------------------------

# Create list of each CCN corridor & save name
corridorList <- list()
corridorName <- data.frame(ID = as.numeric(),
                           Name = as.character())
for(p in 1:dim(CCNcorridors_continuous)[1]){
  
  # Isolate each polygon and save to list
  uniquePolygon <- CCNcorridors_continuous$geometry[p]
  corridorList <- append(corridorList, list(uniquePolygon))
  
  # Save name of each corridor
  tempDF <- data.frame(ID = p,
                       Name = CCNcorridors_continuous$NAME[p])
  corridorName <- rbind(corridorName, tempDF)

}

# Remove "Corridor de/du/d'" from name
corridorName$Name <- str_remove(corridorName$Name, "Corridor ")
corridorName$Name <- str_remove(corridorName$Name, "du ")
corridorName$Name <- str_remove(corridorName$Name, "de ")
corridorName$Name <- str_remove(corridorName$Name, "d'")

# Fix case
corridorName$Name[corridorName$Name == "ruisseau Breckenridge"] <- "Ruisseau Breckenridge"

# Add sorting order
corridorName$CorridorOrder <- c(5, 6, 3, 1, 11, 9, 10, 8, 7, 4, 2, 12)
corridorName$AlphabeticalOrder <- c(10, 2, 11, 3, 12, 8, 4, 6, 7, 5, 1, 9)

# Function to convert polygon to raster and mask current density to corridor
maskCCN = function(rasterObject, polygonObject, inverse = FALSE){
  vectorizedPolygon <- vect(polygonObject)
  maskedRaster <- mask(rasterObject, vectorizedPolygon, inverse = inverse)
}

# Mask current density to CCN corridors
BLBRcurrent_CCNmask <- lapply(
  corridorList,
  function(x) maskCCN(rasterObject = BLBRcurrent_ext, 
                      polygonObject = x))
MAAMcurrent_CCNmask <- lapply(
  corridorList,
  function(x) maskCCN(rasterObject = MAAMcurrent_ext, 
                      polygonObject = x))
PLCIcurrent_CCNmask <- lapply(
  corridorList,
  function(x) maskCCN(rasterObject = PLCIcurrent_ext, 
                      polygonObject = x))
RASYcurrent_CCNmask <- lapply(
  corridorList,
  function(x) maskCCN(rasterObject = RASYcurrent_ext, 
                      polygonObject = x))
URAMcurrent_CCNmask <- lapply(
  corridorList,
  function(x) maskCCN(rasterObject = URAMcurrent_ext, 
                      polygonObject = x))



# Summarize current density within each corridor -------------------------------

# Function to calculate mean of current per corridor and count number of cells
corridor_summary <- function(sppCurrentList){
  
  # Empty dataframes 
  summaryPerCorridor <- data.frame(ID = as.numeric(),
                                   NumberOfCells = as.numeric(),
                                   Mean = as.numeric(),
                                   CIlower = as.numeric(),
                                   CIupper = as.numeric(),
                                   Significance = as.numeric())
  
  # For each corridor in the list
  for(m in 1:length(sppCurrentList)){
    
    # Get corridor raster
    uniqueMask <- sppCurrentList[[m]]
    
    # Calculate number of cells in the corridor
    numberCells <- as.numeric(freq(uniqueMask)[3])
    
    # Get current values and remove NAs
    currentValues <- uniqueMask[]
    currentValues <- currentValues[!is.na(currentValues)]
    
    # Calculate mean current density in the corridor
    meanCurrent <- mean(currentValues)
    
    # Combine results
    tempDF <- data.frame(ID = m,
                         NumberOfCells = numberCells,
                         Mean = meanCurrent,
                         CIlower = NA,
                         CIupper = NA,
                         Significance = NA)
    summaryPerCorridor <- rbind(summaryPerCorridor, tempDF)
    
  }
  return(summaryPerCorridor)
}

# Function to obtain the values from each corridor
corridor_values <- function(sppCurrentList){
  
  # Empty dataframes 
  valuesPerCorridor <- data.frame(ID = as.numeric(),
                                  Value = as.numeric())
  
  # For each corridor in the list
  for(m in 1:length(sppCurrentList)){
    
    # Get corridor raster
    uniqueMask <- sppCurrentList[[m]]
    
    # Get current values and remove NAs
    currentValues <- uniqueMask[]
    currentValues <- currentValues[!is.na(currentValues)]
    
    # Combine results
    tempDF <- data.frame(ID = m,
                         Value = (currentValues))
    valuesPerCorridor <- rbind(valuesPerCorridor, tempDF)
    
  }
  return(valuesPerCorridor)
}

# Calculate summary and get values per corridor
BLBR_summaryPerCorridor <- corridor_summary(BLBRcurrent_CCNmask)
BLBR_summaryPerCorridor$Species <- "BLBR"
BLBR_valuesPerCorridor <- corridor_values(BLBRcurrent_CCNmask)
BLBR_valuesPerCorridor$Species <- "BLBR"
MAAM_summaryPerCorridor <- corridor_summary(MAAMcurrent_CCNmask)
MAAM_summaryPerCorridor$Species <- "MAAM"
MAAM_valuesPerCorridor <- corridor_values(MAAMcurrent_CCNmask)
MAAM_valuesPerCorridor$Species <- "MAAM"
PLCI_summaryPerCorridor <- corridor_summary(PLCIcurrent_CCNmask)
PLCI_summaryPerCorridor$Species <- "PLCI"
PLCI_valuesPerCorridor <- corridor_values(PLCIcurrent_CCNmask)
PLCI_valuesPerCorridor$Species <- "PLCI"
RASY_summaryPerCorridor <- corridor_summary(RASYcurrent_CCNmask)
RASY_summaryPerCorridor$Species <- "RASY"
RASY_valuesPerCorridor <- corridor_values(RASYcurrent_CCNmask)
RASY_valuesPerCorridor$Species <- "RASY"
URAM_summaryPerCorridor <- corridor_summary(URAMcurrent_CCNmask)
URAM_summaryPerCorridor$Species <- "URAM"
URAM_valuesPerCorridor <- corridor_values(URAMcurrent_CCNmask)
URAM_valuesPerCorridor$Species <- "URAM"

# Combine summary per corridor
summaryPerCorridor <- rbind(BLBR_summaryPerCorridor,
                            MAAM_summaryPerCorridor,
                            PLCI_summaryPerCorridor,
                            RASY_summaryPerCorridor,
                            URAM_summaryPerCorridor)

# Combine corridor values
valuesPerCorridor <- rbind(BLBR_valuesPerCorridor,
                           MAAM_valuesPerCorridor,
                           PLCI_valuesPerCorridor,
                           RASY_valuesPerCorridor,
                           URAM_valuesPerCorridor)

# Summary per species
summaryPerSpecies <- valuesPerCorridor %>%
  group_by(Species) %>%
  summarize(Mean = mean(Value))



# Estimate current density outside each corridor -------------------------------

# Remove all CCN corridors from current density maps
BLBRcurrent_nonCCNmask <- mask(BLBRcurrent_ext, 
                               CCNcorridors_continuous, inverse = TRUE)
MAAMcurrent_nonCCNmask <- mask(MAAMcurrent_ext, 
                               CCNcorridors_continuous, inverse = TRUE)
PLCIcurrent_nonCCNmask <- mask(PLCIcurrent_ext, 
                               CCNcorridors_continuous, inverse = TRUE)
RASYcurrent_nonCCNmask <- mask(RASYcurrent_ext, 
                               CCNcorridors_continuous, inverse = TRUE)
URAMcurrent_nonCCNmask <- mask(URAMcurrent_ext, 
                               CCNcorridors_continuous, inverse = TRUE)

# Isolate Gatineau PA
gatineauPA <- PA[PA$NOM == "Gatineau",]
gatineauPA <- gatineauPA %>%
  st_cast("POLYGON")

# Remove Gatineau from current density maps
BLBRcurrent_nonCCNmask <- mask(BLBRcurrent_nonCCNmask, 
                               gatineauPA, inverse = TRUE)
MAAMcurrent_nonCCNmask <- mask(MAAMcurrent_nonCCNmask, 
                               gatineauPA, inverse = TRUE)
PLCIcurrent_nonCCNmask <- mask(PLCIcurrent_nonCCNmask, 
                               gatineauPA, inverse = TRUE)
RASYcurrent_nonCCNmask <- mask(RASYcurrent_nonCCNmask, 
                               gatineauPA, inverse = TRUE)
URAMcurrent_nonCCNmask <- mask(URAMcurrent_nonCCNmask, 
                               gatineauPA, inverse = TRUE)

# Function to sample current density outside each corridor
nonCorridor_sample = function(summaryPerCorridor, sppCurrentList, spp){
  
  # Empty dataframe
  sampleNonCorridor <- data.frame(ID = as.numeric(),
                                  Value = as.numeric(),
                                  Species = as.character())
  
  # For each corridor, 
  # sample current density in number of cells equal to corridor size
  for(i in 1:12){
    
    # Repeating 100 times
    for(loopID in 1:200){
      
      # Get corridor size
      sampleSize <- summaryPerCorridor$NumberOfCells[summaryPerCorridor$ID == i]
      
      # Take sample
      oneSample <- spatSample(sppCurrentList, 
                              sampleSize,
                              method = "random", na.rm = TRUE)
      
      # Remove NA values
      oneSampleValues <- oneSample[]
      oneSampleValues <- oneSampleValues[!is.na(oneSampleValues)]
      
      # Calculate mean
      meanValue <- mean(oneSampleValues, na.rm = TRUE)
      
      # Combine results
      tempDF <- data.frame(ID = i,
                           Value = meanValue, 
                           Species = spp)
      sampleNonCorridor <- rbind(sampleNonCorridor, tempDF)
      
    }
  }
  return(sampleNonCorridor)
}

# Sample current density outside outside each corridor
BLBR_sampleNonCorridor <- nonCorridor_sample(BLBR_summaryPerCorridor,
                                             BLBRcurrent_nonCCNmask,
                                             "BLBR")
MAAM_sampleNonCorridor <- nonCorridor_sample(MAAM_summaryPerCorridor,
                                             MAAMcurrent_nonCCNmask,
                                             "MAAM")
PLCI_sampleNonCorridor <- nonCorridor_sample(PLCI_summaryPerCorridor,
                                             PLCIcurrent_nonCCNmask,
                                             "PLCI")
RASY_sampleNonCorridor <- nonCorridor_sample(RASY_summaryPerCorridor,
                                             RASYcurrent_nonCCNmask,
                                             "RASY")
URAM_sampleNonCorridor <- nonCorridor_sample(URAM_summaryPerCorridor,
                                             URAMcurrent_nonCCNmask, 
                                             "URAM")

# Combine non-corridor sample across species
sampleNonCorridor <- rbind(BLBR_sampleNonCorridor,
                           MAAM_sampleNonCorridor,
                           PLCI_sampleNonCorridor,
                           RASY_sampleNonCorridor,
                           URAM_sampleNonCorridor)
rm(BLBR_sampleNonCorridor)
rm(MAAM_sampleNonCorridor)
rm(PLCI_sampleNonCorridor)
rm(RASY_sampleNonCorridor)
rm(URAM_sampleNonCorridor)

# Function to calculate 95% confidence interval
calculate_CI = function(vectorObject){
  sampleMean <- mean(vectorObject)
  sampleSD <- sd(vectorObject)
  lowerBound <- sampleMean - (1.96*sampleSD)
  upperBound <- sampleMean + (1.96*sampleSD)
  return(c(lowerBound, upperBound))
}

# Function to test significance of current density within vs. outside corridors
corridor_significance = function(nonCorridorSample = sampleNonCorridor, 
                                 summaryPerCorridor, 
                                 groupingVar){
  
  # Set grouping variable list
  if(groupingVar == "Corridor"){
    iList <- 1:12
    columnID <- 1
  }
  if(groupingVar == "Species"){
    iList <- c("BLBR", "MAAM", "PLCI", "RASY", "URAM")
    columnID <- 3
  }
  
  # For each group variable, calculate 95% C.I. of current density outside 
  # corridors and see if it contains the mean current density insider corridors
  for(i in iList){
    
    # Subset current density sample outside corridor
    targetCorridor_data <- nonCorridorSample[nonCorridorSample[,columnID] == i,]
    
    # Calculate 95% C.I.
    targetCorridor_CI <- calculate_CI(targetCorridor_data[,2])
    
    # Add C.I. values to per corridor summary dataframe
    summaryPerCorridor$CIlower[summaryPerCorridor[,1] == i] <- 
      targetCorridor_CI[1]
    summaryPerCorridor$CIupper[summaryPerCorridor[,1] == i] <- 
      targetCorridor_CI[2]
    
    # Get corridor mean current density
    if(groupingVar == "Corridor"){
      testValue <- summaryPerCorridor$Mean[summaryPerCorridor$ID == i]
    }
    if(groupingVar == "Species"){
      testValue <- summaryPerCorridor$Mean[summaryPerCorridor$Species == i]
    }
    
    # Test significance
    if(testValue < targetCorridor_CI[1]){
      significanceResult <- "-"
    }
    if(testValue > targetCorridor_CI[2]){
      significanceResult <- "+"
    }
    if(testValue >= targetCorridor_CI[1] & testValue <= targetCorridor_CI[2]){
      significanceResult <- NA
    } 
    
    summaryPerCorridor$Significance[summaryPerCorridor[,1] == i] <- 
      significanceResult

  }
  return(summaryPerCorridor)
}

# Calculate significance of current density per corridor
BLBR_summaryPerCorridor <- corridor_significance(
  summaryPerCorridor = BLBR_summaryPerCorridor,
  groupingVar = "Corridor")
MAAM_summaryPerCorridor <- corridor_significance(
  summaryPerCorridor = MAAM_summaryPerCorridor,
  groupingVar = "Corridor")
PLCI_summaryPerCorridor <- corridor_significance(
  summaryPerCorridor = PLCI_summaryPerCorridor,
  groupingVar = "Corridor")
RASY_summaryPerCorridor <- corridor_significance(
  summaryPerCorridor = RASY_summaryPerCorridor,
  groupingVar = "Corridor")
URAM_summaryPerCorridor <- corridor_significance(
  summaryPerCorridor = URAM_summaryPerCorridor,
  groupingVar = "Corridor")

# Calculate significance of current density per species
summaryPerSpecies <- corridor_significance(summaryPerCorridor = summaryPerSpecies,
                                           groupingVar = "Species")



# Plot current density per corridor and per species ----------------------------

# Function to detect outlier values
calculate_outliers = function(x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}

# Function to detect outlier
detect_outliers_ID = function(valuesPerCorridor){
  
  outlierDF <- valuesPerCorridor %>% 
    group_by(ID) %>%
    mutate(outlier = calculate_outliers(Value)) %>%
    ungroup()
  
  return(outlierDF)
  
}

# Detect outlier values
BLBR_outliers <- detect_outliers_ID(BLBR_valuesPerCorridor)
MAAM_outliers <- detect_outliers_ID(MAAM_valuesPerCorridor)
PLCI_outliers <- detect_outliers_ID(PLCI_valuesPerCorridor)
RASY_outliers <- detect_outliers_ID(RASY_valuesPerCorridor)
URAM_outliers <- detect_outliers_ID(URAM_valuesPerCorridor)

# Merge outlier values with corridor names
BLBR_outlierAndName <- merge(BLBR_outliers, corridorName, by = "ID")
MAAM_outlierAndName <- merge(MAAM_outliers, corridorName, by = "ID")
PLCI_outlierAndName <- merge(PLCI_outliers, corridorName, by = "ID")
RASY_outlierAndName <- merge(RASY_outliers, corridorName, by = "ID")
URAM_outlierAndName <- merge(URAM_outliers, corridorName, by = "ID")

# Combine results across species
outlierAndName <- rbind(BLBR_outlierAndName, 
                        MAAM_outlierAndName,
                        PLCI_outlierAndName,
                        RASY_outlierAndName,
                        URAM_outlierAndName)

# Function to plot result
plot_perCorridor = function(valuesPerCorridor, summaryPerCorridor, outliers,
                            corridorNameDF = corridorName){
  
  # Merge values per corridor with corridor significance
  valuesPerCorridor_sign <- merge(valuesPerCorridor,
                                  summaryPerCorridor[,c("ID", "Significance")],
                                  by = "ID")
  # Merge values per corridor with corridor names
  valuesPerCorridor_name <- merge(valuesPerCorridor_sign, 
                                  corridorNameDF, 
                                  by = "ID")
  
  # Color match for current density significance
  colorDictionary <- data.frame(Significance = c("+", "-", NA),
                                ColourCode = c("palegreen", "#F8766D", "grey"))
  
  # Merge summary per corridor with colour dictionary
  summaryPerCorridor_color <- summaryPerCorridor %>%
    merge(colorDictionary, by = "Significance") %>%
    merge(corridorNameDF, by = "ID") %>%
    arrange(AlphabeticalOrder)
  
  # Set corridor order & levels
  corridorNameDF <- corridorNameDF %>%
      arrange(CorridorOrder)
  levelOrder <- corridorNameDF$Name
  # Set alphabetical order & palette 
  significancePalette <- summaryPerCorridor_color$ColourCode
  
  # Plot current density per corridor with colour-coded significance 
  perCorridorPlot <- ggplot(valuesPerCorridor_name, 
         aes(x = factor(Name,  level = levelOrder), y = Value)) +
    labs(y = "Densité de courant", x = "Corridor") +
    geom_boxplot(aes(fill = as.factor(Name)), 
                 outlier.color = NA, show.legend = F) + 
    geom_point(data = outliers[outliers$outlier,], 
               aes(color = as.factor(Name)),
               alpha = 0.5, show.legend = F) +
    scale_colour_manual(values = significancePalette) +
    scale_fill_manual(values = significancePalette) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  return(perCorridorPlot)
}
  
BLBR_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = BLBR_valuesPerCorridor,
  summaryPerCorridor = BLBR_summaryPerCorridor,
  outliers = BLBR_outlierAndName)
#plot(BLBR_perCorridorPlot)
MAAM_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = MAAM_valuesPerCorridor,
  summaryPerCorridor = MAAM_summaryPerCorridor,
  outliers = MAAM_outlierAndName)
#plot(MAAM_perCorridorPlot)
PLCI_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = PLCI_valuesPerCorridor,
  summaryPerCorridor = PLCI_summaryPerCorridor,
  outliers = PLCI_outlierAndName)
#plot(PLCI_perCorridorPlot)
RASY_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = RASY_valuesPerCorridor,
  summaryPerCorridor = RASY_summaryPerCorridor,
  outliers = RASY_outlierAndName)
#plot(RASY_perCorridorPlot)
URAM_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = URAM_valuesPerCorridor,
  summaryPerCorridor = URAM_summaryPerCorridor,
  outliers = URAM_outlierAndName)
#plot(URAM_perCorridorPlot)


# Single plot current density per corridor across species ----------------------

# Function to merge values per corridor with corridor names
merge_valuesAndName = function(valuesPerCorridor, summaryPerCorridor,
                               corridorNameDF = corridorName){
  
  # Merge values per corridor with corridor significance
  valuesPerCorridor_sign <- merge(valuesPerCorridor,
                                  summaryPerCorridor[,c("ID", "Significance")],
                                  by = "ID")
  # Merge values per corridor with corridor names
  valuesPerCorridor_name <- merge(valuesPerCorridor_sign, 
                                  corridorName, 
                                  by = "ID")
  
  return(valuesPerCorridor_name)
}

BLBR_valuesAndName <- merge_valuesAndName(
  valuesPerCorridor = BLBR_valuesPerCorridor,
  summaryPerCorridor = BLBR_summaryPerCorridor)
MAAM_valuesAndName <- merge_valuesAndName(
  valuesPerCorridor = MAAM_valuesPerCorridor,
  summaryPerCorridor = MAAM_summaryPerCorridor)
PLCI_valuesAndName <- merge_valuesAndName(
  valuesPerCorridor = PLCI_valuesPerCorridor,
  summaryPerCorridor = PLCI_summaryPerCorridor)
RASY_valuesAndName <- merge_valuesAndName(
  valuesPerCorridor = RASY_valuesPerCorridor,
  summaryPerCorridor = RASY_summaryPerCorridor)
URAM_valuesAndName <- merge_valuesAndName(
  valuesPerCorridor = URAM_valuesPerCorridor,
  summaryPerCorridor = URAM_summaryPerCorridor)

# Combine results across species
valuesAndName <- rbind(BLBR_valuesAndName, 
                       MAAM_valuesAndName,
                       PLCI_valuesAndName,
                       RASY_valuesAndName,
                       URAM_valuesAndName)

# Function to merge summary per corridor with color dictionary and corridor name
create_signPalette = function(spp_summaryPerCorridor, 
                              corridorNameDF = corridorName){
  
  # Color match for current density significance
  colorDictionary <- data.frame(Significance = c("+", "-", NA),
                                ColourCode = c("palegreen", "#F8766D", "grey"))
  
  # Merge summary per corridor with colour dictionary
  summaryPerCorridor_color <- spp_summaryPerCorridor %>%
    merge(colorDictionary, by = "Significance") %>%
    merge(corridorNameDF, by = "ID") %>%
    arrange(AlphabeticalOrder)
  
  # Set alphabetical order & palette 
  significancePalette <- summaryPerCorridor_color$ColourCode
  
  return(significancePalette)
}

BLBR_signPalette <- create_signPalette(BLBR_summaryPerCorridor)
MAAM_signPalette <- create_signPalette(MAAM_summaryPerCorridor)
PLCI_signPalette <- create_signPalette(PLCI_summaryPerCorridor)
RASY_signPalette <- create_signPalette(RASY_summaryPerCorridor)
URAM_signPalette <- create_signPalette(URAM_summaryPerCorridor)

# Combine results across species
signPalette <- c(BLBR_signPalette, 
                 MAAM_signPalette,
                 PLCI_signPalette,
                 RASY_signPalette,
                 URAM_signPalette)

# Set corridor order & levels
corridorName <- corridorName %>%
  arrange(CorridorOrder)
levelOrder <- corridorName$Name

# Plot current density per corridor with colour-coded significance 
singleCorridorsPlot <- ggplot(valuesAndName, 
       aes(x = factor(Name,  level = levelOrder), 
           y = Value)) +
  labs(y = "Flux de mouvement", x = "Corridor") +
  geom_boxplot(aes(fill = interaction(Name, Species)),
               outlier.color = NA, show.legend = F, 
               lwd = 0.5) + 
  geom_point(data = outlierAndName[outlierAndName$outlier,], 
             aes(color = interaction(Name, Species)),
             alpha = 0.5, position=position_jitterdodge(jitter.width=0), 
             show.legend = F) +
  scale_colour_manual(values = signPalette) +
  scale_fill_manual(values = signPalette) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#plot(singleCorridorsPlot)

ggsave("singlePlot.png", 
       plot = singleCorridorsPlot, device = "png", width = 10, height = 6, units = "in")



# Plot current density across all corridors per species ------------------------

# Function to detect outlier
outlier_species <- valuesPerCorridor %>% 
    group_by(Species) %>%
    mutate(outlier = calculate_outliers(Value)) %>%
    ungroup()

# Color match for current density significance
colorDictionary <- data.frame(Significance = c("+", "-", NA),
                              ColourCode = c("palegreen", "#F8766D", "grey"))

# Merge summary per corridor with colour dictionary
summaryPerSpecies_color <- summaryPerSpecies %>%
  merge(colorDictionary, by = "Significance")

# Set palette following alphabetical order
significancePalette <- summaryPerSpecies_color$ColourCode

# Plot current density per corridor with colour-coded significance 
perSpeciesPlot <- ggplot(valuesPerCorridor, 
       aes(x = factor(Species), 
           y = Value)) +
  labs(y = "Flux de mouvement", x = "Espèces") +
  geom_boxplot(aes(fill = Species),
               outlier.color = NA, show.legend = T, 
               lwd = 0.5) + 
  geom_point(data = outlier_species[outlier_species$outlier,], 
             aes(color = Species),
             alpha = 0.5, position=position_jitterdodge(jitter.width=0), 
             show.legend = F) +
  scale_colour_manual(values = significancePalette) +
  scale_fill_manual(values = significancePalette) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#plot(perSpeciesPlot)

ggsave("perSpeciesPlot.png", 
       plot = perSpeciesPlot, device = "png", 
       width = 6, height = 6, units = "in")




# Write to file ----------------------------------------------------------------

st_write(obj = minConvexPolygon_merged, # Remove area attribute
         dsn = file.path(intermediatesDir, "Boundaries"),
         layer = "minConvexPolygon",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")


writeRaster(BLBRcurrent_nonCCNmask, 
            file.path(intermediatesDir, "Resistance", 
                      "BLBRcurrent_nonCCNmask.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)

BLBRcurrent_singleCCNmask <- mask(BLBRcurrent_ext, 
                                  CCNcorridors_continuous, inverse = FALSE)
writeRaster(BLBRcurrent_singleCCNmask, 
            file.path(intermediatesDir, "Resistance", 
                      "BLBRcurrent_singleCCNmask.tif"), 
            overwrite = TRUE, datatype = "FLT8S", NAflag = -9999)

