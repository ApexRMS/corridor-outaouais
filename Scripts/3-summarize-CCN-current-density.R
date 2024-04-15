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

# Mask and crop current density to minimum convex polygon 
BLBRcurrent_ext <- BLBRcurrent_crs %>%
  mask(minConvexPolygon) %>%
  crop(minConvexPolygon)
MAAMcurrent_ext <- MAAMcurrent_crs %>%
  mask(minConvexPolygon) %>%
  crop(minConvexPolygon)
PLCIcurrent_ext <- PLCIcurrent_crs %>%
  mask(minConvexPolygon) %>%
  crop(minConvexPolygon)
RASYcurrent_ext <- RASYcurrent_crs %>%
  mask(minConvexPolygon) %>%
  crop(minConvexPolygon)
URAMcurrent_ext <- URAMcurrent_crs %>%
  mask(minConvexPolygon) %>%
  crop(minConvexPolygon)



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
corridorName$AlphabeticalOrder <- c(3, 1, 11, 5, 10, 2, 7, 6, 8, 4, 12, 9)

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
BLBR_valuesPerCorridor <- corridor_values(BLBRcurrent_CCNmask)
MAAM_summaryPerCorridor <- corridor_summary(MAAMcurrent_CCNmask)
MAAM_valuesPerCorridor <- corridor_values(MAAMcurrent_CCNmask)
PLCI_summaryPerCorridor <- corridor_summary(PLCIcurrent_CCNmask)
PLCI_valuesPerCorridor <- corridor_values(PLCIcurrent_CCNmask)
RASY_summaryPerCorridor <- corridor_summary(RASYcurrent_CCNmask)
RASY_valuesPerCorridor <- corridor_values(RASYcurrent_CCNmask)
URAM_summaryPerCorridor <- corridor_summary(URAMcurrent_CCNmask)
URAM_valuesPerCorridor <- corridor_values(URAMcurrent_CCNmask)



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

# Function to calculate mean current density outside each corridor
nonCorridor_summary = function(summaryPerCorridor, sppCurrentList){
  
  # Empty dataframe
  summaryNonCorridor <- data.frame(ID = as.numeric(),
                                   Mean = as.numeric())
  
  # For each corridor, 
  # sample current density in number of cells equal to corridor size
  for(i in 1:12){
    
    # Repeating 100 times
    for(loopID in 1:100){
      
      # Get corridor size
      sampleSize <- summaryPerCorridor$NumberOfCells[summaryPerCorridor$ID == i]
      
      # Take sample
      oneSample <- spatSample(sppCurrentList, 
                              sampleSize,
                              method = "random", na.rm = TRUE)
      
      # Calculate mean
      meanValue <- mean(oneSample[,1])
      
      # Combine results
      tempDF <- data.frame(ID = i,
                           Mean = meanValue)
      summaryNonCorridor <- rbind(summaryNonCorridor, tempDF)
      
    }
  }
  return(summaryNonCorridor)
} 
  
# Calculate mean current density outside each corridor
BLBR_summaryNonCorridor <- nonCorridor_summary(BLBR_summaryPerCorridor,
                                               BLBRcurrent_nonCCNmask)
MAAM_summaryNonCorridor <- nonCorridor_summary(MAAM_summaryPerCorridor,
                                               MAAMcurrent_nonCCNmask)
PLCI_summaryNonCorridor <- nonCorridor_summary(PLCI_summaryPerCorridor,
                                               PLCIcurrent_nonCCNmask)
RASY_summaryNonCorridor <- nonCorridor_summary(RASY_summaryPerCorridor,
                                               RASYcurrent_nonCCNmask)
URAM_summaryNonCorridor <- nonCorridor_summary(URAM_summaryPerCorridor,
                                               URAMcurrent_nonCCNmask)

# Function to calculate 95% confidence interval
calculate_CI = function(vectorObject){
  sampleMean <- mean(vectorObject)
  sampleSD <- sd(vectorObject)
  lowerBound <- sampleMean - (1.96*sampleSD)
  upperBound <- sampleMean + (1.96*sampleSD)
  return(c(lowerBound, upperBound))
}

# Function to test significance of current density within vs. outside corridors
corridor_significance = function(summaryNonCorridor, summaryPerCorridor){
  
  # For each corridor, calculate 95% C.I. of current density outside corridors
  # and see if it contains the mean current density of each corridor
  for(i in 1:12){
    
    # Subset current density sample outside corridor
    targetCorridor_data <- summaryNonCorridor[summaryNonCorridor$ID == i,]
    
    # Calculate 95% C.I.
    targetCorridor_CI <- calculate_CI(targetCorridor_data$Mean)
    
    # Add C.I. values to per corridor summary dataframe
    summaryPerCorridor$CIlower[summaryPerCorridor$ID == i] <- 
      targetCorridor_CI[1]
    summaryPerCorridor$CIupper[summaryPerCorridor$ID == i] <- 
      targetCorridor_CI[2]
    
    # Get corridor mean current density
    testValue <- summaryPerCorridor$Mean[summaryPerCorridor$ID == i]
    
    if(testValue < targetCorridor_CI[1]){
      significanceResult <- "-"
    }
    if(testValue > targetCorridor_CI[2]){
      significanceResult <- "+"
    }
    if(testValue >= targetCorridor_CI[1] & testValue <= targetCorridor_CI[2]){
      significanceResult <- NA
    } 
    
    summaryPerCorridor$Significance[summaryPerCorridor$ID == i] <- 
      significanceResult
    
  }
  return(summaryPerCorridor)
}

# Calculate significance of current density per corridor
BLBR_summaryPerCorridor <- corridor_significance(BLBR_summaryNonCorridor,
                                                 BLBR_summaryPerCorridor)
MAAM_summaryPerCorridor <- corridor_significance(MAAM_summaryNonCorridor,
                                                 MAAM_summaryPerCorridor)
PLCI_summaryPerCorridor <- corridor_significance(PLCI_summaryNonCorridor,
                                                 PLCI_summaryPerCorridor)
RASY_summaryPerCorridor <- corridor_significance(RASY_summaryNonCorridor,
                                                 RASY_summaryPerCorridor)
URAM_summaryPerCorridor <- corridor_significance(URAM_summaryNonCorridor,
                                                 URAM_summaryPerCorridor)



# Plot -------------------------------------------------------------------------

# Function to detect outlier values
calculate_outliers = function(x) {
  x < quantile(x, .25) - 1.5 * IQR(x) |
    x > quantile(x, .75) + 1.5 * IQR(x)
}

# Function to detect outlier
detect_outliers = function(valuesPerCorridor){
  
  outlierDF <- valuesPerCorridor %>% 
    group_by(ID) %>%
    mutate(outlier = calculate_outliers(Value)) %>%
    ungroup()
  
  return(outlierDF)
  
}

# Detect outliers
BLBR_outliers <- detect_outliers(BLBR_valuesPerCorridor)
MAAM_outliers <- detect_outliers(MAAM_valuesPerCorridor)
PLCI_outliers <- detect_outliers(PLCI_valuesPerCorridor)
RASY_outliers <- detect_outliers(RASY_valuesPerCorridor)
URAM_outliers <- detect_outliers(URAM_valuesPerCorridor)

# Function to plot result
plot_perCorridor = function(valuesPerCorridor, summaryPerCorridor, outliers,
                            corridorNameDF = corridorName){
  
  # Merge values per corridor with corridor significance
  valuesPerCorridor_sign <- merge(valuesPerCorridor,
                                  summaryPerCorridor[,c("ID", "Significance")],
                                  by = "ID")
  # Merge values per corridor with corridor names
  valuesPerCorridor_name <- merge(valuesPerCorridor_sign, 
                                  corridorName, 
                                  by = "ID")
  
  # Merge values per corridor with corridor names
  outliers_name <- merge(outliers, corridorName, by = "ID")
  
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
    geom_point(data = outliers_name[outliers_name$outlier,], 
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
  outliers = BLBR_outliers)
MAAM_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = MAAM_valuesPerCorridor,
  summaryPerCorridor = MAAM_summaryPerCorridor,
  outliers = MAAM_outliers)
PLCI_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = PLCI_valuesPerCorridor,
  summaryPerCorridor = PLCI_summaryPerCorridor,
  outliers = PLCI_outliers)
RASY_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = RASY_valuesPerCorridor,
  summaryPerCorridor = RASY_summaryPerCorridor,
  outliers = RASY_outliers)
URAM_perCorridorPlot <- plot_perCorridor(
  valuesPerCorridor = URAM_valuesPerCorridor,
  summaryPerCorridor = URAM_summaryPerCorridor,
  outliers = URAM_outliers)


