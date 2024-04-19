# a312
# Bronwyn Rayfield, Carina Firkowski, & Sarah Chisholm (ApexRMS)
# Run with R-4.2.2                               

# Load constants, functions, etc
source("./Scripts/0-constants.R")

# Workspace ----

# Raw data filenames
#forestageName <- "forest-age-code.tif"
#forestDensityName <- "forest-density.tif"
#surficialdepositName <- "surficial-deposits.tif"
#drainageName <- "drainage.tif"
landcoverName <- "landcover.tif"
roadName <- "aq-roads.shp"
studyareaName <- "missingData_studyArea.tif"

#privatelandName <- "RMN_20210608.shp"
#protectedareaName <- "AP_REG_S_20210824.shp"
#landcoverBTSLPolyName <- "BTSL_SLL_Occ_sol_Land_cover.shp"
#landcoverBTSLName <- "BTSL-SLL-Occ-sol-Land-cover.tif"
#countyName <- "MRC_s_2021_02.shp"
#extentName <- "OutaouaisConnectivityExtent-30m.tif"

# GRASS setup ----
if(doGRASSSetup){
  #https://gis.stackexchange.com/questions/183032/create-a-new-grass-database-in-r-with-crs-projection
  # Manually set up empty GRASS database - see GRASSTemplate
  initGRASS(gisBase=gisBase, gisDbase=gisDbase, location=grassLocation, mapset='PERMANENT', override=TRUE)
  
  execGRASS("g.proj", georef=file.path(intermediatesDir, "Boundaries", studyareaName), flags="c")
  
  # Initialize new mapset inheriting projection info
  execGRASS("g.mapset", mapset = "RawData", flags="c")
  
  # SC: Only load land cover and roads? Nothing else gets modified
  # Load layers into grass database
  execGRASS("v.in.ogr", input=file.path(intermediatesDir, "AQ Roads", roadName), output="rawDataRoad", flags=c("overwrite", "o"))
  #execGRASS("v.in.ogr", input=file.path(b03RawMapsDir, protectedareaName), output="rawDataProtectedArea", flags=c("overwrite", "o"))
  #execGRASS("v.in.ogr", input=file.path(b03RawMapsDir, countyName), output="rawDataCounty", flags=c("overwrite", "o"))
  #execGRASS("v.in.ogr", input=file.path(b03RawMapsDir, landcoverBTSLPolyName), output="rawDataLandcoverBTSLPoly", flags=c("overwrite", "o"))
  #execGRASS("v.in.ogr", input=file.path(b03RawMapsDir, privatelandName), output="rawDataPrivateLand", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(b03RawMapsDir, extentName), output="rawDataExtent", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(intermediatesDir, forestageName), output="rawDataForestAge", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(intermediatesDir, forestDensityName), output="rawDataForestDensity", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(b03RawMapsDir, landcoverBTSLName), output="rawDataLandcoverBTSL", flags=c("overwrite", "o"))
  execGRASS("r.in.gdal", input=file.path(intermediatesDir, landcoverName), output="rawDataLandcover", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(b01b02RawMapsDir, landcoverBufferFillName), output="rawDataLandcoverBufferFill", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(intermediatesDir, "Boundaries", studyareaName), output="rawDataStudyArea", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(intermediatesDir, surficialdepositName), output="rawDataSurficialDeposits", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(intermediatesDir, drainageName), output="rawDataDrainage", flags=c("overwrite", "o"))
  #execGRASS("r.in.gdal", input=file.path(b03RawMapsDir, ontarioMaskName), output="rawDataOntarioMask", flags=c("overwrite", "o"))
}else{
  initGRASS(gisBase=gisBase, gisDbase=gisDbase, location=grassLocation, mapset="RawData", override=TRUE)
}

# Set the geographic region
execGRASS('g.region', n='465990', e='-499200', w='-729390', s='262950', res='30') # This data should already be set when running line 53



# check your geographic location
execGRASS("g.region", flags = "p") # only included in the working file, to be removed later

# Reclassify layers ----
# NB No need to reclassify forest density or drainage
# Study area
# Fix NA values in study area layer: i.e. 2, 3, and 4 = NULL
# rclStudyArea <- read.csv(file.path("b01b02", "data", "tables", "studyareaReclass.csv"), header=TRUE)[,c('Value','Code')]
# write.table(rclStudyArea, file="b01b02/data/tables/studyareaRules.txt", sep="=", col.names=FALSE, quote=FALSE, row.names=FALSE)
# execGRASS('r.reclass', input='rawDataStudyArea', output='b03-studyArea-test', rules=file.path("b01b02", "data", "tables", "studyareaRules.txt"), flags=c('overwrite'))

# SC: Reclass was done in previous script
# Forest age - reclassify
# rclAge<-read.csv(file.path(b01b02RawTablesDir, "forestAgeReclass.csv"), header=TRUE)[,c('Value','Code')]
# write.table(rclAge, file=file.path(b01b02RawTablesDir, "forestAgeRules.txt"), sep="=", col.names=FALSE, quote=FALSE, row.names=FALSE)
# execGRASS('r.reclass', input='rawDataForestAge', output='forestAge', rules=file.path(b01b02RawTablesDir, "forestAgeRules.txt"), flags=c('overwrite'))

# SC: Reclass was done in previous script
# Surficial deposit - reclassify
# rclDeposit<-read.csv(file.path(b01b02RawTablesDir, "depositReclass.csv"),header=TRUE)[,c('Value','Recode')]
# write.table(rclDeposit, file=file.path(b01b02RawTablesDir, "depositRules.txt"), sep="=", col.names=FALSE, quote=FALSE, row.names=FALSE)
# execGRASS('r.reclass', input='rawDataSurficialDeposits', output='surficialDeposit', rules=file.path(b01b02RawTablesDir, "depositRules.txt"), flags=c('overwrite'))

#############
# Landcover #
#############
# Reclassify landcover in Quebec to match buffer classes
rcl<-read.csv(file.path(tabularDataDir, "landcoverQuebecReclass.csv"),header=TRUE)[,c('Code','Recode')]
write.table(paste0(rcl[,'Code'],'=',rcl[,'Recode']),file.path(tabularDataDir,'landCoverQuebecReclassRule.txt'),sep="",col.names=FALSE,quote=FALSE,row.names=FALSE)
execGRASS('r.reclass',input='rawDataLandcover',output='landcover',rules=file.path(tabularDataDir,'landCoverQuebecReclassRule.txt'),flags=c('overwrite'))

#SC: no buffer data
## Fill out buffer with Quebec landcover
#execGRASS('r.patch', input="rawDataLandcoverBufferFill,landcoverBufferClasseGen", output='landcoverBufferFilled', flags=c('overwrite'))

# Reclassify landcover to match Albert et al. classes
rcl<-read.csv(file.path(tabularDataDir, "landcoverBufferReclass.csv"),header=TRUE)[,c('Value','Code')]
write.table(paste0(rcl[,'Value'],'=',rcl[,'Code']),file.path(tabularDataDir, 'landcoverBufferReclassRule.txt'),sep="",col.names=FALSE,quote=FALSE,row.names=FALSE)
execGRASS('r.reclass',input='landcover',output='landcoverReclassed' ,rules=file.path(tabularDataDir, 'landcoverBufferReclassRule.txt'),flags=c('overwrite'))




# Landcover BTSL ----
# Reclass tables
speciesLandcoverReclass <- read.csv(file.path(tabularDataDir, "speciesLandcoverReclass.csv"), header=TRUE)
#landcoverReclassRaw <- read.csv(file.path(b01b02RawTablesDir, "landcoverBTSLReclass.csv"), header=TRUE)
#roadReclassBDTQ <- read.csv(file.path(b01b02RawTablesDir, "roadReclassBDTQ.csv"), header=TRUE)
roadReclassAQ <- read.csv(file.path(tabularDataDir, "roadReclassAQ.csv"), header=TRUE)

# SC: Not using BTSL data
# # Reclassify landcover in BTSL to match Albert et al. classes
# landcoverReclass <- landcoverReclassRaw[, c("Value", "Code")]
# write.table(landcoverReclass, file=file.path(b01b02RawTablesDir, "landReclassRule.txt"), sep="=", col.names=FALSE, quote=FALSE, row.names=FALSE)
# execGRASS('r.reclass', input='rawDataLandcoverBTSL', output='landcoverBTSLReclass', rules=file.path(b01b02RawTablesDir, "landReclassRule.txt"), flags=c('overwrite'))

# # Fix broken linear features by extracting them, rasterizing them at higher resolution, and then reimposing them on the landcover raster
# # Extract linear agriculture (i.e. "Milieu agricole non cultiv?")
# agLinearCode <- speciesLandcoverReclass$LandcoverCode[speciesLandcoverReclass$LandcoverName == "AgricultureLinearElements"]
# agLinearDetailedClasses <- landcoverReclassRaw$SQL_CLASSE_DET[landcoverReclassRaw$Code == agLinearCode]
# whereString <- paste0("CLASSE_DET IN ('",paste(agLinearDetailedClasses, collapse="','"),"')")
# execGRASS("v.extract", input="rawDataLandcoverBTSLPoly", layer='1', type="area", where=whereString, output="landcoverAgLinearPoly", flags=c("overwrite"))
# execGRASS('g.region', res='2')
# execGRASS("v.to.rast", input="landcoverAgLinearPoly", use="val", value=agLinearCode, output="landcoverAgLinearRas_2m", flags=c("overwrite"))
# execGRASS('g.region', res=paste0(myResolution))
# execGRASS('r.resamp.stats', input="landcoverAgLinearRas_2m", output=paste0("landcoverAgLinearRas_", myResolution, "m"), method="maximum", flags=c("overwrite"))
# # Impose linear agriculture on landcover map
# execGRASS('r.patch', input=paste0("landcoverAgLinearRas_", myResolution, "m,landcoverBTSLReclass"), output=paste0("landcoverBTSL", myResolution, "m"), flags=c("overwrite"))

# # Extract roads from landcover map
minorRoadCode <- speciesLandcoverReclass$LandcoverCode[speciesLandcoverReclass$LandcoverName == "MinorRoads"]
majorRoadCode <- speciesLandcoverReclass$LandcoverCode[speciesLandcoverReclass$LandcoverName == "MajorRoads"]
# # Minor
# roadMinorDetailedClasses <- landcoverReclassRaw$SQL_CLASSE_DET[landcoverReclassRaw$Code == minorRoadCode]
# whereString = paste0("CLASSE_DET IN ('",paste(roadMinorDetailedClasses, collapse="','"),"')")
# execGRASS("v.extract", input="rawDataLandcoverBTSLPoly", layer='1', type="area", where=whereString, output="landcoverMinorRoadPASLPoly", flags=c("overwrite"))
# execGRASS('g.region', res='2')
# execGRASS("v.to.rast", input="landcoverMinorRoadPASLPoly", use="val", value=minorRoadCode, output="landcoverMinorRoadPASLRas_2m", flags=c("overwrite"))
# execGRASS('g.region', res=paste0(myResolution))
# execGRASS('r.resamp.stats', input="landcoverMinorRoadPASLRas_2m", output=paste0("landcoverMinorRoadPASLRas_", myResolution, "m"), method="maximum", flags=c("overwrite"))
# # Major
# roadMajorDetailedClasses <- landcoverReclassRaw$SQL_CLASSE_DET[landcoverReclassRaw$Code == majorRoadCode]
# whereString = paste0("CLASSE_DET IN ('",paste(roadMajorDetailedClasses, collapse="','"),"')")
# execGRASS("v.extract", input="rawDataLandcoverBTSLPoly", layer='1', type="area", where=whereString, output="landcoverMajorRoadPASLPoly", flags=c("overwrite"))
# execGRASS('g.region', res='2')
# execGRASS("v.to.rast", input="landcoverMajorRoadPASLPoly", use="val", value=majorRoadCode, output="landcoverMajorRoadPASLRas_2m", flags=c("overwrite"))
# execGRASS('g.region', res=paste0(myResolution))
# execGRASS('r.resamp.stats', input="landcoverMajorRoadPASLRas_2m", output=paste0("landcoverMajorRoadPASLRas_", myResolution, "m"), method="maximum", flags=c("overwrite"))
# # All roads PASL
# execGRASS('r.mapcalc', expression=paste0("landcoverRoadPASL1 = if(isnull(landcoverMajorRoadPASLRas_", myResolution, "m),0,landcoverMajorRoadPASLRas_", myResolution, "m) + if(isnull(landcoverMinorRoadPASLRas_", myResolution, "m),0,landcoverMinorRoadPASLRas_", myResolution, "m)"), flags=c("overwrite"))
# write.table(c('0=0', paste0(minorRoadCode, '=', minorRoadCode), paste0(majorRoadCode, '=', majorRoadCode), paste0(minorRoadCode + majorRoadCode, '=', majorRoadCode)), paste0(b03RawTablesDir, '/roadPASLrule.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
# execGRASS('r.reclass', input='landcoverRoadPASL1', output='landcoverRoadPASL', rules=paste0(b03RawTablesDir, '/roadPASLrule.txt'), flags=c('overwrite'))

# # Add roads from AQ database
# # Minor
roadMinorDetailedClasses <- roadReclassAQ$ClsRte_SQL[roadReclassAQ$Code == minorRoadCode]
whereString = paste0("ClsRte IN ('",paste(roadMinorDetailedClasses, collapse="','"),"')")
execGRASS("v.extract", input="rawDataRoad", type="line", where=whereString, output="landcoverMinorRoadAQ", flags=c("overwrite"))
execGRASS('g.region', res='2')
execGRASS("v.to.rast", input="landcoverMinorRoadAQ", use="val", value=minorRoadCode, output="landcoverMinorRoadAQRas_2m", flags=c("overwrite"))
execGRASS('g.region', res='30')
execGRASS('r.resamp.stats', input="landcoverMinorRoadAQRas_2m", output=paste0("landcoverMinorRoadAQRas_30m"), method="maximum", flags=c("overwrite"))
# Major
roadMajorDetailedClasses <- roadReclassAQ$ClsRte_SQL[roadReclassAQ$Code == majorRoadCode]
whereString = paste0("ClsRte IN ('",paste(roadMajorDetailedClasses, collapse="','"),"')")
execGRASS("v.extract", input="rawDataRoad", type="line", where=whereString, output="landcoverMajorRoadAQ", flags=c("overwrite"))
execGRASS('g.region', res='2')
execGRASS("v.to.rast", input="landcoverMajorRoadAQ", use="val", value=majorRoadCode, output="landcoverMajorRoadAQRas_2m", flags=c("overwrite"))
execGRASS('g.region', res='30')
execGRASS('r.resamp.stats', input="landcoverMajorRoadAQRas_2m", output=paste0("landcoverMajorRoadAQRas_", '30', "m"), method="maximum", flags=c("overwrite"))

#execGRASS('r.mask', raster="rawDataOntarioMask",flags=c("i","overwrite"))
#execGRASS('r.mapcalc', expression=paste0("landcoverMajorRoadAQRasClip_30m = landcoverMajorRoadAQRas_", '30', "m * 1"), flags=c("overwrite"))
#execGRASS('r.mask', raster="rawDataOntarioMask",flags=c("r"))

# All roads AQ
execGRASS('r.mapcalc', expression=paste0("landcoverRoadAQ1 = if(isnull(landcoverMajorRoadAQRas_", '30', "m),0,landcoverMajorRoadAQRas_", '30', "m) + if(isnull(landcoverMinorRoadAQRas_", '30', "m),0,landcoverMinorRoadAQRas_", '30', "m)"), flags=c("overwrite"))
write.table(c('0=0', paste0(minorRoadCode, '=', minorRoadCode), paste0(majorRoadCode, '=', majorRoadCode), paste0(minorRoadCode + majorRoadCode, '=', majorRoadCode)), paste0(tabularDataDir, '/rule.txt'), sep="", col.names=FALSE, quote=FALSE, row.names=FALSE)
execGRASS('r.reclass', input='landcoverRoadAQ1', output='landcoverRoadAQ', rules=paste0(tabularDataDir, '/rule.txt'), flags=c('overwrite'))

# # Combine all roads into a single layer
# # NB: assume the PASL roads are correct and then add the AQ roads to them
execGRASS('r.mapcalc', expression='landcoverRoad = landcoverRoadAQ', flags=c("overwrite"))
execGRASS('r.null', map='landcoverRoad', setnull='0')

# Impose roads on BTSL landcover map
execGRASS('r.patch', input=paste0('landcoverRoad,landcoverReclassed'), output='landcoverFinal', flags=c('overwrite'))

# SC: No buffer to combine with landcover raster
#####################################
# Combine BTSL and Buffer landcover #
#####################################
#execGRASS('r.patch', input='landcover,landcoverBuffer', output='landcover', flags=c('overwrite'))

############################
# Save outputs to geotiffs #
############################
execGRASS('r.out.gdal', input='landcoverFinal',output=paste0(intermediatesDir, '/landcover-with-roads.tif'), format='GTiff', createopt='COMPRESS=LZW', flags=c('overwrite'))
# execGRASS('r.out.gdal', input='rawDataSurficialDeposits',output=paste0(tabularDataDir, '/forest-age-', myResolution, 'm.tif'), format='GTiff', createopt='COMPRESS=LZW', flags=c('overwrite'))
# execGRASS('r.out.gdal', input='rawDataForestDensity', output=paste0(tabularDataDir, '/forest-density-', myResolution, 'm.tif'), format='GTiff', createopt='COMPRESS=LZW',flags=c('overwrite'))
# execGRASS('r.out.gdal', input='rawDataDrainage', output=paste0(tabularDataDir, '/drainage-', myResolution, 'm.tif'), format='GTiff', createopt='COMPRESS=LZW', flags=c('overwrite'))
# execGRASS('r.out.gdal', input='rawDataSurficialDeposits', output=paste0(tabularDataDir, '/deposit-', myResolution, 'm.tif'), format='GTiff', createopt='COMPRESS=LZW', flags=c('overwrite'))
# execGRASS('r.out.gdal', input='rawDataStudyArea', output=paste0(tabularDataDir, '/study-area-', myResolution, 'm.tif'), format='GTiff', createopt='COMPRESS=LZW', flags=c('overwrite'))