##########################################
######## R code for setting up climate data for Nyasha's PhD
######## DON'T RUN!!! - Can only be run from Jasper's "SAEON MacBook Pro...
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 13 April 2016
##########################################

##########################################
###1) Get libraries, setwd and get data
##########################################
library(raster)
library(gdalUtils)
library(rgdal)

setwd("/Users/jasper/GIT/Nyasha")

##########################################
###2) Get example raster
##########################################

age=raster("Data/BiomassAccumulation/ages.tif")
prj <- proj4string(age)

rast <- raster("/Users/jasper/GIT/Nyasha/Data/MaxEnt_Env layers_1min/CFRgeocombined_1min.asc")
proj4string(rast) <- prj
writeRaster(rast, "Data/Adam/standardizedgrid.tif")

##########################################
###2) Get "current" climate data
##########################################
# from Schulze et al 2007 - vars include map, mmp01, pptconc, tmax01, tmin07

if(!file.exists("Data/Adam/currentclimate.grd")) {
  
  grids <- c("/Users/jasper/Documents/GIS/Schultze (2007)/grids/gmap", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/gmednrfl1", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/tmaxave01c", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/tminave07c") #The paths to the files we need
  gridsn <- sapply(grids, FUN = function(x){strsplit(x, "/")[[1]][8]}) #Extract the file names
  
  #Align all rasters to the same extent, grid, projection...
#  if(!file.exists("Data/Adam/gmap.tif")) {
    for (i in 1:length(grids))
    {
      align_rasters(grids[i], "Data/Adam/standardizedgrid.tif", paste("Data/Adam/", gridsn[i], ".tif", sep=""), output_Raster = TRUE, nThreads = 2, verbose = FALSE)
    }
  clim <- stack(paste("Data/Adam/", gridsn, ".tif", sep=""))
#  } else {clim <- stack(paste("Data/Adam/", gridsn, ".tif", sep=""))}
  
  #Get and rasterize pptcon to the same grid
#  if(!file.exists("Data/Adam/pptcon.grd")) {
    pptcon <- readOGR(dsn = "/Users/jasper/Documents/GIS/Schultze (2007)/shape_files/rfl_seasconc.shp", layer = "rfl_seasconc")
    rasterize(pptcon, clim, field = "RFL_CONC", filename = "Data/Adam/pptcon.grd") #} else {
    pptcon <- raster("Data/Adam/pptcon.grd")
#    }
  
  #Bind the aligned rasters together, rename and write to file
  clim <- stack(clim, pptcon)
  names(clim) <- c("map", "mmp01", "tmax01", "tmin07", "pptconc") #Use names that match the futures names
  writeRaster(clim, "Data/Adam/currentclimate.grd")
} else {
  clim <- brick("Data/Adam/currentclimate.grd") #Otherwise, if the file does exist, just read it in as a RasterBrick
}
#rm(clim, grids, gridsn, pptcon)

##########################################
###2) Get "future" climate data
##########################################

# (including projections from multiple models) from Wilson et al. 2015
# First get future anomalies

if(!file.exists("Data/Adam/futureclimate.grd")) {fclim <- stack(list.files("/Users/jasper/Documents/Papers/PhD/Fynbos literature/Wilson 2015 PNAS tifs", full.names=T, pattern = ".tif"))
fclim <- projectRaster(fclim, clim)
fclim_SD <- subset(fclim, seq(2,440,2))
fclim <- subset(fclim, seq(1,439,2))
writeRaster(fclim, "Data/Adam/futureclimate.grd")
writeRaster(fclim_SD, "Data/Adam/futureclimate_SD.grd")} else {
  fclim <- brick("Data/Adam/futureclimate.grd")
}
  
#Get and format metadata about the futures
futureInfo <- as.data.frame(matrix(unlist(strsplit(names(fclim), "_")), nlayers(fclim), 4, byrow=T, dimnames=list(1:nlayers(fclim), c("Scenario", "Model", "Period", "Variable"))), stringsAsFactors = F)
futureInfo$Variable <- sapply(futureInfo$Variable, function(x) {strsplit(x, ".", fixed=T)[[1]][1]})

#Add futures to current
fclimplus <- list()
for (i in 1:nlayers(clim))
{
  fclimplus[[i]] <- clim[[i]] + fclim[[which(futureInfo$Variable==names(clim)[i])]]
  names(fclimplus[[i]]) <- apply(futureInfo[which(futureInfo$Variable==names(clim)[i]),1:3], MARGIN=1, FUN="paste", collapse="_")
}

names(fclimplus) <- names(clim)

#writeRaster(fclimplus, "Data/Adam/futurepluscurrentclimate.grd")

#for (i in 1:nlayers(clim))
#{
#  fclimplus[[which(futureInfo$Variable==names(clim)[i])]] <- clim[[i]] + fclim[[which#(futureInfo$Variable==names(clim)[i])]]
#}
#Write out future climate + current and "futureclimate" metadata
write.csv(futureInfo, "Data/Adam/futureclimate_Info.csv")

for(i in 1:length(fclimplus))
{
writeRaster(fclimplus[[i]], paste("Data/Adam/", names(fclimplus)[i], "/futureclimate_plus_Current.grd", sep=""))
writeRaster(fclimplus[[i]], paste("Data/Adam/", names(fclimplus)[i], "/", names(fclimplus[[i]]), ".asc", sep=""), bylayer=T)
}

writeRaster(clim, paste("Data/Adam/", names(clim), ".asc", sep=""), bylayer=T)


# Get veg age/return interval data (including projections from multiple models)
if(!file.exists("Data/Adam/futureages.grd")) {fage <- stack(list.files("/Users/jasper/Documents/Papers/PhD/Fynbos literature/Wilson 2015 PNAS tifs/ages", full.names=T, pattern = ".tif"))
fage <- projectRaster(fage, age)
writeRaster(fage, "Data/Adam/futureages.grd")} else {fage <- brick("Data/Adam/futureages.grd")}

fage_plus <- age + fage

names(fage_plus) <- names(fage)

writeRaster(fage_plus, "Data/Adam/age/futureage_plus_Current.grd")
writeRaster(fage_plus, paste("Data/Adam/age/", names(fage_plus), ".asc", sep=""), bylayer=T)

writeRaster(age, "Data/Adam/age/age_Current.grd")
writeRaster(age, "Data/Adam/age/age.asc", bylayer=T)


#Add futures to current ages
#for (i in 1:nlayers(fage))
#{
#  fclim[[which(futureInfo$Variable==names(clim)[i])]] <- clim[[i]] + fclim[[which(futureInfo$Variable==names(clim)[i])]]
#}

