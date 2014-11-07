##############################################################################
######## Script to organize input data for Nyasha's species distribution modelling chapter 
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 6 November 2014
######## Data: 
######## 1) species localities from Vera Hoffmann's MSc
######## 2) climate from Adam Wilson's interpolated daily weather data for the CFR using CDO tools
######## 3) biomass data from Adam Wilson's postfire recovery paper
######## 4) soil moisture and fire return interval data from Merow et al. 2014 Protea repens modelling paper
######## 5) soils/geology information from the CFR Bayesian Working Group (dig out ref?)
##############################################################################
######## 
###Steps:
###1) setwd and get libraries
###2) get GIS data
###3) match GIS data to same projection, grid, etc
###4) get locality data and check that they intersect with GIS data
###5) run MaxEnt models
##############################################################################
######## 
###To do:
###1) Discuss and decide on what climate data to include
##############################################################################

##############################################################################
###1) Set working directory and maxent data location and get libraries
##############################################################################
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha"); maxdat="/Applications/maxent/"}
if (Sys.getenv("USERNAME")=='Nyasha') {setwd("C:/Users/Nyasha/Git/Nyasha"); maxdat="C:/somewhere"}
if (Sys.getenv("USERNAME")=='jasper') {setwd("C:/Users/jasper/Git/Nyasha"); maxdat="C:/Extra software/maxent/"}

library(raster);library(gdata); library(calibrate); library (ncdf)

##############################################################################
###2) Get GIS data
##############################################################################
###Get summary climate data - we should discuss these and consider some of the others... we need to work out cdd, gdd etc means
tmax=raster("Data/Climate/MATmax_Q50.nc")
tmin=raster("Data/Climate/MATmin_Q50.nc")
ppt=raster("Data/Climate/MAP_Q50.nc")
#gdd=raster("Data/Climate/GDD_Q50.nc"); NAvalue(x) <- -999 #Growing degree days
#cdd=raster("Data/Climate/CDD_Q50.nc"); NAvalue(x) <- -999 #Cumulative dry days
#cfd=raster("Data/Climate/CFD_Q50.nc"); NAvalue(x) <- -999 #Cumulative frost days

###Get biomass data
seasonality=raster("Data/BiomassAccumulation/A.tif")
time_to_recovery=raster("Data/BiomassAccumulation/ages.tif")
max_NDVI=raster("Data/BiomassAccumulation/gamma.tif")
recovery_rate=raster("Data/BiomassAccumulation/lambda.tif")
fire_return=raster("Data/BiomassAccumulation/FireReturnTime_WeibullScale500m.tif")

###Get soil moisture and fire return interval data...
#soil_moisture=raster("Data/BiomassAccumulation/XXXXX.tif")

###Get geology
geol=stack(list.files("/Users/jasper/GIT/Nyasha/Data/Geology", full.names=T, pattern=".asc")); proj4string(geol)=proj4string(seasonality)

###Resample and reproject all rasters to the same grid and Coordinate Reference System
tmax=projectRaster(tmax,seasonality) #note that function uses bilinear interpolation by default. Can be a little slow...
tmin=projectRaster(tmin,seasonality)
ppt=projectRaster(ppt,seasonality)
fire_return=projectRaster(fire_return,seasonality)
geol=projectRaster(geol,seasonality)
#gdd=projectRaster(gdd,seasonality)
#cdd=projectRaster(cdd,seasonality)
#cfd=projectRaster(cfd,seasonality)
#soil=projectRaster(soil,seasonality) #if needed?

###Pull GIS data into a RasterStack
env=stack(tmax, tmin, ppt, seasonality, time_to_recovery, max_NDVI, recovery_rate, fire_return, geol)
nms=c("tmax", "tmin", "ppt", "seasonality", "time_to_recovery", "max_NDVI", "recovery_rate", "fire_return", names(geol))

###Drop cells with missing data
mask=is.na(tmax*tmin*ppt*seasonality*time_to_recovery*max_NDVI*recovery_rate*fire_return*geol[[1]])
env=mask(env, mask, maskvalue=1, updatevalue=NA)
names(env)=nms
env=stack(env)

###Paste GIS data into MaxEnt directory
writeRaster(env, filename=paste(maxdat,"env_layers/",nms,".asc",sep=""), format="ascii", bylayer=T, overwrite=T)

###Clear objects from memory
#rm(list=c("nms", "env", "tmax", "tmin", "ppt", "seasonality", "time_to_recovery", "max_NDVI", "recovery_rate", "geol", "fire_return"))

##############################################################################
###4) Get georef data, intersect with GIS layers and trim to those that still have 20 or more refs
##############################################################################
###Get data
refs=read.csv("Data/Vera_Hoffman/Georef Data_March2011_Jasper.csv",header=T)

###Make frefs a spatial object in R
coordinates(refs)<-refs[,c(10,9)]
proj4string(refs) <- CRS(proj4string(mask))

###Extract GIS data to georefs and trim out georefs that don't intersect with GIS data
#xrefs=extract(mask,refs) #; rownames(tmaxS)=site$source_pop.
#refs=refs[!xrefs,]
hmm=extract(env,refs)
y=is.na(hmm)
refs=refs[-which(rowSums(y)>0),]

###Trim out species with <15 records
x=aggregate(1:length(refs$Taxon), by=list(refs$Taxon), FUN="length")
colnames(x)=c("Taxon","Count")
#length(which(x$Count>14)) #Checks how many species we're left with
x=x[which(x$Count>14),]
refs=refs[which(refs$Taxon%in%x$Taxon),]

###Clean data by making it a plain dataframe (not spatial anymore) and dropping levels and make it a spatial object again
refs=as.data.frame(refs)
refs=droplevels(refs) #Dropped extra factor classes (levels) from the data - e.g. names of species we're no longer using

###Write out georefs file for records
write.csv(refs, paste("Data/Vera_Hoffman/nyashafocalspecies",Sys.Date(),".csv",sep=""))

###Write out georefs file to MaxEnt folder in appropriate format
loc=refs[,c(10,12,11)]
colnames(loc)<-c("Species","Lon","Lat")
write.csv(loc, paste(maxdat,"loc",Sys.Date(),".csv",sep=""))
