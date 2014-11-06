##############################################################################
######## Script to organize input data for Nyasha's species distribution modelling chapter 
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 6 November 2014
######## Data: 
######## 1) species localities from Vera Hoffmann's MSc
######## 2) climate from Adam Wilson's interpolated daily weather data for the CFR using CDO tools
######## 3) fire data from Adam Wilson's fire data
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
###1) Set working directory and get libraries
##############################################################################
if (Sys.getenv("USER")=='jasper') setwd("/Users/jasper/GIT/Nyasha")
if (Sys.getenv("USERNAME")=='Nyasha') setwd("C:/Users/Nyasha/Git/Nyasha")
if (Sys.getenv("USERNAME")=='jasper') setwd("C:/Users/jasper/Git/Nyasha")

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

###Get fire data
seasonality=raster("Data/BiomassAccumulation/A.tif")
time_to_recovery=raster("Data/BiomassAccumulation/ages.tif")
max_NDVI=raster("Data/BiomassAccumulation/gamma.tif")
recovery_rate=raster("Data/BiomassAccumulation/lambda.tif")

###Add soil moisture and fire return interval data...
#soil_moisture=raster("Data/BiomassAccumulation/XXXXX.tif")
#fire_return=raster("Data/BiomassAccumulation/XXXXX.tif")

###Resample and reproject all rasters to the same grid and Coordinate Reference System
tmax=projectRaster(tmax,seasonality) #note that function uses bilinear interpolation by default. Can be a little slow...
tmin=projectRaster(tmin,seasonality)
ppt=projectRaster(ppt,seasonality)
#gdd=projectRaster(gdd,seasonality)
#cdd=projectRaster(cdd,seasonality)
#cfd=projectRaster(cfd,seasonality)
#soil=projectRaster(soil,seasonality) #if needed?
#fire_return=projectRaster(fire_return,seasonality) #if needed?

###Drop cells with no biomass accumulation data
x=is.na(seasonality)
tmax=tmax*!x
tmin=tmin*!x
ppt=ppt*!x
#gdd=gdd*!x
#cdd=cdd*!x
#cfd=cfd*!x
#soil=soil*!x
#fire_return=fire_return*!x

##############################################################################
###4) Get site data
##############################################################################

###Summarize ref data and draw out species with 20 or more records
#Get data and summarize info
refs=read.csv("Data/Vera_Hoffman/Georef Data_March2011_Jasper.csv",header=T)
x=aggregate(1:length(refs$Taxon), by=list(refs$Taxon), FUN="length")
colnames(x)=c("Taxon","Count")
y=unique(merge(x, refs[,c("Clade", "Taxon")]))

#Draw out species with 20 or more georefs
hmm=y$Taxon[which(y$Count>19)] 
frefs=refs[which(refs$Taxon%in%hmm),]

###Intersect 
Latitude.Decimal  Longitude.Decimal
#####


coordinates(frefs)<-frefs[,9:10]
proj4string(frefs) <- CRS(proj4string(seasonality)) #+proj=utm +zone=34 +ellps=WGS84 +south")#

###Extract data to sites and bind site and climate data
tmaxS=extract(tmax,site); rownames(tmaxS)=site$source_pop.
tminS=extract(tmin,site); rownames(tminS)=site$source_pop.
pptS=extract(ppt,site); rownames(pptS)=site$source_pop.
