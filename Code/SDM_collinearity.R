##############################################################################
######## Script to test for collinearity in Nyasha's data for species distribution modelling chapter
##############################################################################
######## Compiled by Jasper Slingsby 2015
######## Last edited: 5 May 2015
######## Data: 
######## 1) raster layers
##############################################################################

##############################################################################
###1) Set working directory and maxent data location and get libraries
##############################################################################
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha"); maxdat="/Applications/maxent/"}
if (Sys.getenv("USERNAME")=='Nyasha') {setwd("C:/Users/Nyasha/Git/Nyasha"); maxdat="C:/somewhere"}
if (Sys.getenv("USERNAME")=='jasper') {setwd("C:/GIT/Nyasha"); maxdat="C:/Extra software/maxent/"}

library(raster);library(gdata); library(calibrate); library (ncdf) #; library(picante)
#You may need to install separate software called netCDF (http://www.unidata.ucar.edu/software/netcdf/docs/winbin.html)

##############################################################################
###2) Get GIS data
##############################################################################

###Get Rasters
files=list.files(paste(maxdat,"env_layers/",sep=""), pattern=".asc", full=T)
dat=stack(x=files)
proj4string(dat)="+proj=longlat +ellps=WGS84 +no_defs"

###Get spp localities
loc=read.csv(paste("Data/Vera_Hoffman/Georef Species Data_31 March.csv",sep=""), stringsAsFactors=F)
#loc=loc[-which(is.na(loc$Latitude.Decimal)), 1:17]
loc=SpatialPointsDataFrame(coords=data.frame(long=loc$Longitude.Decimal, lat=loc$Latitude.Decimal), loc, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
           
##############################################################################
###3) Extract GIS data
##############################################################################

ldat=extract(dat, loc)
ldat=cbind(loc@data, ldat)
ldat=na.omit(ldat)

write.csv(ldat, "Data/LocalityData/clean_localities_5May2015.csv", row.names=F)
###Identify and fix 4 records manually 8002085 and 8012844
#length(unique(ldat$Scientific_Name))
#test=unique(ldat[,c(7,13:17)])
#dups=test$Scientific_Name[which(duplicated(test$Scientific_Name))]
#test=ldat[which(ldat$Scientific_Name%in%dups),]

##############################################################################
###4) Play with collinearity
##############################################################################

dat=ldat[,18:36]
cors=cor(dat) #cor.table in library(picante) gives you p-values too

pairs(dat[,c(5,6,10,11,12,17:19)])

pairs(dat[,c(5,6,10,12,18,19)]) #drop recovery_rate (11) and time_to_recovery (11) (retain fire_return)

cor(dat[,c(5,6,10,12,18,19)])

cors=cor(dat[,-c(11,17)])

sum(cors>.7)
