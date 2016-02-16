##############################################################################
######## Script to extract fire data to veg types 
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 16 November 2016
######## Data: 

library(raster)
library (rgdal)

###################
###Get Adam Wilson's fire data
###################
season<-raster("/Users/jasper/Documents/PostDoc/Connecticut/Adam/BiomassAccumulation/A.tif")
age<-raster("/Users/jasper/Documents/PostDoc/Connecticut/Adam/BiomassAccumulation/ages.tif")
maxNDVI<-raster("/Users/jasper/Documents/PostDoc/Connecticut/Adam/BiomassAccumulation/gamma.tif")
rate<-raster("/Users/jasper/Documents/PostDoc/Connecticut/Adam/BiomassAccumulation/lambda.tif")
#FRI<-


#Stack fire data
fire<-stack(maxNDVI, season, rate,age)
names(fire)<-c("Maximum NDVI", "Seasonality", "Recovery rate", "Time to recovery")

###################
##Get vegmap layers
###################
xveg<-readOGR(dsn=paste("/Users/jasper/Documents/Data/Anabelle/Acocks/VegMap",sep=""),layer="vegm2006")    #all vegmap

#Trim to Fynbos
xveg<-xveg[xveg$BIOME=="Fynbos Biome",]

###################
##Check projections
###################
proj4string(maxNDVI)
proj4string(xveg)

###################
###Extract fire data to vegmap and reshape to dataframes by variable
###################
#vdat<-extract(fire, xveg, layer=1, fun=NULL, df=TRUE, na.rm=T)
vdat<-extract(fire, xveg)

#cleaning
names(vdat)<-xveg$POLYGONID #Label rows by polygon
xdat<-vdat[!unlist(lapply(vdat, FUN="is.null"))] #Exlude null values
hmm<-melt(xdat) #"melt" list into dataframe

#Extract dataframes by variable
NDVI<-na.omit(hmm[which(hmm$X2=="Maximum.NDVI"),])
Seasonality<-na.omit(hmm[which(hmm$X2=="Seasonality"),])
Recovery.rate<-na.omit(hmm[which(hmm$X2=="Recovery.rate"),])
Time.to.recovery<-na.omit(hmm[which(hmm$X2=="Time.to.recovery"),])

#Get factor labels
vegform<-read.csv("/Users/jasper/Dropbox/SAEON/Conferences/Biodiversity SA 2013/Analyses/vegformation.csv",stringsAsFactors=F)

#Set factors
Veg_group<-droplevels(sapply(NDVI$L1, FUN=function(x,y,z){return(z[which(y==x)])}, y=xveg@data$POLYGONID, z=xveg@data$GROUP))
Veg_type<-droplevels(sapply(NDVI$L1, FUN=function(x,y,z){return(z[which(y==x)])}, y=xveg@data$POLYGONID, z=xveg@data$NAME))
Veg_formation<-Veg_group
Soil_type<-Veg_group
levels(Veg_formation)<-vegform[,2]
levels(Soil_type)<-vegform[,3]
levels(Veg_group)<-vegform[,4]

#plot
par(mfrow=c(4,3))
boxplot(NDVI[,3]~droplevels(Veg_formation), cex.axis=0.75, main="Vegetation formation", las=3, ylab="NDVI")
boxplot(NDVI[,3]~Soil_type, cex.axis=0.75, main="Soil type", las=3, ylab="NDVI")
boxplot(NDVI[,3]~Veg_group, cex.axis=0.75, main="Vegetation group", las=3, ylab="NDVI")
boxplot(Time.to.recovery[,3]~droplevels(Veg_formation), cex.axis=0.75, main="Vegetation formation", las=3, ylab="Time to recovery")
boxplot(Time.to.recovery[,3]~Soil_type, cex.axis=0.75, main="Soil type", las=3, ylab="Time to recovery")
boxplot(Time.to.recovery[,3]~Veg_group, cex.axis=0.75, main="Vegetation group", las=3, ylab="Time to recovery")
boxplot(Seasonality[,3]~droplevels(Veg_formation), cex.axis=0.75, main="Vegetation formation", las=3, ylab="Seasonality")
boxplot(Seasonality[,3]~Soil_type, cex.axis=0.75, main="Soil type", las=3, ylab="Seasonality")
boxplot(Seasonality[,3]~Veg_group, cex.axis=0.75, main="Vegetation group", las=3, ylab="Seasonality")
boxplot(Recovery.rate[,3]~droplevels(Veg_formation), cex.axis=0.75, main="Vegetation formation", las=3, ylab="Recovery rate")
boxplot(Recovery.rate[,3]~Soil_type, cex.axis=0.75, main="Soil type", las=3, ylab="Recovery rate")
boxplot(Recovery.rate[,3]~Veg_group, cex.axis=0.75, main="Vegetation group", las=3, ylab="Recovery rate")

