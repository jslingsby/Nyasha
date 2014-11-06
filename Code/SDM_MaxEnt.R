##############################################################################
######## Script to run MaxEnt models for Nyasha's species distribution modelling chapter 
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
###2) get georef data
###3) loop species through MaxEnt
##############################################################################
######## 
###To do:
###1) 
##############################################################################

##############################################################################
###1) Set working directory and maxent data location and get libraries
##############################################################################
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha"); maxdat="/Applications/maxent/"; maxent="/Applications/maxent/maxent.jar"}
if (Sys.getenv("USERNAME")=='jasper') {setwd("C:/Users/jasper/Git/Nyasha"); maxdat="C:/Extra software/maxent/"; maxent="C:/somewhere"}
if (Sys.getenv("USERNAME")=='Nyasha') {setwd("C:/Users/Nyasha/Git/Nyasha"); maxdat= "C:/somewhere" ; maxent="C:/somewhere"}
  
#library(raster);library(gdata); library(calibrate); library (ncdf)

##############################################################################
###2) Get georef data
##############################################################################
refs=read.csv("Data/Vera_Hoffman/nyashafocalspecies2014-11-06.csv", header=T, row.names=1, stringsAsFactors=F)

refs=refs[,c(10,12,11)]
colnames(refs)<-c("Species","Lon","Lat")

#write.csv(locality, "D:\\GIS\\MaxEnt\\maxent\\protea.csv", row.names=FALSE)

#library(phyloclim)
#fix(niche.equivalency.test)

#setwd("D:\\GIS\\MaxEnt\\maxent\\")

######################################################################
###3) Run MaxEnt
######################################################################
setwd(maxdat)

pnames<-unique(refs[,1])

for(i in 1:length(pnames))
{
  local<-refs[which(refs$Species==pnames[i]),]
  
  ### Write out Locality file
  write.csv(local, "loc.csv", row.names=FALSE)
  
  ### Set batch file details - see setMEbatch in climateTools.R
  call <-"java -mx5512m -jar maxent.jar environmentallayers=env_layers samplesfile=loc.csv outputdirectory=maxentoutput redoifexists autorun"
  system(call, wait=TRUE)
  # system("D:/GIS/MaxEnt/maxent/batchProtea.bat", wait=TRUE)
}