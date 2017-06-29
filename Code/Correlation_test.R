########Correlation test for environmental variables########
#######Compiled by Nyasha, 28 June 2017#######

setwd("C:/Users/nyasha/Desktop/veg_modelling")
library(raster)

##Get env data
fire=raster("env_data/fire_clip1.tif")
map=raster("env_data/map_clip1.tif")
mmp01=raster("env_data/mmp01_clip1.tif")
pptconc=raster("env_data/pptconc_clip1.tif")
soils=raster("env_data/soils_clip1.tif")
tmax01=raster("env_data/tmax01_clip1.tif")
tmin07=raster("env_data/tmin07_clip1.tif")

dat=stack(stack(fire, map,mmp01, pptconc, soils, tmax01,tmin07))
edat = extract( dat , 1:ncell(dat) )
edat=na.omit(edat)

pearson_cor=cor(edat,method = c("pearson"))
spearman_cor=cor(edat,method = c("spearman")) 

cortest=as.data.frame(pearson_cor)
write.csv(cortest, file = "Pearson_cortest.csv")
