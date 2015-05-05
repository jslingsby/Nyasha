library(raster)

fls=list.files("C:\\Extra software\\maxent\\maxentoutput", pattern="_avg.asc", full.names=T)

spp=stack(fls)

spp_x=sum(spp)

plot(spp_x)


veg=readOGR(dsn="C:\\GIT\\Nyasha\\Data\\VegMap", layer="vegm2006_bioregions")


fyn=veg[which(veg$BIOME=="Fynbos Biome"),2]
fyn$BIOREGION=droplevels(fyn$BIOREGION)

plot(fyn)
plot(spp_x, add=T)

dat=extract(spp[[1]], fyn, df=T, fun=sum, na.rm=T)

hist(spp_x)
#getValues(spp_x)