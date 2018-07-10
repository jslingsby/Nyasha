##############################################################################
######## Code to rerun PCoA on BWG soils layers for the CFR
######## 
##############################################################################
######## Compiled by Jasper SLingsby 2018
######## Last edited: 9 July 2018
######## Data: 
######## BWG soils layers for the CFR
##############################################################################

library(raster)
library(rgdal)
library(ape)
library(vegan)
library(reshape2)
library(ggplot2)

# soil <- readOGR(dsn = "/Users/jasper/Documents/GIS/BWGCFR/all_shapefiles_9jan/geo_9jan.shp", layer = "geo_9jan")
# x <- soil@data[,2:14]
# coordinates(x) <- ~ XNEW + YNEW
# srast <- stack(x)

###Get and format data
soil <- stack(list.files("/Users/jasper/Documents/GIS/BWGCFR/BWG_soils_Jasper", full.names = T))

dat <- rasterToPoints(soil)

write.csv(dat, "Data/Geology/soilcluster.csv", row.names = F)

##############################################################################
###On cluster!!!
##############################################################################

library(ape)
library(vegan)

dat <- read.csv("Data/Geology/soilcluster.csv")
#rownames(dat) <- 1:nrow(dat)

###Do PCoA
soilD <- vegdist(dat[,3:ncol(dat)], "bray")
res <- pcoa(soilD)

###Explor output
res$values
biplot(res)

##############################################################################
###Explore existing layer
##############################################################################

###Get and format data
soil <- stack(list.files("/Users/jasper/GIT/Nyasha/Data/Geology", pattern = ".asc", full.names = T))

dat <- rasterToPoints(soil)

mdat <- melt(dat[,3:ncol(dat)])

jpeg("Output/SoilPCoA.jpg", width = 5, height = 5, units = "in", res = 300, pointsize = 6)
par(mfrow=c(3,4))
boxplot(CFRgeocombined ~ fert1, data = dat, main = "Fert1(low)", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ fert2, data = dat, main = "Fert2", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ fert3, data = dat, main = "Fert3", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ fert4, data = dat, main = "Fert4(high)", ylab = "Soil PC Axis 1", xlab = "Presence of class")

#par(mfrow=c(2,2))
boxplot(CFRgeocombined ~ tex1, data = dat, main = "Texture1(fine)", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ tex2, data = dat, main = "Texture2", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ tex3, data = dat, main = "Texture3", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ tex4, data = dat, main = "Texture4(coarse)", ylab = "Soil PC Axis 1", xlab = "Presence of class")

#par(mfrow=c(2,2))
boxplot(CFRgeocombined ~ ph1, data = dat, main = "pH1(acidic)", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ ph2, data = dat, main = "pH2", ylab = "Soil PC Axis 1", xlab = "Presence of class")
boxplot(CFRgeocombined ~ ph3, data = dat, main = "pH3(alkaline)", ylab = "Soil PC Axis 1", xlab = "Presence of class")
dev.off()
