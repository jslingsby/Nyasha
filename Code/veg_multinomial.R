######### Code to cluster vegetation types adapted from Rouget et al. (2015) compiled by nyasha 24/05/2017##########

#setwd("C:/Users/nyasha/Desktop/veg_modelling")

if (Sys.getenv("USER")=='nyasha') {setwd("C:/Users/nyasha/Desktop/veg_modelling/")}

if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/Dropbox/Shared/Nyasha/Data/")}


library(raster)
library(rgdal)
library(pvclust)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(scales)
library(reshape2)
library(AUC)

###########################################Create matrix of vegtypes ad environmental vars########################
### all data were trimmed to untransformed fynbos in ArcGIS 10.2

# ##Example code for looping
# uniquemodelnames <- c("M1", "M2", "M3")
# 
# output <- as.data.frame(matrix(nrow(sfdat)), ncol(length(uniquemodelnames)))
# 
# for (i in 1:length(uniquemodelnames))
# {
#   fire=raster(paste0("env_data/", uniquemodelnames[1], "fire_clip1.tif"))
#   #...insert more vars, combine and scale - call it "sfdatMod"
#   
#   output[,i] <- predict(mnm1, sfdatMod, type = "class")
# }

##Get env data
fire=raster("env_data/fire_clip1.tif")
map=raster("env_data/map_clip1.tif")
mmp01=raster("env_data/mmp01_clip1.tif")
pptconc=raster("env_data/pptconc_clip1.tif")
soils=raster("env_data/soils_clip1.tif")
tmax01=raster("env_data/tmax01_clip1.tif")
tmin07=raster("env_data/tmin07_clip1.tif")

##Get vegtype raster
vegtype=raster("vegtype_data/gis_data/vegmap_cfr_untra.tif")

##Pull vegtype and env data into raster stack
env=stack(vegtype,fire, map,mmp01, pptconc, soils, tmax01,tmin07)
#writeRaster(env, filename="C:/Users/nyasha/Desktop/veg_modelling/env_dataveg_envstack.TIF", options=c("COMPRESS=LZW", "INTERLEAVE=BAND"), overwrite=TRUE)

##create matrix of vegtypes and corresponding values of environmental variables
dat <- na.omit(as.data.frame(rasterToPoints(env)))

##add column with vegtype names to matrix
veg_code = read.csv("veg_types.csv")
dat = merge.data.frame(dat,veg_code[, c("Value", "NAME")], by.x = "vegmap_cfr_untra", by.y = "Value")

##Remove rows that do not contain the name "fynbos" to ensure we have strictly fynbos subtypes
dat = dat[grep("Fynbos", dat$NAME), ]
dat <- droplevels(dat)

##Standardize covariates
covars <- c("fire_clip1", "map_clip1", "mmp01_clip1", "pptconc_clip1", "soils_clip1", "tmax01_clip1", "tmin07_clip1")   
sdat <- as.data.frame(cbind(dat[, c("NAME", "x", "y", "vegmap_cfr_untra")], scale(dat[,covars])))

##Trim sdat to focal vegtypes
sfdat <- droplevels(sdat[which(sdat$NAME %in% levels(sdat$NAME)[summary(sdat$NAME)>99]),])

#ssfdat <- droplevels(sfdat[sample(1:nrow(sfdat), 500),]) #Take a subsample - ignore

##Fit global models
mnm1 <- multinom(NAME ~ fire_clip1 + map_clip1 + tmin07_clip1 + soils_clip1 + tmax01_clip1 + mmp01_clip1 + pptconc_clip1, data = sfdat, maxit = 500)

summary(mnm1) #Gives estimates of the coefficients and their standard error for each veg type (not that useful...)

##One way of visualizing the fit
preds <- predict(mnm1, sfdat, type = "class")
hmm <- cbind(sfdat$NAME, preds)
heatmap(table(hmm[,1], hmm[,2]), Rowv = NA, Colv = NA, scale = "column")

#table(sfdat$NAME, preds)

##Run every possible combination of the glibal model...
options(na.action = "na.fail")
dmnm1 <- dredge(mnm1)
dmnm1
View(dmnm1)
# dmnm1S <- subset(dmnm1, delta < 1500)
# model.avg(dmnm1S)
# fmod <- subset(dmnm1, 122)
options(na.action = "na.omit")

##Calculate AUC
auc(roc(predict(mnm1, sfdat, type = "class"), sfdat$NAME)) #Global model
auc(roc(predict(multinom(NAME ~ fire_clip1, data = sfdat, maxit = 500), sfdat, type = "class"), sfdat$NAME)) #Fire-only model


###Example code for future predictions
predsM1 <- predict(mnm1, sfdatM1, type = "class") #Where M1 = future model projection 1 and sfdatM1 is a data frame the same as sfdat (i.e. rows are ini the same order to represent the same pixel), but includes the M1 projections for the covariates
predsM2 <- predict(mnm1, sfdatM2, type = "class") #same as above, etc



###Example code for plotting based on https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100

output <- as.data.frame(cbind(sfdat$NAME, #Contrived dummy output (replace with data from loop above)
                              predict(mnm1, sfdat, type = "class"), 
                              predict(multinom(NAME ~ fire_clip1, data = sfdat, maxit = 500), sfdat)))

outdat <- apply(output, MARGIN = 2, FUN = function(x){summary(as.factor(x))}) #Something like this

outdat <- lapply(outdat, function(x){data.frame(names(x), x)})

outdat <- bind_rows(outdat, .id = "model")

names(outdat) <- c("Model", "VegType", "Count")

ggplot(outdat,aes(x = Model, y = Count, fill = VegType)) + 
  geom_bar(position = "fill",stat = "identity") +
  coord_flip()

####################################################################################################################################################################################
