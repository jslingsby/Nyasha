######### Code to cluster vegetation types adapted from Rouget et al. (2015) compiled by nyasha 24/05/2017##########

setwd("C:/Users/nyasha/Desktop/veg_modelling")

library(raster)
library(rgdal)
library(pvclust)
###########################################Create matrix of vegtypes ad environmental vars########################
### all data were trimmed to untransformed fynbos in ArcGIS 10.2
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
writeRaster(env, filename="C:/Users/nyasha/Desktop/veg_modelling/env_dataveg_envstack.TIF", 
            options=c("COMPRESS=LZW", "INTERLEAVE=BAND"), overwrite=TRUE)

##create matrix of vegtypes and corresponding values of environmental variables
matrix = extract( env , 1:ncell(env) )
##check if there are any any NAs in a row
row.has.na <- apply(matrix, 1, function(x){any(is.na(x))})
##see how many rows would have to be dropped
sum(row.has.na)
##drop na rows
matrix.filtered <- matrix[!row.has.na,]

##add column with vegtype names to matrix
vegmatrix = as.data.frame(matrix.filtered)
veg_code = read.csv("veg_types.csv")
vegmatrix_names = merge.data.frame(vegmatrix,veg_code[, c("Value", "NAME")], by.x = "vegmap_cfr_untra", by.y = "Value")

##Remove rows that do not contain the name "fynbos" to ensure we have strictly fynbos subtypes
filteredRows = vegmatrix_names[grep("Fynbos", vegmatrix_names$NAME), ]

##Get proportion of gridcell belonging to each vegtype,filter out anything lower than 10 gridcells?
library(plyr)
count= count(filteredRows, 'NAME')
count$percent<- prop.table(count$freq)*100
write.csv(count, file = "vegtype_freq.csv", sep = " ")

##get mean values of each env var per veg type
library(dplyr)
new.vegmatrix = filteredRows %>% group_by(vegmap_cfr_untra, NAME) %>% summarise_each(funs(mean))
write.csv(new.vegmatrix, file = "veg_matrix.csv") #input for clustering vegetyes

####################################cluster vegtypes based on climate,soil ad fire#########################

#Load data file (matrix of 74 fynbos vegetation types x 7 environmental variables)
vegmatrix <- read.csv("veg_matrix-wfire.csv")
vegmatrix2 <- read.csv("veg_matrix-nofire.csv")
vegmatrix3 <- read.csv("veg_matrix-wfire_nosoil.csv")
vegmatrix4 <- read.csv("veg_matrix-nofire_soil.csv")

## Get mean values of covariates based on vegtype
#library(dplyr)
#newmatrix <- matrix %>% group_by(mapcode12, veg_unit) %>% summarise_each(funs(mean))
#write.csv(newmatrix, paste("VEGMAP2012Beta/vegmatrix_means",Sys.Date(),".csv",sep=""))
matrix_std <- as.data.frame(scale(vegmatrix[,4:10]))# Standardise all values and return as data frame
matrix_std$vegname <-vegmatrix$NAME #returns the vegtype names to the data frame
#reorder columns in standardised matrix
matrix_std2 <- matrix_std[,c(8,1,2,3,4,5,6,7)]


##Hierarchical clustering
d <- dist(matrix_std2, method = "euclidean") # Distance matrix based on Euclidean distance
fitd <- hclust(d, method="ward.D2") #Hierarchical clustering using "ward" method
plot(fitd,labels = matrix_std2$vegname, cex=0.9)# Display dendogram

# cut and assign classes
groups <- cutree(fitd, h=5) # h -cuts tree at specific height, k-cust tree into specific no. of clusters
table(groups) # give size of each cluster
# Draw dendogram with red borders around the 10 clusters
rect.hclust(fitd, h=5, border="red")

#save(groups, file = "fyn_10.txt", ascii = TRUE) # save cluster ID to file
#save.image()

##PVCLUST - bootstrapping method to calculate significance of each cluster
##NB:this algorithm will give a differEnt result everytime you run it due to the random sampling,
fynbos <- setNames(data.frame(t(matrix_std2[,-1])), matrix_std2[,1])#transpose matrix and keep column labels
fit <- pvclust(fynbos, method.hclust="ward.D2",
               method.dist="euclidean", use.cor="pairwise.complete.obs",
               nboot=100, r=seq(.5,1.4,by=.1), store=FALSE, weight=FALSE)
print(fit)
plot(fit, cex.pv = 0.7, cex = 0.7, col.pv=c(2,0,0)) # dendogram with p values
# add rectangles around groups highly supported by the data (>=.95)
pvrect(fit, alpha=.95) 

####get list of clusters and corresponding vetypes
clusters <- pvpick(fit) #returns a list of vegtypes in each cluster
clusters$edges <- NULL #if you want to remove the last element 'edges'
clusters

###figure out to merge this list to the matrix_std2 data frame, for now used excel to convert the list
pv19 <- read.csv("output/pv_cluster19.csv")
pv19_means <- merge(pv19,matrix)
pv19_clusters <- pv19_means %>% group_by(clusters) %>% summarise_each(funs(mean)) #gets mean values of each variable for each cluster
pv19_profile <- pv19_clusters[, colSums(is.na(pv19_clusters)) != nrow(pv19_clusters)] ##drops NA colums
write.csv(pv19_profile, paste("output/pv_cluster19_profile",".csv",sep=""))


#next...how much does each var explain the variance between clusters? 
##calculate coefficient of variance (NB:neg values in soil data can be converted to absolute values)

co.var <- function(x) sd(x)/mean(x)*100

#####check for outliers
#seplot(fit)
#seplot(fit, identify=TRUE)
#print(fit, which=c(69))
