##############################################################################
######## Script to find species for Nyasha's modelling project
##############################################################################
######## Compiled by Jasper Slingsby 2015
######## Last edited: 2 Sept 2015
######## Data: 
##############################################################################
######## Output:
##############################################################################
######## 
###Steps:
###
##############################################################################
###1) Get data
##############################################################################

###Libraries
library(vegan)
library(raster)

###Set working directory
if (Sys.getenv("USER")=='jasper') {local="/Users/jasper/Dropbox/SAEON/Projects/PRECIS/"}

##############################################################################
###2) Get and organize distribution and climate data
##############################################################################

###Get distribution data
load(paste(local,"flaggedData_25Nov2013_w_correctedpoints.Rdata", sep=""))

###Trim to focal species records with accuracy <5km
plant_sa <- plant_sa[which(plant_sa$LOC_CONF<2001),]
plant_sa <- droplevels(plant_sa)

###Get GIS layer and trim records to the study extent (excluding transformed etc)
#Drop gaps
Rext <- raster("Data/BiomassAccumulation/gamma.tif")
x <- plant_sa
coordinates(x) <- cbind(plant_sa$LONGITUDE, plant_sa$LATITUDE)
clim <- extract(Rext, x)
plant_sa <- plant_sa[!is.na(clim),]

#Get cell ID's so duplicates by species can be dropped later
Rext <- setValues(Rext, 1:length(getValues(Rext)))
x <- plant_sa
coordinates(x) <- cbind(plant_sa$LONGITUDE, plant_sa$LATITUDE)
plant_sa$cellID <- extract(Rext, x)

###Look at how many records per spp
x <- aggregate(rep(1,nrow(plant_sa)), by=list(plant_sa$TAXNAME), FUN="sum")
spset <- x[which(x[,2]>14),1]

##############################################################################
###3) Get growth form, trait and species information, merge and trim
##############################################################################

Spp <- read.csv("Data/POSA/POSAoffline_Species.txt")
Spp$TAXNAME = paste(Spp$Genus, Spp$Species, sep = " ")
SpSet <- Spp[which(Spp$TAXNAME %in% spset),]

###Match species traits and merge
tr <- read.csv("/Users/jasper/GIT/Dimensions-ZA/data_base/speciesXtraits.csv", stringsAsFactors = F)
trt <- tr[which(tr$genus_species_GM %in% SpSet$TAXNAME), ]
colnames(trt)[which(colnames(trt)=="genus_species_GM")] = "TAXNAME"
TrtSpSet <- merge(trt, SpSet)

###Drop species by life-history, growth form, and lineage
TrtSpSet <- TrtSpSet[which(TrtSpSet$perennial==1), ]

###Drop unwanted columns
TrtSpSet <- TrtSpSet[, which(colnames(TrtSpSet) %in% c("TAXNAME", "family_GM", "SpeciesFull", "Genus", "Species", "Subspecies", "monoecious", "dioecious", "resprout_postfire",  "serotinous", "plant_longevity", "flammability", "pollination", "dispersal", "graminoid", "low_shrub", "mid_shrub", "tall_shrub", "tree"))]

###Extract only growth forms that are of interest
TrtSpSet <- TrtSpSet[which(rowSums(TrtSpSet[,c("graminoid", "low_shrub", "mid_shrub", "tall_shrub", "tree")])>0), ]

###Extract locality records for species of interest
LocSet <- unique(plant_sa[which(plant_sa$TAXNAME %in% TrtSpSet$TAXNAME), ])
LocSet <- LocSet[!duplicated(cbind(LocSet$TAXNAME, LocSet$cellID)),]

###Look at how many records per spp once we drop duplicates per cell
x <- aggregate(rep(1,nrow(LocSet)), by=list(LocSet$TAXNAME), FUN="sum")
x <- x[which(x[,2]<11),1]

LocSet <- LocSet[-which(LocSet$TAXNAME %in% x), ]
TrtSpSet <- TrtSpSet[-which(TrtSpSet$TAXNAME %in% x), ]

##############################################################################
###4) Save out results
##############################################################################

write.csv(LocSet, "Data/PRECIS/locality_data_PRECIS_TRIMMED.csv", row.names=F)
write.csv(TrtSpSet, "Data/PRECIS/species_attributes_PRECIS_TRIMMED.csv", row.names=F)
