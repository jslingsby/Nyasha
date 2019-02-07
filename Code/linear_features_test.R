
library(raster)
library(sp)

#loc <- read.csv("/Users/jasper/GIT/Nyasha/Data/Vera_Hoffman/nyashafocalspecies2015-06-28.csv", header = T, stringsAsFactors = F)

loc <- read.csv("/Users/jasper/GIT/Nyasha/Data/LocalityData/PRECIS_locality_data_dec2015.csv", header = T, stringsAsFactors = F)

coordinates(loc) <- ~ LONGITUDE + LATITUDE

lyrs <- list.files("/Users/jasper/GIT/Nyasha/Data/MaxEnt_Env layers_1min", full.names = T, pattern ="asc")

lyrs <- stack(lyrs[-c(grep("fert", lyrs), grep("ph", lyrs), grep("tex", lyrs), grep("mmp01", lyrs), grep("NDVI", lyrs), grep("recovery", lyrs))])

dat <- as.data.frame(extract(lyrs, loc))

#Add mmp01
mmp <- raster("/Users/jasper/GIT/Nyasha/Data/MaxEnt_Env layers_1min/mmp01_1min.asc")

dat$mmp01 <- extract(mmp, loc)

###Make output dataframe
linear <- matrix(NA, length(levels(as.factor(loc$TAXON))), ncol(dat))
colnames(linear) <- colnames(dat)
quadratic <- matrix(NA, length(levels(as.factor(loc$TAXON))), ncol(dat))
colnames(quadratic) <- colnames(dat)
both <- matrix(NA, length(levels(as.factor(loc$TAXON))), ncol(dat))
colnames(both) <- colnames(dat)


#Loop through species and models
for (i in 1:length(levels(as.factor(loc$TAXON))))
{

  ###Get data for focal species
  sp <- levels(as.factor(loc$TAXON))[i]
  sdat <- as.data.frame(dat[which(loc$TAXON == sp),])
  
  ###Get background samples
  samp <- dat[-which(loc$TAXON == sp),]
  samp <- as.data.frame(samp[sample(1:nrow(samp), 2*nrow(sdat)),])
  sdat$presence <- 1
  samp$presence <- 0
  sdat <- rbind(sdat,samp)
  
  ###Do logistic GLM with linear vs quadratic predictors for each var
  for(j in 1:(ncol(sdat)-1))
  {
    linear[i,j] <- AIC(glm(sdat$presence ~ sdat[,j], family=binomial))
    quadratic[i,j] <- AIC(glm(sdat$presence ~ I(sdat[,j]^2), family=binomial))
    both[i,j] <- AIC(glm(sdat$presence ~ sdat[,j] + I(sdat[,j]^2), family=binomial))
  }
}


linear <- linear[-which(levels(as.factor(loc$TAXON)) %in% c("PodocElong", "PodocLatif", "PteroCamph", "PteroTeret", "WiddrCedar", "WiddrNodif")),]
quadratic <- quadratic[-which(levels(as.factor(loc$TAXON)) %in% c("PodocElong", "PodocLatif", "PteroCamph", "PteroTeret", "WiddrCedar", "WiddrNodif")),]
both <- both[-which(levels(as.factor(loc$TAXON)) %in% c("PodocElong", "PodocLatif", "PteroCamph", "PteroTeret", "WiddrCedar", "WiddrNodif")),]

result <- linear - quadratic
as.matrix(sapply(as.data.frame(result), function(x){sum(x<3)}))

resultlb <- linear - both
as.matrix(sapply(as.data.frame(resultlb), function(x){sum(x<3)}))

# result <- both - linear
# sapply(as.data.frame(result), function(x){sum(x<3)})
# 
# result <- quadratic - linear
# sapply(as.data.frame(result), function(x){sum(x<3)})
