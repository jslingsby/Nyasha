

#Get list of lambda files
lambdas <- list.files("/Users/jasper/Documents/GIS/MaxEnt/maxent/nyashatest/", pattern=".lambdas", full.names=TRUE)

#Get lambda files for species of interest
index <- grep("crinifolia", lambdas)
focal <- lambdas[index]

#Gets the files in the order in object "lambdas"
lfls <- lapply(focal, "read.csv", header=F)

#lapply(lfls, function(x){return(x[,1:2])})
ldf <- do.call("rbind", lfls)

ave <- aggregate(ldf[,2:ncol(ldf)], by = list(ldf[,1]), mean)
sdev <- aggregate(ldf[,2:ncol(ldf)], by = list(ldf[,1]), sd)
