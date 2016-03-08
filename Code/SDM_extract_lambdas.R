##############################################################################
######## Script to read in and summarize lambdas from the MaxEnt models for Nyasha's species distribution modelling chapter 
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 8 March 2016
##############################################################################

##############################################################################
###1) Get function (from John B. Baumgartner - https://github.com/johnbaums/things/blob/master/R/parse_lambdas.R)
##############################################################################

parse_lambdas <- function(lambdas) {
  if(is(lambdas, 'MaxEnt')) {
    lambdas <- lambdas@lambdas
  } else {
    lambdas <- readLines(lambdas)
  }
  con <- textConnection(lambdas)
  n <- count.fields(con, ',', quote='')
  close(con)
  meta <- setNames(lapply(strsplit(lambdas[n==2], ', '), 
                          function(x) as.numeric(x[2])),
                   sapply(strsplit(lambdas[n==2], ', '), '[[', 1))
  lambdas <- setNames(data.frame(do.call(rbind, strsplit(lambdas[n==4], ', ')), 
                                 stringsAsFactors=FALSE),
                      c('feature', 'lambda', 'min', 'max'))
  lambdas[, -1] <- lapply(lambdas[, -1], as.numeric)
  lambdas$feature <- sub('=', '==', lambdas$feature)
  lambdas$feature <- sub('<', '<=', lambdas$feature)
  lambdas$type <- factor(sapply(lambdas$feature, function(x) {
    switch(gsub("\\w|\\.|-|\\(|\\)", "", x),
           "==" = 'categorical',
           "<=" = "threshold",
           "^" = "quadratic",
           "*" = "product", 
           "`" = "reverse_hinge",
           "'" = 'forward_hinge',
           'linear')
  }))
  vars <- gsub("\\^2|\\(.*<=|\\((.*)==.*|`|\\'|\\)", "\\1", lambdas$feature)
  lambdas$var <- sub('\\*', ',', vars)
  c(list(lambdas=lambdas[, c(1, 6, 2:5)]), meta)
}

##############################################################################
###2) Get species names
##############################################################################



##############################################################################
###3) Get lambda files
##############################################################################

#Get list of lambda files from MaxEnt output directory
lambdas <- list.files("/Users/jasper/Documents/GIS/MaxEnt/maxent/nyashatest/", pattern=".lambdas", full.names=TRUE)

#parse_lambdas("/Users/jasper/Documents/GIS/MaxEnt/maxent/nyashatest/crinifolia_0.lambdas")

#Get lambda files for species of interest
index <- grep("crinifolia", lambdas)
focal <- lambdas[index]

#Gets the files in the order in object "lambdas"
lfls <- lapply(focal, "read.csv", header=F)

#lapply(lfls, function(x){return(x[,1:2])})
ldf <- do.call("rbind", lfls)

ave <- aggregate(ldf[,2:ncol(ldf)], by = list(ldf[,1]), mean)
sdev <- aggregate(ldf[,2:ncol(ldf)], by = list(ldf[,1]), sd)



#lapply(me, function(x) x@lambdas)