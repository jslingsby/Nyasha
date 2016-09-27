##############################################################################
######## Script to read in and summarize lambdas from the MaxEnt models for Nyasha's species distribution modelling chapter 
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 8 March 2016
##############################################################################
# See http://gsp.humboldt.edu/OLM/GSP_570/Learning%20Modules/10%20BlueSpray_Maxent_Uncertinaty/MaxEnt%20lambda%20files.pdf for guidelines etc

#if (Sys.getenv("USER")=='jasper') {lambdas_location <- "/Users/jasper/Dropbox/Shared/Nyasha/Maxent_run7_dec2015/Test_lambdas"}
if (Sys.getenv("USER")=='jasper') {lambdas_location <- "/Users/jasper/Dropbox/Shared/Nyasha/Data/Lambdas_12Sept"}

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

#spnames <- unique(read.csv("Data/LocalityData/PRECIS_locality_data_dec2015.csv", stringsAsFactors = F)$TAXON) #SEE NEW WAY OF EXTRACTING NAMES BELOW

##############################################################################
###3) Get lambda files and create a loop to parse lambdas by species
##############################################################################

#Get list of lambda files from MaxEnt output directory
lambdas <- list.files(lambdas_location, pattern=".lambdas", full.names=TRUE)

#Get sepcies names from lambda files
slambdas <- list.files(lambdas_location, pattern=".lambdas", full.names=FALSE)
spnames <- unique(sapply(slambdas, function(x){strsplit(x, split = "_")[[1]][1]}))

#LOOP over species

mean_lambda <- list()
sdev_lambda <- list()
n_models <- list()

for(i in 1:length(spnames)) {

#Get lambda files for species of interest
index <- grep(spnames[i], lambdas)
focal <- lambdas[index]

#Make a list of all lambdas for species i
lfls <- lapply(focal, "parse_lambdas")

#Extract just the lambdas tables into a list of tables
lfls <- lapply(lfls, function(x){x$lambdas})

#Collapse into one table and remove covariates with zero lambdas
lfls <- do.call("rbind", lfls)
lfls <- lfls[which(lfls$lambda!=0),]

#Calculate mean and range for each lambda for each covariate for each species
ave <- aggregate(lfls[,3], by = list(lfls[,1]), mean, na.rm=T)
sdev <- aggregate(lfls[,3], by = list(lfls[,1]), sd)
n <- aggregate(lfls[,3], by = list(lfls[,1]), function(x){sum(x!=0)})

#Organize outputs
colnames(ave)[2] <- spnames[i]; colnames(sdev)[2] <- spnames[i]; colnames(n)[2] <- spnames[i]

#Plce outputs in lists
mean_lambda[[i]] <- ave
sdev_lambda[[i]] <- sdev
n_models[[i]] <- n

} #END LOOP

###Reorganize means into a table [You'd need to repeat this code replacing mean_lambda with sdev_lambda etc to get sdev or n...]
names(mean_lambda) <- spnames
vs <- unique(unlist(lapply(mean_lambda, function(x){x[,1]})))
vs <- as.data.frame(vs)
colnames(vs)="Group.1"
for(i in 1:length(mean_lambda)) {vs <- merge(vs, mean_lambda[[i]], all=T)}
rownames(vs) <- vs[,1]
vs <-as.data.frame(t(vs[,2:ncol(vs)]))
#vs$Species <- rownames(vs)

##############################################################################
###4) Bind on species attributes and create boxplots...
##############################################################################

#e.g.
#boxplot(vs$recovery_rate_1min_clip ~ rep(c("A","B"), 52))
