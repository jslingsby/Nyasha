##############################################################################
###5) MaxEnt modelling using biomod2 - NOT WORKING!!!
##############################################################################
###
library(biomod2)
###Set data
spnames=unique(refs$Taxon) #get vector of unoque species names

dat=refs[which(refs$Taxon==spnames[1]),] #pull out data for one species
resp=rep(1, dim(dat)[1]) #make a vector of 1s as long as the number of localities for this species
xy=dat[,1:2] #SpatialPoints(coords=dat[,1:2], proj4string=CRS(proj4string(seasonality)))


data=BIOMOD_FormatingData(resp.var=resp, 
                          expl.var=env, 
                          resp.xy=xy, 
                          resp.name=as.character(spnames[1]),
                          PA.nb.rep=1,
                          PA.nb.absences=1000,
                          PA.strategy='random')

if (Sys.getenv("USER")=='jasper') 
{ 
  myBiomodOptions <- BIOMOD_ModelingOptions(MAXENT = list( path_to_maxent.jar = '/Applications/maxent/maxent.jar',
                                                           memory_allocated = 3984,
                                                           maximumiterations = 200,
                                                           visible = FALSE,
                                                           linear = TRUE,
                                                           quadratic = TRUE,
                                                           product = TRUE,
                                                           threshold = TRUE,
                                                           hinge = TRUE,
                                                           lq2lqptthreshold = 80,
                                                           l2lqthreshold = 10,
                                                           hingethreshold = 15,
                                                           beta_threshold = -1,
                                                           beta_categorical = -1,
                                                           beta_lqp = -1,
                                                           beta_hinge = -1,
                                                           defaultprevalence = 0.5))
}

#if (Sys.getenv("USERNAME")=='Nyasha') 
#if (Sys.getenv("USERNAME")=='jasper')

fit1=BIOMOD_Modeling(data=data, models='MAXENT', 
                     model.options=myBiomodOptions,
                     NbRunEval= 10, 
                     DataSplit=70, 
                     VarImport=0, 
                     modeling.id = as.character(format(Sys.time(), "%s")))

###############