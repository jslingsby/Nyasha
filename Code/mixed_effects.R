##############################################################################
######## Testing for the effect of life history traits on the contribution of 
######## the fire variable (rec_rate) towards redcting secies occurence 
##############################################################################
######## Compiled by Nyasha Magadzire 2016
######## Last edited: 12 September 2016
######## Data: 
######## 1) Species trait and var contribution table for enhanced SDM
##############################################################################
#if (Sys.getenv("USERNAME")=='nyasha') {setwd ("C:/PhD_GIT/Nyasha")}
#if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha")}

library(nlme)
library(MuMIn)

#esdm <- read.csv("F:/Nyasha/Data/MaxEnt_Analysis/LME_perccont.csv", header=T)
esdm <- read.csv("Data/MaxEnt_Analysis/LME_perccont.csv", header=T)

tesdm <- esdm[which(esdm$FAMILY%in%c("CELASTRACEAE","CUPRESSACEAE","PODOCARPACEAE")),]

#Trim out trees and reclassify medium and tall shrubs to "Tall shrubs"
sesdm <- esdm[-which(esdm$FAMILY%in%c("CELASTRACEAE","CUPRESSACEAE","PODOCARPACEAE")),]
sesdm$Growth_form[which(sesdm$Growth_form=="Medium shrub")] <- "Tall shrub"
sesdm <- droplevels(sesdm)

shesdm <- sesdm
shesdm$Growth_form <- as.character(shesdm$Growth_form)
shesdm$Growth_form[which(shesdm$Growth_form %in% c("Tall shrub", "Low shrub"))] <- "Shrub"
shesdm <- droplevels(shesdm)

gesdm <- droplevels(esdm[which(esdm$Growth_form=="Graminoid"),])

#Inspect the data
boxplot(rec_rate ~ Fire_response + Growth_form, data=esdm)
boxplot(rec_rate ~ Fire_response + Growth_form, data=sesdm)

#Fit model to full data (including the trees...)
fit = lme(rec_rate ~ Fire_response * Growth_form, random = ~1|Pair, data=esdm, method="ML") #Run through all models
dd <- dredge(fit)
dd #All moodels are within 2.6 delta AUC so you can't really exclude any of them... best to just look at the full model
#model.avg(dd, subset = delta < 4) #Does model averaging if there are many alternate models
#summary(get.models(dd, 4)[[1]]) #best model
summary(fit)

#Fit model to full data (excluding the trees...)
fit = lme(rec_rate ~ Fire_response * Growth_form, random = ~1|Pair, data=shesdm, method="ML") #Run through all models
dd <- dredge(fit)
dd #All moodels are within 1.06 delta AUC so you can't really exclude any of them... best to just look at the full model
summary(fit)

#Let's look at the results if we don't use a random effect? (i.e. is phylogeny important here?)
summary(lm(rec_rate ~ Fire_response * Growth_form, data=sesdm))
summary(lm(rec_rate ~ Fire_response + Growth_form, data=sesdm))
summary(lm(rec_rate ~ Growthform_trait, data=sesdm))

#Graminoids only - just for fun
fit = lme(rec_rate ~ Fire_response * Growth_form, random = ~1|Pair, data=shesdm, method="ML") #Run through all models
summary(fit)

