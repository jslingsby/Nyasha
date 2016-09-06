##############################################################################
######## Script to test for phylogenet in Nyasha's data for species distribution modelling chapter
##############################################################################
######## Compiled by Nyasha Magadzire 2016
######## Last edited: 15 February 2016
######## Data: 
######## 1) Species trait and var contribution table for enhanced SDM
##############################################################################
if (Sys.getenv("USERNAME")=='nyasha') {setwd ("C:/PhD_GIT/Nyasha")}
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha")}

library(nlme)
library(lme4)
library(MuMIn)
esdm <- read.csv("Data/MaxEnt_Analysis/lme_esdm_data2.csv", header=T)
esdm$pair <- paste("pair", sort(c(1:55,1:55)), sep="") 
esdm$Life_history <- factor(esdm$Growth_form:esdm$Fire_response) #Make one factor that incorporates fire response and growth form

#FRI
boxplot(FRI ~ Fire_response + Growth_form, data=esdm)
boxplot(FRI ~ Life_history, data=esdm)

fit = lme(FRI ~ Fire_response + Growth_form, random = ~1|pair, data=esdm, method="ML")
summary(fit)

fit = lme(FRI ~ Life_history, random = ~1|pair, data=esdm, method="ML")
summary(fit)

fit = lme(FRI ~ Fire_response * Growth_form, random = ~1|pair, data=esdm, method="ML") #Run through all models
dd <- dredge(fit)
dd
model.avg(dd, subset = delta < 4)
summary(get.models(dd, 1)[[1]]) #best model

#Max_temp
boxplot(Max_temp ~ Fire_response + Growth_form, data=esdm)
boxplot(Max_temp ~ Life_history, data=esdm)

fit = lme(Max_temp ~ Fire_response + Growth_form, random = ~1|pair, data=esdm, method="ML")
summary(fit)

fit = lme(Max_temp ~ Life_history, random = ~1|pair, data=esdm, method="ML")
summary(fit)

fit = lme(Max_temp ~ Fire_response * Growth_form, random = ~1|pair, data=esdm, method="ML")
dd <- dredge(fit)
dd
model.avg(dd, subset = delta < 4)
summary(get.models(dd, 1)[[1]]) #best model

#FRI
fit2 = lme(FRI ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(FRI ~ Growth_form, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(FRI ~ Lineage, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)


#Max_NDVI
fit2 = lme(Max_NDVI ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(Max_NDVI ~ Growth_form, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(Max_NDVI ~ Lineage, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)


#Seasonality
fit2 = lme(Seasonality ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(Seasonality ~ Growth_form, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

fit2 = lme(Seasonality ~ Lineage, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)
coef(fit2)
plot(ranef(fit2))
plot(fit2)

###########

fit3 = lme(Max_NDVI ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit3)
coef(fit3)
plot(ranef(fit3))
plot(fit3)

fit4 = lme(Rainfall ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit4)
coef(fit4)
plot(ranef(fit4))
plot(fit4)

fit5 = lme(Seasonality ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit5)
coef(fit5)
plot(ranef(fit5))
plot(fit5)

fit6 = lme(Max_temp ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit6)
coef(fit6)
plot(ranef(fit6))
plot(fit6)

fit7 = lme(Min_temp ~ Fire_response, random = ~1|pair, data=esdm, method="ML")
summary(fit7)
coef(fit7)
plot(ranef(fit7))
plot(fit7)


#Scrap code looking at Growth form classifications
esdm$GF_alt <- as.factor(c(rep("Tree", 6), rep("Shrub", 52), rep("Graminoid", 52)))
esdm$GF_alt <- relevel(esdm$GF_alt, "Tree")

esdm$GF_alt2 <- as.factor(c(rep("Tree", 6), rep("Shrub", 52), as.character(esdm$Lineage[59:110])))
esdm$GF_alt2 <- relevel(esdm$GF_alt2, "Tree")

esdm$GF_alt3 <- rep("Fine-leaved shrub", 110)
esdm$GF_alt3[which(esdm$Lineage%in%c("PROTEACEAE", "CELASTRACEAE", "EUPHORBIACEAE", "FABACEAE", "PODOCARPACEAE"))] <- "Broad-leaved shrub"
esdm$GF_alt3[which(esdm$Lineage%in%c("POACEAE", "RESTIONACEAE"))] <- "Graminoid"
esdm$GF_alt3 <- as.factor(esdm$GF_alt3)
esdm$GF_alt3 <- relevel(esdm$GF_alt3, "Fine-leaved shrub")