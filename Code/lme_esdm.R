##############################################################################
######## Script to test for phylogenet in Nyasha's data for species distribution modelling chapter
##############################################################################
######## Compiled by Nyasha Magadzire 2016
######## Last edited: 15 February 2016
######## Data: 
######## 1) Species trait and var contribution table for enhanced SDM
##############################################################################

setwd ("C:/PhD_GIT/Nyasha")
library(nlme)
esdm <- read.csv("C:/PhD_GIT/Nyasha/Data/MaxEnt_Analysis/lme_esdm_data2.csv", header=T)

fit1 = lme(Geology ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
summary(fit1)
coef(fit1)
plot(ranef(fit1))
plot(fit1)

fit2 = lme(FRI ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit2)
plot(ranef(fit2))
plot(fit2)


fit3 = lme(Max_NDVI ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit3)
plot(ranef(fit3))
plot(fit3)

fit4 = lme(Rainfall ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit4)
plot(ranef(fit4))
plot(fit4)

fit5 = lme(Seasonality ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit5)
plot(ranef(fit5))
plot(fit5)

fit6 = lme(Max_temp ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit6)
plot(ranef(fit6))
plot(fit6)

fit7 = lme(Max_temp ~ Growth_form * Fire_response, random = ~1|Lineage/TAXON, data=esdm, method="ML")
coef(fit7)
plot(ranef(fit7))
plot(fit7)
