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
esdm <- read.csv("Data/MaxEnt_Analysis/lme_esdm_data2.csv", header=T)
esdm$pair <- paste("pair", sort(c(1:55,1:55)), sep="") 

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
