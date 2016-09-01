##############################################################################
######## Testing for the effect of life history traits on the contribution of 
######## the fire variable (rec_rate) towards redcting secies occurence 
##############################################################################
######## Compiled by Nyasha Magadzire 2016
######## Last edited: 1 September 2016
######## Data: 
######## 1) Species trait and var contribution table for enhanced SDM
##############################################################################
library(nlme)
esdm <- read.csv("F:/Nyasha/Data/MaxEnt_Analysis/LME_perccont.csv", header=T)

fit1 = lme(rec_rate ~ Fire_response, random = ~1|Pair, data=esdm, method="ML")
summary(fit1)


fit2 = lme(rec_rate ~ Growth_form, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit2)


fit3 = lme(rec_rate ~ FAMILY, random = ~1|Fire_response, data=esdm, method="ML")
summary(fit3)

library(car)
library(lme4)


lmm1 <- lmer(rec_rate ~ Growth_form * Fire_response + (1 |FAMILY), data = esdm,REML = FALSE)
summary(lmm1)
Anova(lmm1)


lmm2 <- lmer(rec_rate ~ Growth_form * Fire_response + (1 |Pair), data = esdm,REML = FALSE)
summary(lmm2)
Anova(lmm2)

lmm3 <- lmer(rec_rate ~ Growth_form * Fire_response + (1 |Pair) + (1 |FAMILY), data = esdm,REML = FALSE)
summary(lmm3)
Anova(lmm3)


