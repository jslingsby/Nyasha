###############################Plotting vegtype frequencies using combined climate scenarios#################################

if (Sys.getenv("USER")=='nyasha') {setwd("C:/Users/nyasha/Desktop/veg_modelling/")}

if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/Dropbox/Shared/Nyasha/Data/")}

library(ggplot2)
library(gridExtra)
library(scales)
library(lme4)
library(dplyr)
library(broom)

deltas = read.csv("output4/combined_percent_cover.csv")

###################scatter plots percent cover~ chane in fri/map across 44 climatic scenarios 
vegtypes <- levels(deltas[,"names.x"])

RCP_comb <- lapply(vegtypes, function(vegtype) {
  ggplot(deltas[deltas[, "names.x"]==vegtype,], aes(x=(delta.mean.fri), y=percent.cover)) +
    facet_wrap(~names.x, ncol = 1) + # show each vegtype in a seperate facet
    geom_point() +
    geom_smooth(method="lm")+
    scale_y_continuous(breaks=pretty_breaks(n=3))+
    theme(axis.title = element_blank(),axis.text = element_text(size=12,colour="black"),
          strip.text = element_text(size=12)) 
})   

do.call(grid.arrange, c(RCP_comb, nrow=6))

RCP_comb <- lapply(vegtypes, function(vegtype) {
  ggplot(deltas[deltas[, "names.x"]==vegtype,], aes(x=(delta.mean.map), y=percent.cover)) +
    facet_wrap(~names.x, ncol = 1) + # show each vegtype in a seperate facet
    geom_point() +
    geom_smooth(method="lm") +
    scale_y_continuous(breaks=pretty_breaks(n=3))+
    theme(axis.title = element_blank(),axis.text = element_text(size=12,colour="black"),
          strip.text = element_text(size=12)) 
})   


do.call(grid.arrange, c(RCP_comb, nrow=6))

####get statistics (r square, p value, etc) - stats were too crowded in the plots so calculated them separately
################################################################################################################    
fitted_models1 <- deltas %>% group_by(names.x) %>% do(model = lm(percent.cover ~ delta.mean.fri, data = .))  
stats_fri <- fitted_models1 %>% glance(model)

fitted_models2 <- deltas %>% group_by(names.x) %>% do(model = lm(percent.cover ~ delta.mean.map, data = .))  
stats_map <- fitted_models2 %>% glance(model)