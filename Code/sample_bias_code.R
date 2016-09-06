

###Get a raster?

#Convert raster to a table of all possible localities
x <- rasterToPoints(r) #where r is any of the environmental rasters trimmed to the correct extent

#Set up lists to store p-values
ks <- list()
mu <- list()

for (i in 1:100) #loop through 100 samples - you could make it 500 or 1000 if it runs quickly?
{
dat <- x[sample(1:nrow(x), 110),] #sample 110 random localities (not sure how many you need?)
###Add roads setup?
ks[[i]] <- ks.test(the details)$p.value #Kolmogorov-Smirnov - set up "the details"?
mu[[i]] <- wilcox.test(the details)$p.value #Mann-Whitney U
}

hist(as.numeric(ks))
hist(as.numeric(mu))
