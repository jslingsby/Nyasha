###Code to calculate seasonal weather variables from Adam's interpolated daily data using CDO Tools
###Only runs on Jasper's Mac...

setwd("/Users/jasper/Documents/PostDoc/Connecticut/Adam/weatherdat")
#cd /Users/jasper/Documents/PostDoc/Connecticut/Adam/weatherdat
#cdo eca_cdd,2 -shifttime,6months weather_fine.nc cdd_annualmean.nc
#selseas,DJF, MAM, JJA, SON

###Set up CDO calls
tmx="cdo yseasmean -selvar,tmax_mean weather_fine.nc tmaxseasmean.nc" #Tmax_mean by season
tmn="cdo yseasmean -selvar,tmin_mean weather_fine.nc tminseasmean.nc" #Tmin_mean by season
ppt="cdo yseasmean -selvar,ppt_mean weather_fine.nc pptseasmean.nc" #Ppt_mean (daily) by season

###Run CDO calls to Terminal
system(tmx, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL)
system(tmn, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL)
system(ppt, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL)


tmxseas=stack("tmaxseasmean.nc")
names(tmxseas)=c("DJF", "MAM", "JJA", "SON")

tmnseas=stack("tminseasmean.nc")
names(tmnseas)=c("DJF", "MAM", "JJA", "SON")

pptseas=stack("pptseasmean.nc")
names(pptseas)=c("DJF", "MAM", "JJA", "SON")

###########################################
###End
###########################################

x=raster("cdd_annualmean.nc", varname="consecutive_dry_days_index_per_time_period")
y=raster("cdd_annualmean.nc", varname="number_of_cdd_periods_with_more_than_5days_per_time_period")

p=raster("cdd_annualmean_unshift.nc", varname="consecutive_dry_days_index_per_time_period")
q=raster("cdd_annualmean_unshift.nc", varname="number_of_cdd_periods_with_more_than_5days_per_time_period")

cdd=stack("/Users/jasper/Dropbox/Shared/Kobus/Data/Climate/CDD_Q50.nc")
NAvalue(cdd)=-999
cdd_M=mean(cdd, na.rm=T)

gdd=stack("/Users/jasper/Dropbox/Shared/Kobus/Data/Climate/GDD_Q50.nc")
NAvalue(gdd)=-999
gdd_M=mean(gdd, na.rm=T)

calc(x, fun=sd)
