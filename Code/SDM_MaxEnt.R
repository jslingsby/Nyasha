##############################################################################
######## System calls to run MaxEnt models for Nyasha's species distribution modelling chapter 
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 7 November 2014
##############################################################################
###!!!!!!!THIS IS NOT FOR RUNNING IN R!!!!!!!!
##############################################################################

##############################################################################
###1) Calls to MaxEnt - Click on "Help"in the MaxEnt graphical user interface to see what the options mean
##############################################################################

#basic call
#java -mx512m -jar maxent.jar environmentallayers=env_layers samplesfile=loc.csv outputdirectory=maxentoutput redoifexists autorun

#is the same as (shortened form)
#java -mx512m -jar maxent.jar -e env_layers -s loc.csv -o maxentoutput -r -a

#the pimped version for Jasper's office workstation
#java -mx12288m -jar maxent.jar -e env_layers -s loc.csv -J true -z false threads=4 writeplotdata=true replicates=10 -o maxentoutput -r -a

##############################################################################
###2) Set batch to run from R
##############################################################################

if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha"); maxdat="/Applications/maxent/"; maxent="/Applications/maxent/maxent.jar"}
if (Sys.getenv("USERNAME")=='jasper') 
{
  #setwd("C:/Users/jasper/Git/Nyasha") 
  maxdat="C:/Extra software/maxent/"
  maxent="C:/somewhere"
  setwd(maxent)
  call="java -mx12288m -jar maxent.jar -e env_layers -s loc.csv -J true -z false threads=4 writeplotdata=true replicates=10 -o maxentoutput -r -a"
  system(call, wait=FALSE)
}

if (Sys.getenv("USERNAME")=='Nyasha') {setwd("C:/Users/Nyasha/Git/Nyasha"); maxdat= "C:/somewhere" ; maxent="C:/somewhere"}


######################################################################
###3) Running MaxEnt one species at a time through R
######################################################################
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/GIT/Nyasha"); maxdat="/Applications/maxent/"; maxent="/Applications/maxent/maxent.jar"}
if (Sys.getenv("USERNAME")=='jasper') {setwd("C:/Users/jasper/Git/Nyasha"); maxdat="C:/Extra software/maxent/"; maxent="C:/somewhere"}
if (Sys.getenv("USERNAME")=='Nyasha') {setwd("C:/Users/Nyasha/Git/Nyasha"); maxdat= "C:/somewhere" ; maxent="C:/somewhere"}

refs=read.csv("Data/Vera_Hoffman/nyashafocalspecies2014-11-06.csv", header=T, row.names=1, stringsAsFactors=F)

setwd(maxdat)

pnames<-unique(refs[,1])

for(i in 1:length(pnames))
{
  local<-refs[which(refs$Species==pnames[i]),]
  
  ### Write out Locality file
  write.csv(local, "singleloc.csv", row.names=FALSE)
  
  ### Set batch file details - see setMEbatch in climateTools.R
  call <-"java -mx512m -jar maxent.jar environmentallayers=env_layers samplesfile=singleloc.csv outputdirectory=maxentoutput redoifexists autorun"
  system(call, wait=TRUE)
  # system("D:/GIS/MaxEnt/maxent/batchProtea.bat", wait=TRUE)
}