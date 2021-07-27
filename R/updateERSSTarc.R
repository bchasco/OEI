#  This script will download the necessary ERSST data,
#    create monthly and seasonal indices from the arc region (JOhnstone and Mantua 2014),
#    and load them into the database

library(RODBC)
library(dplyr)
library(tidyr)
library(rnoaa)

rm(list=ls())


updateWebdata <- function(pwd=pwd, uid = uid, covariate = "erSST"){
  connectoToDb(uid,pwd) #This is a connect function that is already in the package OEI
 }

# Import the boundary file from Johnstone
wholeGrid<-read.table("./R/yx3", header = FALSE)
arc<-read.table("./R/arc3", header = FALSE)
arc<-wholeGrid[t(arc),]
# plot(wholeGrid$V2, wholeGrid$V1)
# points(arc$V2, arc$V1, col="red")


# We need the lat and long for the arc area, not just the index for them
#  use any file for this
anyfile <- ersst(year = 2010, month = 2)
names(anyfile)
lon <- ncdf4::ncvar_get(anyfile,"lon")
lat <- ncdf4::ncvar_get(anyfile,"lat",verbose=F)
# use the index (arc) to get locations
arcLocsY<-match(arc$V1, lat)
arcLocsX<-match(arc$V2, lon)
#plot(cbind(arcLocsX, arcLocsY))

# Get the mean SSTa from the arc area (this could take awhile, the first time you run it)
meanArc<-data.frame(year=as.numeric(), month=as.numeric(), sstarc=as.numeric(), stringsAsFactors = FALSE)
# Loop over files and get mean arc anomaly
for (yy in 1970:2021) {
  for (mm in 1:12) {
    #if ( yy==2017 & mm > 4) next
    try({
      thisfile <- ersst(year = yy, month = mm)
      ssta <- ncdf4::ncvar_get(thisfile,"ssta")
      meanArc[nrow(meanArc)+1,]<-data.frame(yy, mm, mean(ssta[cbind(arcLocsX, arcLocsY)], na.rm = TRUE), stringsAsFactors = FALSE)
    }
    ,silent = T)
  }
}

plot(meanArc$year+meanArc$month/12, meanArc$sstarc, type='l', xlab="", ylab="SSTarc")

#**********************************************
#    Push data to database
#**********************************************

# Create the channel to connect to the database
channel <- odbcConnect("NWFSC_OCEAN", uid=uid, pwd=pwd)

# Query the existing data, we want to know what years are in there
existingData<-sqlQuery(channel, "SELECT [SigmaPlotDate]
                                  ,[YYYY]
                                  ,[MM]
                                  ,[NPGO]
                                  ,[PDO]
                                  ,[ONI]
                                  ,[NPI]
                                  ,[SSTARC]
                       FROM [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]")

# What is the value of SSTarc for Jan, 2019?
existingData %>% filter(YYYY==2019, MM==1) %>% select(SSTARC)
# is it the same as what we just downloaded?
meanArc %>% filter(year==2019, month==1) %>% select (sstarc)

# Just checking:
mergedData<-merge(meanArc, existingData, by.x = c('year','month'), by.y=c("YYYY","MM"))
plot(mergedData$SSTARC, mergedData$sstarc)
hist(mergedData$SSTARC-mergedData$sstarc)


# There's got to be a better way than this than row by row...
for (ii in 1:nrow(meanArc)) {
  if (meanArc$year[ii] < min(existingData$YYYY)) next
  sqlQuery(channel, paste('update [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]
         SET [SSTARC] = ', eval(meanArc$sstarc[ii]),
                          'WHERE [YYYY]=', eval(meanArc$year[ii]), 'AND [MM]=', eval(meanArc$month[ii])))
}

