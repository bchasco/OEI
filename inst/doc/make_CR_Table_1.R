## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(OEI)
library(knitr)
library(dplyr)#You shouldn't have to include these
library(tidyverse)#You shouldn't have to include these
uid<-"Brian.Burke"
pwd<-"Burke.Brian"

## ----getTable-------------------------------------------------------
trawlInfo<-getTable(uid = uid, pwd=pwd, schemaName = "crepo", tableName = "Trawl Info")
stationInfo<-getTable(uid = uid, pwd=pwd, schemaName = "crepo", tableName = "Station Info")

## ----refine data----------------------------------------------------
trawlInfo<-trawlInfo[trawlInfo$"Cruise #"==62,]
stationInfo<-stationInfo[stationInfo$"Cruise #"==62,]
tableData<-merge(trawlInfo, stationInfo, by="Station Code")
finalData<-tableData[,c("Transect Name","Sample Date","Haul #","Station #","Depth (m)",
                        "Start Lat (decimal degrees)","Start Long (decimal degrees)",
                        "Trawling distance (km)","Bearing (degrees)",
                        "Start Time","End Time")]
OEI::plot(df=data.frame(x=finalData$`Depth (m)`,
                        y=finalData$`Trawling distance (km)`,
                        Index=finalData$`Transect Name`), 
          multipane = TRUE, 
          ncol=3)


## ----make table, echo=FALSE-----------------------------------------
kable(finalData)

