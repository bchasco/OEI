#  This script will download the entire PDO time series,
#    create monthly and seasonal indices,
#    and load them into the database

library(RODBC)
library(dplyr)
library(tidyr)

rm(list=ls())

#**********************************************
#    Download PDO
#**********************************************

#PDO INDEX
#
# Updated standardized values for the PDO index, derived as the 
#  leading PC of monthly SST anomalies in the North Pacific Ocean, 
#  poleward of 20N. The monthly mean global average SST anomalies
#  are removed to separate this pattern of variability from any 
#  "global warming" signal that may be present in the data. 
#
#
# For more details, see:
#  
#  Zhang, Y., J.M. Wallace, D.S. Battisti, 1997: 
#  ENSO-like interdecadal variability: 1900-93. J. Climate, 10, 1004-1020. 
#
# Mantua, N.J. and S.R. Hare, Y. Zhang, J.M. Wallace, and R.C. Francis,1997: 
#   A Pacific interdecadal climate oscillation with impacts on salmon 
#   production. Bulletin of the American Meteorological Society, 78, pp. 1069-1079.
#   (available via the internet at url: http://www.atmos.washington.edu/~mantua/abst.PDO.html) 
#
# missing value flag is -9999

pdo<-read.csv("https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_PDO.csv?time,PDO", header = TRUE, stringsAsFactors = FALSE)

# First row is a second header and needs to be removed
pdo<-pdo[-1,]

head(pdo)
tail(pdo)
str(pdo)

# Flatten it and add back year and month
pdo$year<-as.numeric(substr(pdo$time,1,4))
pdo$month <- as.numeric(substr(pdo$time,6,7))
pdo<-pdo[,c(3,4,2)]
colnames(pdo)[3]<-"index"

# We can plot it as is...
pdo.ts<-ts(pdo$index, start=pdo$year[1], frequency=12)
plot(pdo.ts)
abline(0,0)
# Zoom in
plot(pdo.ts, xlim=c(1990,2021))
abline(0,0)

# Filled lines
# How much do we want to plot?
pdo.temp<-pdo[pdo$year>=1950,]
# Create an x variable that goes left to right then right back to left
xx<-pdo.temp$year+(pdo.temp$month-1)/12
xx <- c(xx, rev(xx))
yy.red <- c(rep(0, nrow(pdo.temp)), rev(pdo.temp$index))
yy.red[yy.red<0 | is.na(yy.red)]<-0
yy.blue <- c(rep(0, nrow(pdo.temp)), rev(pdo.temp$index))
yy.blue[yy.blue>0 | is.na(yy.blue)]<-0
# because polygon() is dumb and wants a pre-existing plot
plot(pdo.temp$year, pdo.temp$index, ylim=c(min(pdo.temp$index, na.rm=TRUE), max(pdo.temp$index, na.rm=TRUE)), type="n", xlab="", ylab="PDO")
polygon(xx, yy.red, border=NA, col="red")
polygon(xx, yy.blue, border=NA, col="blue")

# Get the seasonal indices
# Seasons are:
#  win: Dec-Feb
#  spr: Mar-May
#  sum: Jun-Aug
#  aut: Sep-Nov

# For winter, we need to manually change the year for December, so it is included in the following year
toChange<-which(pdo$month == 12)
pdo.win<-pdo
pdo.win$year[toChange]<-pdo.win$year[toChange]+1
pdo.win<-pdo.win[pdo.win$month %in% c(12,1,2),]
pdo.win<-aggregate(pdo.win$index, by=list(pdo.win$year), mean, na.rm=TRUE)
colnames(pdo.win)<-c("year","pdo.win")
#write.csv(pdo.win, "pdo.win.csv", row.names = FALSE)

pdo.spr<-pdo[pdo$month %in% 3:5,]
pdo.spr<-aggregate(pdo.spr$index, by=list(pdo.spr$year), mean, na.rm=TRUE)
colnames(pdo.spr)<-c("year","pdo.spr")
#write.csv(pdo.spr, "pdo.spr.csv", row.names = FALSE)

pdo.sum<-pdo[pdo$month %in% 6:8,]
pdo.sum<-aggregate(pdo.sum$index, by=list(pdo.sum$year), mean, na.rm=TRUE)
colnames(pdo.sum)<-c("year","pdo.sum")
#write.csv(pdo.sum, "pdo.sum.csv", row.names = FALSE)

pdo.aut<-pdo[pdo$month %in% 9:11,]
pdo.aut<-aggregate(pdo.aut$index, by=list(pdo.aut$year), mean, na.rm=TRUE)
colnames(pdo.aut)<-c("year","pdo.aut")
#write.csv(pdo.aut, "pdo.aut.csv", row.names = FALSE)

# Get the May-Sep index (the stoplight chart uses the SUM of the monthly values)
pdoMaySep<-pdo[pdo$month %in% 5:9,]
pdoMaySep<-aggregate(pdoMaySep$index, by=list(pdoMaySep$year), sum, na.rm=TRUE)
colnames(pdoMaySep)<-c("year","pdo.sum")
#write.csv(pdoMaySep, "pdoMaySep.csv", row.names = FALSE)

# and Dec-Mar index
toChange<-which(pdo$month == 12)
pdoDecMar<-pdo
pdoDecMar$year[toChange]<-pdoDecMar$year[toChange]+1
pdoDecMar<-pdoDecMar[pdoDecMar$month %in% c(12, 1:3),]
pdoDecMar<-aggregate(pdoDecMar$index, by=list(pdoDecMar$year), sum, na.rm=TRUE)
colnames(pdoDecMar)<-c("year","pdo.win")
#write.csv(pdoDecMar, "pdoDecMar.csv", row.names = FALSE)

# Plot the seasonal means
cols<-c("blue","darkgreen","red","orange")
plot(pdo.win$year, pdo.win$pdo.win, type='l', lty=1, col=cols[1], xlim=c(1970,2022), ylab="Seasonal PDO Index", xlab="")
lines(pdo.spr$year, pdo.spr$pdo.spr, col=cols[2])
lines(pdo.sum$year, pdo.sum$pdo.sum, col=cols[3])
lines(pdo.aut$year, pdo.aut$pdo.aut, col=cols[4])
abline(0,0)
legend("bottomright", legend = c("Winter","Spring","Summer","Autumn"), col = cols, lty = 1, bty='n')


#**********************************************
#    Push data to database
#**********************************************

# Create the channel to connect to the database
channel <- odbcConnect("NWFSC_OCEAN", uid="your username goes here", pwd="your password goes here")

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

# What is the value of PDO for Jan, 2021?
existingData %>% filter(YYYY==2021, MM==1) %>% select(PDO)
# is it the same as what we just downloaded?
pdo %>% filter(year==2021, month==1) %>% select (index)

# Just checking:
mergedData<-merge(pdo, existingData, by.x = c('year','month'), by.y=c("YYYY","MM"))
plot(mergedData$PDO, mergedData$index)
hist(mergedData$PDO-mergedData$index)


# There's got to be a better way than this than row by row...
for (ii in 1:nrow(pdo)) {
  if (pdo$year[ii] < min(existingData$YYYY)) next
  sqlQuery(channel, paste('update [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]
         SET [PDO] = ', eval(pdo$index[ii]),
         'WHERE [YYYY]=', eval(pdo$year[ii]), 'AND [MM]=', eval(pdo$month[ii])))
}

