#  This script will download the entire NPGO time series,
#    create monthly and seasonal indices,
#    and load them into the database

library(RODBC)
library(dplyr)
library(tidyr)

rm(list=ls())

# User info
uid="your username goes here"
pwd="your username goes here"


##### NPGO ####

#**********************************************
#    NPGO
#**********************************************

#  Last update 03-Jul-2020 by E. Di Lorenzo 
#  NPGO index monthly averages 
#  from Jan-1950  to  Jul-2020 
# 
#  WARNING: Values after Dec-2004 are updated  
#  using Satellite SSHa from AVISO Delayed Time product.   
#  http://opendap.aviso.oceanobs.com/thredds/dodsC/dataset-duacs-dt-global-allsat-msla-h 
# 
#  PRELIMINARY: Values after May-2019 are preliminary and updated  
#  using Satellite SSHa from AVISO Near Real Time product.   
#  http://opendap.aviso.oceanobs.com/thredds/dodsC/dataset-duacs-nrt-over30d-global-allsat-msla-h 
# 
#  The update is performed by taking the NPGO spatial pattern of Di Lorenzo et al. 2008 
#  computed over the period 1950-2004, and projecting the AVISO Satellite SSHa. 
#  During the pre-processing of the AVISO data, we remove the seasonal cycle based on 
#  the 1993-2004 seasonal means. 
# 
#  AVISO PRODUCT UPDATE Summer 2014: AVISO has released a re-processed dataset for the sea level. 
#  Starting from the November 2014, the NPGO index is computed with this updated dataset. NPGO 
#  values from 2004 onward have been recomputed with very minor differences from previous releases. 
# 
#  Ref: 
#  Di Lorenzo et al., 2008: North Pacific Gyre Oscillation  
#  links ocean climate and ecosystem change, GRL. 
#
#      YEAR            MONTH        NPGO index 

npgo <- readLines("http://www.o3d.org/npgo/npgo.php")
# I'm keeping any line with 3 items in it...  Luckily that works.
counts <- count.fields(textConnection(npgo), blank.lines.skip = FALSE)
npgo<-read.table(text=npgo[counts == 3], header=FALSE, stringsAsFactors = FALSE)
colnames(npgo)<-c("year","month","index")
npgo$date<-strptime(paste(npgo$year, formatC(npgo$month, width = 2, format = "d", flag = "0"), "15", sep="-"), format='%Y-%m-%d')

head(npgo)
tail(npgo, 20)
str(npgo)

plot(npgo$date, npgo$index, type='b')
# Zoom in
plot(ts(npgo$index, start=min(npgo$year), frequency=12), xlim=c(1990,2021), ylab="NPGO")
abline(0,0)

# Seasonal indices
toChange<-which(npgo$month == 12)
npgo.win<-npgo
npgo.win$year[toChange]<-npgo.win$year[toChange]+1
npgo.win<-npgo.win[npgo.win$month %in% c(12,1,2),]
npgo.win<-aggregate(npgo.win$index, by=list(npgo.win$year), mean, na.rm=TRUE)
colnames(npgo.win)<-c("year","npgo.win")
#write.csv(npgo.win, "npgo.win.csv", row.names = FALSE)

npgo.spr<-npgo[npgo$month %in% 3:5,]
npgo.spr<-aggregate(npgo.spr$index, by=list(npgo.spr$year), mean, na.rm=TRUE)
colnames(npgo.spr)<-c("year","npgo.spr")
#write.csv(npgo.spr, "npgo.spr.csv", row.names = FALSE)

npgo.sum<-npgo[npgo$month %in% 6:8,]
npgo.sum<-aggregate(npgo.sum$index, by=list(npgo.sum$year), mean, na.rm=TRUE)
colnames(npgo.sum)<-c("year","npgo.sum")
#write.csv(npgo.sum, "npgo.sum.csv", row.names = FALSE)

npgo.aut<-npgo[npgo$month %in% 9:11,]
npgo.aut<-aggregate(npgo.aut$index, by=list(npgo.aut$year), mean, na.rm=TRUE)
colnames(npgo.aut)<-c("year","npgo.aut")
#write.csv(npgo.aut, "npgo.aut.csv", row.names = FALSE)

# summer
npgo.aprsep<-npgo[npgo$month %in% c(4:9),]
npgo.aprsep<-aggregate(npgo.aprsep$index, by=list(npgo.aprsep$year), mean, na.rm=TRUE)
colnames(npgo.aprsep)<-c("year","npgo.sum")
plot(npgo.aprsep, type='b')
# output results
#write.csv(npgo.aprsep, "npgoAprSept.csv", row.names = FALSE)

# winter index
toChange<-which(npgo$month == 12)
npgo.decmar<-npgo
npgo.decmar$year[toChange]<-npgo.decmar$year[toChange]+1
npgo.decmar<-npgo.decmar[npgo.decmar$month %in% c(12, 1:3),]
npgo.decmar<-aggregate(npgo.decmar$index, by=list(npgo.decmar$year), sum, na.rm=TRUE)
colnames(npgo.decmar)<-c("year","npgo.win")
plot(npgo.decmar, type='b')
# output results
#write.csv(npgo.decmar, "npgoDecMar.csv", row.names = FALSE)

# Filled lines
# Create an x variable that goes left to right then right back to left
# How much do we want to plot?
npgo.temp<-npgo[npgo$year>=2010,]
xx<-npgo.temp$year+(npgo.temp$month-1)/12
xx <- c(xx, rev(xx))
yy.red <- c(rep(0, nrow(npgo.temp)), rev(npgo.temp$index))
yy.red[yy.red<0 | is.na(yy.red)]<-0
yy.blue <- c(rep(0, nrow(npgo.temp)), rev(npgo.temp$index))
yy.blue[yy.blue>0 | is.na(yy.blue)]<-0
# because polygon() is dumb and wants a pre-existing plot
plot(npgo.temp$year, npgo.temp$index, ylim=c(min(npgo.temp$index, na.rm=TRUE), max(npgo.temp$index, na.rm=TRUE)), type="n", xlab="", ylab="NPGO")
polygon(xx, yy.red, border=NA, col="red")
polygon(xx, yy.blue, border=NA, col="blue")

# Get the Jan-Jun index (the stoplight chart uses the mean of the monthly values)
npgoMaySept<-npgo[npgo$month %in% 5:9,]
npgoMaySept<-aggregate(npgoMaySept$index, by=list(npgoMaySept$year), mean, na.rm=TRUE)
colnames(npgoMaySept)<-c("year","index")
plot(npgoMaySept, type='b')
abline(0,0)

rm(counts); rm(toChange); rm(xx); rm(yy.blue); rm(yy.red)

#********************************
# Plot the seasonal means
#********************************
cols<-c("blue","darkgreen","red","orange")
plot(npgo.win$year, npgo.win$npgo.win, type='l', lty=1, col=cols[1], xlim=c(1970,2022), ylab="Seasonal npgo Index", xlab="")
lines(npgo.spr$year, npgo.spr$npgo.spr, col=cols[2])
lines(npgo.sum$year, npgo.sum$npgo.sum, col=cols[3])
lines(npgo.aut$year, npgo.aut$npgo.aut, col=cols[4])
abline(0,0)
legend("bottomright", legend = c("Winter","Spring","Summer","Autumn"), col = cols, lty = 1, bty='n')


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

# What is the value of PDO for Jan, 2021?
existingData %>% filter(YYYY==2019, MM==1) %>% select(NPGO)
# is it the same as what we just downloaded?
npgo %>% filter(year==2019, month==1) %>% select (index)

# Just checking:
mergedData<-merge(npgo, existingData, by.x = c('year','month'), by.y=c("YYYY","MM"))
plot(mergedData$NPGO, mergedData$index)
hist(mergedData$NPGO-mergedData$index)


# There's got to be a better way than this than row by row...
for (ii in 1:nrow(npgo)) {
  if (npgo$year[ii] < min(existingData$YYYY)) next
  sqlQuery(channel, paste('update [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]
         SET [NPGO] = ', eval(npgo$index[ii]),
         'WHERE [YYYY]=', eval(npgo$year[ii]), 'AND [MM]=', eval(npgo$month[ii])))
}

