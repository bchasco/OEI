#  This script will download the entire NPI time series,
#    create monthly and seasonal indices,
#    and load them into the database

library(RODBC)
library(dplyr)
library(tidyr)

rm(list=ls())



##### ONI ####
#******************************
# Bring in the ONI
#******************************

# From http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
# But saved locally

oni<-readLines("https://psl.noaa.gov/data/correlation/oni.data")

# I'm keeping any line with 13 items in it...  Luckily that works.
counts <- count.fields(textConnection(oni), blank.lines.skip = FALSE)
oni<-read.table(text=oni[counts == 13], header=FALSE, stringsAsFactors = FALSE)

# Flatten it and add back year and month
oni2<-as.data.frame(as.vector(t(oni[,2:13])))
colnames(oni2)<-"index"
oni2$year<-rep(oni$V1, each=12)
oni2$month <- 1:12
oni<-oni2[,c(2,3,1)]

# Get rid of default value
oni$index[oni$index==-99.9]<-NA

head(oni)
tail(oni)
str(oni)



# We can plot it as is...
oni.ts<-ts(oni$index, start=oni$year[1], frequency=12)
plot(oni.ts)
abline(0,0)
# Zoom in
plot(oni.ts, xlim=c(1990,2021))
abline(0,0)

# Filled lines
# Create an x variable that goes left to right then right back to left
# How much do we want to plot?
oni.temp<-oni[oni$year>=1950,]
xx<-oni.temp$year+(oni.temp$month-1)/12
xx <- c(xx, rev(xx))
yy.red <- c(rep(0, nrow(oni.temp)), rev(oni.temp$index))
yy.red[yy.red<0 | is.na(yy.red)]<-0
yy.blue <- c(rep(0, nrow(oni.temp)), rev(oni.temp$index))
yy.blue[yy.blue>0 | is.na(yy.blue)]<-0
# because polygon() is dumb and wants a pre-existing plot
plot(oni.temp$year, oni.temp$index, ylim=c(min(oni.temp$index, na.rm=TRUE), max(oni.temp$index, na.rm=TRUE)), type="n", xlab="", ylab="ONI")
polygon(xx, yy.red, border=NA, col="red")
polygon(xx, yy.blue, border=NA, col="blue")


# Get the Jan-Jun index (the stoplight chart uses the mean of the monthly values)
oniJanJun<-oni[oni$month %in% 1:6,]
oniJanJun<-aggregate(oniJanJun$index, by=list(oniJanJun$year), mean, na.rm=TRUE)
colnames(oniJanJun)<-c("year","index")
plot(oniJanJun, type='b')
abline(0,0)

# Get the seasonal indices
toChange<-which(oni$month == 12)
oni.win<-oni
oni.win$year[toChange]<-oni.win$year[toChange]+1
oni.win<-oni.win[oni.win$month %in% c(12,1,2),]
oni.win<-aggregate(oni.win$index, by=list(oni.win$year), mean, na.rm=TRUE)
colnames(oni.win)<-c("year","oni.win")
#write.csv(oni.win, "oni.win.csv", row.names = FALSE)

oni.spr<-oni[oni$month %in% 3:5,]
oni.spr<-aggregate(oni.spr$index, by=list(oni.spr$year), mean, na.rm=TRUE)
colnames(oni.spr)<-c("year","oni.spr")
#write.csv(oni.spr, "oni.spr.csv", row.names = FALSE)

oni.sum<-oni[oni$month %in% 6:8,]
oni.sum<-aggregate(oni.sum$index, by=list(oni.sum$year), mean, na.rm=TRUE)
colnames(oni.sum)<-c("year","oni.sum")
#write.csv(oni.sum, "oni.sum.csv", row.names = FALSE)

oni.aut<-oni[oni$month %in% 9:11,]
oni.aut<-aggregate(oni.aut$index, by=list(oni.aut$year), mean, na.rm=TRUE)
colnames(oni.aut)<-c("year","oni.aut")
#write.csv(oni.aut, "oni.aut.csv", row.names = FALSE)




#********************************
# Plot the seasonal means
#********************************
cols<-c("blue","darkgreen","red","orange")
plot(oni.win$year, oni.win$oni.win, type='l', lty=1, col=cols[1], xlim=c(1970,2022), ylab="Seasonal oni Index", xlab="")
lines(oni.spr$year, oni.spr$oni.spr, col=cols[2])
lines(oni.sum$year, oni.sum$oni.sum, col=cols[3])
lines(oni.aut$year, oni.aut$oni.aut, col=cols[4])
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
existingData %>% filter(YYYY==2016, MM==1) %>% select(ONI)
# is it the same as what we just downloaded?
oni %>% filter(year==2016, month==1) %>% select (index)

# Just checking:
mergedData<-merge(oni, existingData, by.x = c('year','month'), by.y=c("YYYY","MM"))
plot(mergedData$ONI, mergedData$index)
hist(mergedData$ONI-mergedData$index)


# There's got to be a better way than this than row by row...
for (ii in 1:nrow(oni)) {
  if (oni$year[ii] < min(existingData$YYYY)) next
  sqlQuery(channel, paste('update [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]
         SET [ONI] = ', eval(oni$index[ii]),
         'WHERE [YYYY]=', eval(oni$year[ii]), 'AND [MM]=', eval(oni$month[ii])))
}

