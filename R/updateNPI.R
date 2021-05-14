#  This script will download the entire NPI time series,
#    create monthly and seasonal indices,
#    and load them into the database

library(RODBC)
library(dplyr)
library(tidyr)

rm(list=ls())



##### NPI ####

#**********************************************
#    NPI (measures Aleutian Low)
#**********************************************

npi <- readLines("https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt")

# I'm keeping any line with 2 items in it...  Luckily that works.
counts <- count.fields(textConnection(npi), blank.lines.skip = FALSE)
npi<-read.table(text=npi[counts == 2], header=FALSE, stringsAsFactors = FALSE)
npi$year<-as.numeric(substr(npi[,1],1,4))
npi$month<-as.numeric(substr(npi[,1],5,6))
npi$index<-npi[,2]
npi<-npi[,c(3:5)]
# Get rid of default value
npi$index[npi$index==-999]<-NA

head(npi)
tail(npi, 20)
str(npi)

npi.mean<-mean(npi$index, na.rm=TRUE)

# We can ployt it as is...
npi.ts<-ts(npi$index, start=npi$year[1], frequency=12)
plot(npi.ts)
abline(npi.mean,0)
# Zoom in
plot(npi.ts, xlim=c(2000,2021))
abline(npi.mean,0)

# Get the seasonal indices
toChange<-which(npi$month == 12)
npi.win<-npi
npi.win$year[toChange]<-npi.win$year[toChange]+1
npi.win<-npi.win[npi.win$month %in% c(12,1,2),]
npi.win<-aggregate(npi.win$index, by=list(npi.win$year), mean, na.rm=TRUE)
colnames(npi.win)<-c("year","npi.win")
#npi.win<-npi.win[ npi.win$year<=2017,]
#write.csv(npi.win, "npi.win.csv", row.names = FALSE)

npi.spr<-npi[npi$month %in% 3:5,]
npi.spr<-aggregate(npi.spr$index, by=list(npi.spr$year), mean, na.rm=TRUE)
colnames(npi.spr)<-c("year","npi.spr")
#write.csv(npi.spr, "npi.spr.csv", row.names = FALSE)

npi.sum<-npi[npi$month %in% 6:8,]
npi.sum<-aggregate(npi.sum$index, by=list(npi.sum$year), mean, na.rm=TRUE)
colnames(npi.sum)<-c("year","npi.sum")
#write.csv(npi.sum, "npi.sum.csv", row.names = FALSE)

npi.aut<-npi[npi$month %in% 9:11,]
npi.aut<-aggregate(npi.aut$index, by=list(npi.aut$year), mean, na.rm=TRUE)
colnames(npi.aut)<-c("year","npi.aut")
#write.csv(npi.aut, "npi.aut.csv", row.names = FALSE)

# Filled lines
# How much do we want to plot?
npi.temp<-npi[npi$year>=1950,]
# Create an x variable that goes left to right then right back to left
xx<-npi.temp$year+(npi.temp$month-1)/12
xx <- c(xx, rev(xx))
yy.red <- c(rep(npi.mean, nrow(npi.temp)), rev(npi.temp$index))
yy.red[yy.red<npi.mean | is.na(yy.red)]<-npi.mean
yy.blue <- c(rep(npi.mean, nrow(npi.temp)), rev(npi.temp$index))
yy.blue[yy.blue>npi.mean | is.na(yy.blue)]<-npi.mean
# because polygon() is dumb and wants a pre-existing plot
plot(npi.temp$year, npi.temp$index, ylim=c(min(npi.temp$index, na.rm=TRUE), max(npi.temp$index, na.rm=TRUE)), type="n", xlab="", ylab="NPI")
polygon(xx, yy.red, border=NA, col="red")
polygon(xx, yy.blue, border=NA, col="blue")
for (hh in unique(npi.temp$year)) abline(v=hh)

width<-12
npi.roll<-rollapply(zoo(npi$index), width = width, FUN = mean, na.rm=TRUE)
npi.roll<-data.frame(index=npi.roll)
npi.roll$year<-npi$year[ceiling(width/2):round(nrow(npi)-round(width/2))]
npi.roll$month<-npi$month[ceiling(width/2):round(nrow(npi)-round(width/2))]
#plot(npi$index[-1*c(1:floor(width/2),(nrow(npi)-ceiling(width/2)):nrow(npi))], npi.roll$index)

# Filled lines
# How much do we want to plot?
npi.temp<-npi.roll[npi.roll$year>=1970,]
# Create an x variable that goes left to right then right back to left
xx<-npi.temp$year+(npi.temp$month-1)/12
xx <- c(xx, rev(xx))
yy.red <- c(rep(npi.mean, nrow(npi.temp)), rev(npi.temp$index))
yy.red[yy.red<npi.mean | is.na(yy.red)]<-npi.mean
yy.blue <- c(rep(npi.mean, nrow(npi.temp)), rev(npi.temp$index))
yy.blue[yy.blue>npi.mean | is.na(yy.blue)]<-npi.mean
# because polygon() is dumb and wants a pre-existing plot
plot(npi.temp$year, npi.temp$index, ylim=c(min(npi.temp$index, na.rm=TRUE), max(npi.temp$index, na.rm=TRUE)), type="n", xlab="", ylab="NPI")
polygon(xx, yy.red, border=NA, col="red")
polygon(xx, yy.blue, border=NA, col="blue")
abline(v=1972.5); abline(v=1973.5)
abline(v=1982.5); abline(v=1983.5)
abline(v=1997.5); abline(v=1998.5)
abline(v=2002.5); abline(v=2003.5)
abline(v=2009.5); abline(v=2010.5)
abline(v=2015.5); abline(v=2016.5)



#********************************
# Plot the seasonal means
#********************************
cols<-c("blue","darkgreen","red","orange")
plot(npi.win$year, npi.win$npi.win, type='l', lty=1, col=cols[1], xlim=c(1970,2022), ylim=c(1000,1020), ylab="Seasonal npi Index", xlab="")
lines(npi.spr$year, npi.spr$npi.spr, col=cols[2])
lines(npi.sum$year, npi.sum$npi.sum, col=cols[3])
lines(npi.aut$year, npi.aut$npi.aut, col=cols[4])
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
existingData %>% filter(YYYY==2016, MM==1) %>% select(NPI)
# is it the same as what we just downloaded?
npi %>% filter(year==2016, month==1) %>% select (index)

# Just checking:
mergedData<-merge(npi, existingData, by.x = c('year','month'), by.y=c("YYYY","MM"))
plot(mergedData$NPI, mergedData$index)
hist(mergedData$NPI-mergedData$index)


# There's got to be a better way than this than row by row...
for (ii in 1:nrow(npi)) {
  if (npi$year[ii] < min(existingData$YYYY)) next
  sqlQuery(channel, paste('update [NWFSC_OCEAN].[ncc].[Basin_Scale_Indices]
         SET [NPI] = ', eval(npi$index[ii]),
         'WHERE [YYYY]=', eval(npi$year[ii]), 'AND [MM]=', eval(npi$month[ii])))
}

