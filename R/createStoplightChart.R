# This is just a test, to see if we can dynamically create a stoplight chart

library('plot.matrix')

rm(list=ls())

stoplight<-read.csv("./data/RED-GREEN-YELLOW_2020.csv")

# get the ranks and colors
stoplightData<-stoplight[,!colnames(stoplight) %in% c("Year", "PC1","PC2")]
ranks<-apply(stoplightData, MARGIN = 2, FUN = rank)
rownames(ranks)<-stoplight$Year
nYears<-nrow(ranks)
threeColors<-c("lightseagreen","gold","red4")

# Some indicators are negatively correlated with salmon, so need to be reversed
ranks[,"deepSalinity"]<-nYears+1-(ranks[,"deepSalinity"])
ranks[,"NcopBio"]<-nYears+1-(ranks[,"NcopBio"])
ranks[,"IchthyoBio"]<-nYears+1-(ranks[,"IchthyoBio"])
ranks[,"ChinookCPUE"]<-nYears+1-(ranks[,"ChinookCPUE"])
ranks[,"cohoCPUE"]<-nYears+1-(ranks[,"cohoCPUE"])

# We need it transposed
ranks<-t(ranks)

# adapt margins such that all labels are visible
par(mar=c(5.1, 9.1, 4.1, 1.1)) 
pp<-plot(ranks, col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
         xlab="", ylab="", main="", axis.col=list(side=3, cex.axis=0.8), polygon.cell = list(lwd=4),
         breaks=c(1,nYears/3,nYears*2/3,nYears))
# Same plot, but with ranks added
pp<-plot(ranks, col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
         xlab="", ylab="", main="", axis.col=list(side=3, cex.axis=0.8), polygon.cell = list(lwd=4),
         breaks=c(1,nYears/3,nYears*2/3,nYears), text.cell=list(cex=0.75), fmt.cell='%.0f')


#**************************************************************************
# Can we make a layout and separate the three main types of indicators?
#**************************************************************************

indTypes<-c("climate","localPhys","localBio")
indTypeNames<-c("CLIMATE AND\nATMOSPHERIC","LOCAL\nPHYSICAL","LOCAL\nBIOLOGICAL")

# Assign indicators to the types
names(stoplightData)
indTypeAssignments<-c(1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3)

jpeg(filename = "stopight_temp.jpeg",
     width = 800, height = 500, units = "px", pointsize = 12)

plot.new()
# I don't know why but sometimes I have to call this to reset the plotting area
resetPlot<-function() {
  par(fig=c(0,0.12,sepLocs[1]-0.02,0.95), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
  plot(0,type='n',axes=FALSE,ann=FALSE)
}

# Separator locations (between 0 and 1)
sepLocs<-c(0.76, 0.49)

# Plot 1
par(fig=c(0.3,1,sepLocs[1]-0.02,0.95), new=TRUE, mar=c(1,1,1,1), usr=c(0,1,0,1))
plot(ranks[indTypeAssignments==1,], col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
     xlab="", ylab="", main="", axis.col=list(side=3, cex.axis=0.8), polygon.cell = list(lwd=4),
     breaks=c(1,nYears/3,nYears*2/3,nYears))
userCoords<-par("usr")
# Separator
par(fig=c(0.1,1,sepLocs[1], sepLocs[1]+0.02), new=TRUE, mar=c(1,1,1,1))
abline(h=0)
#lines(x = par("usr")[1:2], y = c(0,0))

# Plot 2
par(fig=c(0.3,1,sepLocs[2]-0.02,sepLocs[1]+0.02), new=TRUE, mar=c(1,1,1,1), usr=c(0,1,0,1))
plot(ranks[indTypeAssignments==2,], col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
     xlab="", ylab="", main="", axis.col=NULL, polygon.cell = list(lwd=4),
     breaks=c(1,nYears/3,nYears*2/3,nYears))
# Separator
par(fig=c(0.1,1,sepLocs[2], sepLocs[2]+0.02), new=TRUE, mar=c(1,1,1,1))
abline(h=0)

# Plot 3
par(fig=c(0.3,1,0,sepLocs[2]+0.02), new=TRUE, mar=c(1,1,1,1), usr=c(0,1,0,1))
plot(ranks[indTypeAssignments==3,], col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
     xlab="", ylab="", main="", axis.col=NULL, polygon.cell = list(lwd=4),
     breaks=c(1,nYears/3,nYears*2/3,nYears))


# labels
#  rect -> c(x1,y1,x2,y2)
#  usr  -> c(x1,x2,y1,y2)
#  fig  -> c(x1,x2,y1,y2)
par(fig=c(0.02,0.08,sepLocs[1],0.95), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="lightsteelblue4", border = NA)
par(fig=c(0.02,0.08,sepLocs[1],0.95), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
text(x = (par("usr")[1]+par("usr")[2])/2, y = (par("usr")[3]+par("usr")[4])/2, labels = indTypeNames[1], srt=90, cex=0.8)

resetPlot()
par(fig=c(0.02,0.08,sepLocs[2],sepLocs[1]), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="lightsteelblue3", border = NA)
par(fig=c(0.02,0.08,sepLocs[2],sepLocs[1]), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
text(x = (par("usr")[1]+par("usr")[2])/2, y = (par("usr")[3]+par("usr")[4])/2, labels = indTypeNames[2], srt=90, cex=0.8)

resetPlot()
par(fig=c(0.02,0.08,0.1,sepLocs[2]), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="lightsteelblue2", border = NA)
par(fig=c(0.02,0.08,0.1,sepLocs[2]), new=TRUE, mar=c(0,0,0,0), usr=c(0,1,0,1))
text(x = (par("usr")[1]+par("usr")[2])/2, y = (par("usr")[3]+par("usr")[4])/2, labels = indTypeNames[3], srt=90, cex=0.8)

dev.off()


