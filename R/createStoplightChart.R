# This is just a test, to see if we can dynamically create a stoplight chart

library('plot.matrix')

rm(list=ls())

stoplight<-read.csv("./data/RED-GREEN-YELLOW_2020.csv")

# get the ranks and colors
stoplightData<-stoplight[,!colnames(stoplight) %in% c("Year", "PC1","PC2")]
ranks<-apply(stoplightData, MARGIN = 2, FUN = rank)
rownames(ranks)<-stoplight$Year
nYears<-nrow(ranks)
# Some indicators are negatively correlated with salmon, so need to be reversed
ranks[,"deepSalinity"]<-nYears+1-(ranks[,"deepSalinity"])
ranks[,"NcopBio"]<-nYears+1-(ranks[,"NcopBio"])
ranks[,"IchthyoBio"]<-nYears+1-(ranks[,"IchthyoBio"])
ranks[,"ChinookCPUE"]<-nYears+1-(ranks[,"ChinookCPUE"])
ranks[,"cohoCPUE"]<-nYears+1-(ranks[,"cohoCPUE"])

# adapt margins such that all labels are visible
par(mar=c(5.1, 9.1, 4.1, 1.1)) 
pp<-plot(t(ranks), col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
         xlab="", ylab="", main="", axis.col=list(side=3, cex.axis=0.8), polygon.cell = list(lwd=4),
         breaks=c(1,nYears/3,nYears*2/3,nYears))
# Same plot, but with ranks added
pp<-plot(t(ranks), col = threeColors, border="white", axis.row = list(side=2, las=1), key=NULL,
         xlab="", ylab="", main="", axis.col=list(side=3, cex.axis=0.8), polygon.cell = list(lwd=4),
         breaks=c(1,nYears/3,nYears*2/3,nYears), text.cell=list(cex=0.75), fmt.cell='%.0f')


