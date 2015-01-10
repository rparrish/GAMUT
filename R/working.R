##
## new graph types
# http://www.ancienteco.com/2013/04/do-same-thing-to-bunch-of-variables.html

require(beeswarm)

load("../../../Documents/AMPA/Projects/GAMUT/data/GAMUT.Rdata")

names(metricData)
varlist <- names(metricData)[3:7]


mydata <- metricData

myPlot<-function(index) {
    plot(mydata[,index],
         main=names(mydata[index]),
         pch=16,
         xlab="BodyMass",
         ylab=names(mydata)[index])
}

par(mfrow=c(3,4))
lapply(3:14,FUN=myPlot)


par(mfrow=c(1,1))

x <- metricData$total_patients

boxplot(x)
beeswarm(x, pch=16, method="center", add=TRUE)

swarmx(x)
