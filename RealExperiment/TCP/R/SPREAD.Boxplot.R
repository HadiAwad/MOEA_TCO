postscript("SPREAD.Boxplot.eps", horizontal=FALSE, onefile=FALSE, height=8, width=12, pointsize=10)
resultDirectory<-"RealExperiment/TCP/data"
qIndicator <- function(indicator, problem)
{
fileNSGAII<-paste(resultDirectory, "NSGAII", sep="/")
fileNSGAII<-paste(fileNSGAII, problem, sep="/")
fileNSGAII<-paste(fileNSGAII, indicator, sep="/")
NSGAII<-scan(fileNSGAII)

fileMOCell<-paste(resultDirectory, "MOCell", sep="/")
fileMOCell<-paste(fileMOCell, problem, sep="/")
fileMOCell<-paste(fileMOCell, indicator, sep="/")
MOCell<-scan(fileMOCell)

fileIBEA<-paste(resultDirectory, "IBEA", sep="/")
fileIBEA<-paste(fileIBEA, problem, sep="/")
fileIBEA<-paste(fileIBEA, indicator, sep="/")
IBEA<-scan(fileIBEA)

algs<-c("NSGAII","MOCell","IBEA")
boxplot(NSGAII,MOCell,IBEA,names=algs, notch = FALSE)
titulo <-paste(indicator, problem, sep=":")
title(main=titulo)
}
par(mfrow=c(3,3))
indicator<-"SPREAD"
qIndicator(indicator, "bash")
qIndicator(indicator, "flex")
qIndicator(indicator, "grep")
qIndicator(indicator, "printtokens")
qIndicator(indicator, "printtokens2")
qIndicator(indicator, "sed")
