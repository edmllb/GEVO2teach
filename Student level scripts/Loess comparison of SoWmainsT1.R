setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)

View(summarydatanonas)
head(summarydatanonas)

#need to make scores numeric so read in correctly
DiffprepostA<-as.numeric(summarydatanonas$DiffprepostA)
TotalpreA<-as.numeric(summarydatanonas$TotalpreA)
SoW<-as.factor(summarydatanonas$SoW)

#  Loses analysis carried out 

#y=difference in score x=pre teaching score f=SoW 

data.lo.nona <- loess(DiffprepostA~TotalpreA)
plot(DiffprepostA~TotalpreA,pch=19,cex=0.1)
j <- order(TotalpreA)
lines(TotalpreA[j],data.lo.nona$fitted[j],col="red",lwd=3)
summary(data.lo.nona)

# Loess plot smoothes out extremes of data to form a smooth regression line
residsnona <- data.lo.nona$residuals
residsnona$SoW <- ordered(residsnona$SoW, levels=c("1", "2", "3","4"))
# create new dataframe
LoessSoW<-data.frame(residsnona,summarydatanonas$SoW)
colnames(LoessSoW)<-c("residsnona","SoW")
LoessSoW

# comparison of 1 and 3
wilcox.test(residsnona[SoW=='1'],residsnona[SoW=='3'])

library(vioplot)

pdf("../Graphs for inclusion/Loess SoW1cfSow3T1 .pdf")
vioplot(residsnona[SoW=='1'],residsnona[SoW=='3'],col="gold",
        names=c("1", " 3"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 3.03e-9"), side=1, adj=0.98, line=-1.5)
dev.off()

# or box plot

pdf("../Graphs for inclusion/Loess SoW1cfSoW3T2 .pdf")
boxplot(residsnona[SoW=='1'],residsnona[SoW=='3'],ylab="LOESS residual",names=c("SoW 1","SoW 3"),main="Tranche 1",notch=TRUE)
mtext("P = 3.03e-9",side=3,adj=0.98,line=-1.5)
dev.off()

# effect size

library(effsize)
res<-cliff.delta(residsnona[SoW=='1'],residsnona[SoW=='3'],return.dm=T,conf.level = .95)
print(res)

# just to double check that data used ties in with with whole analysis of SoW
summary(residsnona[SoW=='1'])
sd(residsnona[SoW=='1'])
length(residsnona[SoW=='1'])
summary(residsnona[SoW=='3'])
sd(residsnona[SoW=='3'])
length(residsnona[SoW=='3'])

# comparison of 2 and 4
wilcox.test(residsnona[SoW=='2'],residsnona[SoW=='4'])

pdf("../Graphs for inclusion/Loess SoW2cfSoW4T1 .pdf")
vioplot(residsnona[SoW=='2'],residsnona[SoW=='4'],col="gold",
        names=c("2", " 4"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 0.25"), side=1, adj=0.98, line=-1.5)
dev.off()

# comparison of 

pdf("../Graphs for inclusion/Loess SoW2cfSoW4T1 .pdf")
boxplot(residsnona[SoW=='2'],residsnona[SoW=='4'],ylab="LOESS residual",names=c("SoW 2","SoW 4"),main="Tranche 1",notch=TRUE)
mtext("P = 9.86e-3 ",side=3,adj=0.98,line=-1.5)
dev.off()

library(effsize)
res<-cliff.delta(residsnona[SoW=='2'],residsnona[SoW=='4'],return.dm=T,conf.level = .95)
print(res)

# just to double check that data used ties in with with whole analysis of SoW
summary(residsnona[SoW=='2'])
sd(residsnona[SoW=='2'])
length(residsnona[SoW=='2'])
summary(residsnona[SoW=='4'])
sd(residsnona[SoW=='4'])
length(residsnona[SoW=='4'])

# comparison of 1 and 2

wilcox.test(residsnona[SoW=='1'],residsnona[SoW=='2'])

pdf("../Graphs for inclusion/Loess SoW1cfSoW2T1 .pdf")
vioplot(residsnona[SoW=='1'],residsnona[SoW=='2'],col="gold",
        names=c("1", " 2"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 2.92e-6"), side=1, adj=0.98, line=-1.5)
dev.off()

library(effsize)
res<-cliff.delta(residsnona[SoW=='1'],residsnona[SoW=='2'],return.dm=T,conf.level = .95)
print(res)

summary(residsnona[SoW=='2'])
sd(residsnona[SoW=='2'])
length(residsnona[SoW=='2'])
summary(residsnona[SoW=='4'])
sd(residsnona[SoW=='4'])
length(residsnona[SoW=='4'])

# comparison of 3 and 4

wilcox.test(residsnona[SoW=='3'],residsnona[SoW=='4'])

pdf("../Graphs for inclusion/Loess SoW3cfSoW4T1 .pdf")
vioplot(residsnona[SoW=='3'],residsnona[SoW=='4'],col="gold",
        names=c("3", " 4"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 0.01"), side=1, adj=0.98, line=-1.5)
dev.off()

library(effsize)
res<-cliff.delta(residsnona[SoW=='3'],residsnona[SoW=='4'],return.dm=T,conf.level = .95)
print(res)

summary(residsnona[SoW=='3'])
sd(residsnona[SoW=='3'])
length(residsnona[SoW=='3'])
summary(residsnona[SoW=='4'])
sd(residsnona[SoW=='4'])
length(residsnona[SoW=='4'])

# combined P values (Fisher's method)
library("CombinePValue")

# 1 and 3 for both tranches
selfcontained.test(pvalue=c(),weight=NA,p_permu=NA)
# 2 and 3 for both tranches
selfcontained.test(pvalue=c(0.1478,0.00986),weight=NA,p_permu=NA)
