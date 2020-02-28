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

# summary of separate SoW 

summary(residsnona[SoW=='1'])
sd(residsnona[SoW=='1'])
length(residsnona[SoW=='1']) 
summary(residsnona[SoW=='2'])
sd(residsnona[SoW=='2'])
length(residsnona[SoW=='2']) 
summary(residsnona[SoW=='3'])
sd(residsnona[SoW=='3'])
length(residsnona[SoW=='3']) 
summary(residsnona[SoW=='4'])
sd(residsnona[SoW=='4'])
length(residsnona[SoW=='4']) 

# comparison of Trilobites and Pentadactyl Limb in lesson 4
wilcox.test(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'])

summary(residsnona[SoW=='1']+residsnona[SoW=='3'])
sd(residsnona[SoW=='1']+residsnona[SoW=='3'])
summary(residsnona[SoW=='2']+residsnona[SoW=='4'])
sd(residsnona[SoW=='2']+residsnona[SoW=='4'])

library(vioplot)


vioplot(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'],col="gold",
        names=c("1&3(T)", " 2&4(PL)"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 0.095"), side=1, adj=0.98, line=-1.5)


library(effsize)
res<-cliff.delta(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'],conf.level=.95,return.dm=T)
print(res)


# comparison of Hunting Moths and Papaer Moths in lesson 2
wilcox.test(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'])

summary(residsnona[SoW=='1']+residsnona[SoW=='2'])
sd(residsnona[SoW=='1']+residsnona[SoW=='2'])
summary(residsnona[SoW=='3']+residsnona[SoW=='4'])
sd(residsnona[SoW=='3']+residsnona[SoW=='4'])

vioplot(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'],col="gold",
        names=c("1&2(HM)", " 3&4(PM)"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 1.80e-5"), side=1, adj=0.98, line=-1.5)

res<-cliff.delta(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'],return.dm=T,conf.level = .95)
print(res)

# NOW clear dataset to carry out same analyses for tranche 2

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT2.csv",header=TRUE, stringsAsFactors = FALSE)

View(summarydatanonas)
head(summarydatanonas)

#need to make scores numeric so read in correctly
DiffprepostA<-as.numeric(summarydatanonas$DiffprepostA)
TotalpreA<-as.numeric(summarydatanonas$TotalpreA)
SoW<-as.factor(summarydatanonas$SoW)

#  Loses analysis 

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

# comparison of T and PL lesson 4
wilcox.test(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'])

summary(residsnona[SoW=='1']+residsnona[SoW=='3'])
sd(residsnona[SoW=='1']+residsnona[SoW=='3'])
summary(residsnona[SoW=='2']+residsnona[SoW=='4'])
sd(residsnona[SoW=='2']+residsnona[SoW=='4'])


library(vioplot)


vioplot(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'],col="gold",
        names=c("1&3(T)", " 2&4(PL)"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 2 ",side=3)
mtext(expression("P = 0.0435"), side=1, adj=0.98, line=-1.5)


library(effsize)
res<-cliff.delta(residsnona[SoW=='1']+residsnona[SoW=='3'],residsnona[SoW=='2']+residsnona[SoW=='4'],return.dm=T,conf.level = .95)
print(res)

# comparison of HM and PM lesson 2
wilcox.test(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'])

summary(residsnona[SoW=='1']+residsnona[SoW=='2'])
sd(residsnona[SoW=='1']+residsnona[SoW=='2'])
summary(residsnona[SoW=='3']+residsnona[SoW=='4'])
sd(residsnona[SoW=='3']+residsnona[SoW=='4'])

res<-cliff.delta(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'],return.dm=T,conf.level = .95)
print(res)

vioplot(residsnona[SoW=='1']+residsnona[SoW=='2'],residsnona[SoW=='3']+residsnona[SoW=='4'],col="gold",
        names=c("1&2(HM)", " 3&4(PM)"))
title(ylab="LOESS Residuals",xlab="SoW")
mtext("Tranche 2 ",side=3)
mtext(expression("P = 0.15"), side=1, adj=0.98, line=-1.5)

