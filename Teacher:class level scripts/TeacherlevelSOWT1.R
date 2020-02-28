setwd("~/Documents/Spreadsheets and data/Thesis/Teachers/")
teacherdata<-read.csv("Teacherdata Loess T1.csv",header=TRUE, stringsAsFactors = FALSE)
teacherdata[teacherdata=="na"]<-NA
View(teacherdata)
head(teacherdata)


#need to make scores numeric so read in correctly
DiffPPA<-as.numeric(teacherdata$DiffPPA)
PreA<-as.numeric(teacherdata$PreA)
SOW<-as.factor(teacherdata$SOW)



#  Loses analysis carried out 

#y=difference in score x=pre teaching score f=SoW 


data.lo.nona <- loess(DiffPPA~PreA)

plot(DiffPPA~PreA,pch=19,cex=0.1)

j <- order(PreA)

lines(PreA[j],data.lo.nona$fitted[j],col="red",lwd=3)
summary(data.lo.nona)


# Loess plot smoothes out extremes of data to form a smooth regression line
residsnona <- data.lo.nona$residuals

LoessSOW<-data.frame(residsnona,teacherdata$SOW)
colnames(LoessSOW)<-c("residsnona","SOW")
LoessSOW

kruskal.test(LoessSOW$residsnona~LoessSOW$SOW)
library(rcompanion)
library(dunn.test)
library(effsize)

# effect size for Kruskal
epsilonSquared(x=LoessSOW$residsnona,g=LoessSOW$SOW)


# not needed as not significant
posthoc <-dunn.test(x=LoessSOW$residsnona,g=LoessSOW$SOW, method="bonferroni",alpha = 0.05,altp=TRUE) 
posthoc


summary(residsnona[SOW=='1'])
length(residsnona[SOW=='1'])
summary(residsnona[SOW=='2'])
length(residsnona[SOW=='2'])
summary(residsnona[SOW=='3'])
length(residsnona[SOW=='3'])
summary(residsnona[SOW=='4'])
length(residsnona[SOW=='4'])
sd(residsnona[SOW=='1'])
sd(residsnona[SOW=='2'])
sd(residsnona[SOW=='3'])
sd(residsnona[SOW=='4'])



