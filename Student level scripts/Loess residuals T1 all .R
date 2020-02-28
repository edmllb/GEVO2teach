# Student level Loess residual analyses

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)


View(summarydatanonas)
head(summarydatanonas)
# set  as a categorical factor 

Ability<-as.factor(summarydatanonas$Ability)
Gender<-as.factor(summarydatanonas$Gender)
SoW<-as.factor(summarydatanonas$SoW)


#need to make scores numeric so read in correctly
DiffprepostA<-as.numeric(summarydatanonas$DiffprepostA)
TotalpreA<-as.numeric(summarydatanonas$TotalpreA)
Difference_in_days<-as.numeric(summarydatanonas$Difference_in_days)
Age <-as.numeric(summarydatanonas$Difference_in_days)
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

#now compare their residuals by age Correlation

# to plot 3 graphs on top of each other
par(mfrow=c(3,1))
#

pdf("../Graphs for inclusion/Loess delta snd age T1 .pdf")
cor.test(residsnona,Age, method="spearman")
plot(Age,residsnona, xlab="Age (in days)",ylab="LOESS Residuals",cex=0.7) 
mtext("Tranche 1",side=3)
mtext(expression("P = 0.1443"),side=1,adj=0.98,line=-1.5)
abline(lm(residsnona~Age))
dev.off()
summary(Age)
sd(Age)

### by ability needs to be numeric comparison
##3=h,2=m,1=1 
# so can do Spearman's rather than a Kruskal 
# ability levels as numbers not letters have to subsistute but onl use with multivariate at bottom.  

summarydatanonas$Abilitynumeric <-summarydatanonas$Ability
letters<-gsub("h",3,summarydatanonas$Abilitynumeric)
letters
letters <- gsub("m", 2, letters)
letters <- gsub("l", 1, letters)
numbers <- letters

numbers <- as.numeric(numbers)
numbers
summarydatanonas$Abilitynumeric <-numbers
head(summarydatanonas)

Abilitynumeric<-as.numeric(summarydatanonas$Abilitynumeric)
(Abilitynumeric)
cor.test(residsnona,Abilitynumeric, method="spearman")

summary(residsnona[Abilitynumeric=='1'])
sd(residsnona[Abilitynumeric=='1'])
summary(residsnona[Abilitynumeric=='2'])
sd(residsnona[Abilitynumeric=='2'])
summary(residsnona[Abilitynumeric=='3'])
sd(residsnona[Abilitynumeric=='3'])

install.packages("vioplot")
library(vioplot)

pdf("../Graphs for inclusion/Loess delta and ability T1 .pdf")
vioplot(residsnona[Abilitynumeric=='1'],residsnona[Abilitynumeric=='2'], residsnona[Abilitynumeric=='3'],col="gold",
        names=c("Low ability (n = 205)","Middle ability (n = 431)","High ability (n= 352)"))
title(ylab="LOESS Residuals", xlab="Relative science ability")

#not sure if needed
# abline(lm(residsnona~Abilitynumeric))

mtext("Tranche 1 ",side=3)
mtext(expression("P = 8.535e-09"), side=1, adj=0.98, line=-1.5)

dev.off()





### for gender

wilcox.test(residsnona~Gender)

library(effsize)
res<-cliff.delta(residsnona,Gender,return.dm=T,conf.level = .95)
print(res)


pdf("../Graphs for inclusion/Loess delta and gender T1  .pdf")
vioplot(residsnona[Gender=='M'],residsnona[Gender=='F'],col="gold",names=c("Male (n = 505)","Female (n = 483)"))
title(ylab="LOESS Residuals", xlab="Gender")
mtext("Tranche 1",side=3)
mtext(expression("P = 0.0111"), side=1, adj=0.98, line=-1.5)
# not sure if needed
# abline(lm(residsnona~Gender))
dev.off()

summary(Gender)
summary(residsnona[Gender=='M'])
summary(residsnona[Gender=='F'])
sd(residsnona[Gender=='M'])
sd(residsnona[Gender=='F'])

# multivariate model


res.1<-
  lm(formula=residsnona~Age+Gender+Abilitynumeric)
summary(res.1)

res.1.1<-
  lm(formula=residsnona~Age*Gender*Abilitynumeric)
summary(res.1.1)

#for SoW



# residsnona$SoW <- ordered(residsnona$SoW, levels=c("1", "2", "3","4"))
# create new dataframe
LoessSoW<-data.frame(residsnona,summarydatanonas$SoW)
colnames(LoessSoW)<-c("residsnona","SoW")
LoessSoW


library(PMCMR)

kruskal.test(LoessSoW$residsnona~LoessSoW$SoW)
# post hoc

install.packages('dunn.test')
library(dunn.test)
posthoc <-dunn.test(x=LoessSoW$residsnona,g=LoessSoW$SoW, method="bonferroni",alpha = 0.05,altp=TRUE) 
posthoc



# effect size
install.packages("rcompanion")
library(rcompanion)
epsilonSquared(x=LoessSoW$residsnona,g=LoessSoW$SoW)






pdf("../Graphs for inclusion/Loess stratified by SoW T1 (fig) .pdf")
vioplot(residsnona[SoW=='1'],residsnona[SoW=='2'], residsnona[SoW=='3'],residsnona[SoW=='4'],col="gold",
        names=c("1", "2","3", '4'))
title(ylab="LOESS Residuals", xlab="SoW")
mtext("Tranche 1 ",side=3)
mtext(expression("P = 3.563e-8"), side=1, adj=0.98, line=-1.5)
dev.off()

summary(residsnona[SoW=='1'])
summary(residsnona[SoW=='2'])
summary(residsnona[SoW=='3'])
summary(residsnona[SoW=='4'])
sd(residsnona[SoW=='1'])
sd(residsnona[SoW=='2'])
sd(residsnona[SoW=='3'])
sd(residsnona[SoW=='4'])

# multivariate model for all variables


res.2<-
  lm(formula=residsnona~Age+Gender+Abilitynumeric+SoW)
summary(res.2)

res.2.1<-
  lm(formula=residsnona~Age*Gender*Abilitynumeric*SoW)
summary(res.2.1)

