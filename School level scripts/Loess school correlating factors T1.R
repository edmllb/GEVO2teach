# School level Loess residual analyses

setwd("~/Documents/Spreadsheets and data/Thesis/School level factors/")
cordata<- read.csv("Schooldata T1.csv", head=TRUE,stringsAsFactors = FALSE)
View(cordata)
head(cordata)

Diff<-as.numeric(cordata$Diff)
PreA<-as.numeric(cordata$PreA)
IMD<-as.numeric(cordata$IMD)
# I<-as.numeric(cordata$I)
# E<-as.numeric(cordata$E)
ES<-as.numeric(cordata$ES)
#C<-as.numeric(cordata$C)
#BHS<-as.numeric(cordata$BHS)
LE<-as.numeric(cordata$LE)
IDACI<-as.numeric(cordata$I)
Roll<-as.numeric(cordata$Roll)
MSE.<-as.numeric(cordata$MSE.)
FSM<-as.numeric(cordata$FSM)
TPratio<-as.numeric(cordata$TPratio)
SEN.EHC<-as.numeric(cordata$SEN.EHC)



data.lo.nona <- loess(Diff~PreA)

plot(Diff~PreA,pch=19,cex=0.1)

j <- order(PreA)

lines(PreA[j],data.lo.nona$fitted[j],col="red",lwd=3)
summary(data.lo.nona)

residsnona <- data.lo.nona$residuals

# puts resids into dataset and refers to it in same order all of the time
cordata$residsnona <- data.lo.nona$residuals

# testing for normality and them correlating with residuals
qqnorm(cordata$residsnona)
shapiro.test(cordata$IMD)
shapiro.test(cordata$residsnona)

# do all as spearmans so that they can be compared
cor.test(cordata$IMD,cordata$residsnona, method="spearman")



# setting up a for loop with a conditional clause for all factors
CN <-c("IMD","I","E","ES","HD", "C","BHS","LE", "IDACI","Roll", "SEN.EHC","TPratio")
for (Column in CN){
  print(Column)
  shapiro.test(cordata[[Column]])
  p<-shapiro.test(cordata[[Column]])$p.value
  if (p<0.05){
    method="spearman"
    
  }else{
    method="pearson"
  }
  print(cor.test(cordata[[Column]],cordata$residsnona, method=method))
  
}



# have to remove Nas before testing
cordata$MSE. = as.numeric(cordata$MSE.)
qqnorm(cordata$MSE.[!is.na(cordata$MSE.)])
shapiro.test(cordata$MSE.[!is.na(cordata$MSE.)])
cor.test(cordata$MSE.,cordata$residsnona, method="pearson")


cordata$FSM = as.numeric(cordata$FSM)
qqnorm(cordata$FSM [!is.na(cordata$FSM)])
shapiro.test(cordata$FSM[!is.na(cordata$FSM)])
cor.test(cordata$FSM,cordata$residsnona, method="pearson")



###box plots these need to be corrected for cordata$residsnonas if necessary
par(mfrow=c(2,2))
boxplot(cordata$residsnona~cordata$Type,ylab="LOESS",xlab="School type")
boxplot(cordata$residsnona~cordata$Cat,ylab="LOESS",xlab="School Category")
boxplot(cordata$residsnona~cordata$Rel,ylab="LOESS",xlab="Religious denomination")
boxplot(cordata$residsnona~cordata$Rating,ylab="LOESS",xlab="Ofsted")   

# correlations, scatter graphs and signifiance
## could add shaped line if ok using lines(lowess(set1,set2), col="blue")

par(mfrow=c(2,2))

set1<-(cordata$residsnona)
set2<-(cordata$IMD)



cor.test(set1,set2,method="spearman")

pdf("../Graphs for inclusion/LOESSIMDT1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="IMD",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = 0.17, P = 0.51",side=3,adj=0.98,line=-1.5)
dev.off()

# not needed
set1<-(cordata$I)
set2<-(cordata$residsnona)
cor.test(set1,set2,method="pearson")


set1<-(cordata$residsnona)
set2<-(cordata$E)

pdf("../Graphs for inclusion/LOESSET1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="E",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("cor = -0.14, P = 0.60",side=3,adj=0.98,line=-1.5)
dev.off()
cor.test(set1,set2,method="pearson")

set1<-(cordata$residsnona)
set2ES<-(cordata$ES)

pdf("../Graphs for inclusion/LOESSEST1 .pdf")
plot(set2ES,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="ESI",main="Tranche 1")
abline(lm(set1~set2ES),col="red")
mtext("rho = -0.02, P = 0.94",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2ES,method="spearman")

set1<-(cordata$residsnona)
set2LE<-(cordata$LE)

pdf("../Graphs for inclusion/LOESSLET1 .pdf")
plot(set2LE,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="LEI",main="Tranche 1")
abline(lm(set1~set2LE),col="red")
mtext("rho = 0.39, P = 0.13",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2LE,method="spearman")

set1<-(cordata$residsnona)
set2ID<-(cordata$IDACI)

pdf("../Graphs for inclusion/LOESSIDT1 .pdf")
plot(set2ID,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="IDACI",main="Tranche 1")
abline(lm(set1~set2ID),col="red")
mtext("rho = 0.14, P = 0.60",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2ID,method="spearman")

set1<-(cordata$residsnona)
set2Roll<-(cordata$Roll)

pdf("../Graphs for inclusion/LOESSRollT1 .pdf")
plot(set2Roll,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="Number on school roll",main="Tranche 1")
abline(lm(set1~set2Roll),col="red")
mtext("rho = -0.03, P = 0.93",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2Roll,method="spearman")



cordata$MSE. = as.numeric(cordata$MSE.)
cordata[cordata=="na"]<-NA

set1<-(cordata$residsnona)
set2MSE<-(cordata$MSE.)

pdf("../Graphs for inclusion/LOESSMSET1 .pdf")
plot(set2MSE,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="MSE",main="Tranche 1")
abline(lm(set1~set2MSE),col="red")
mtext("rho = 0.15, P = 0.58",side=3,adj=0.98,line=-1.5)
dev.off()
cor.test(set1,set2MSE,method="spearman")



set1<-(cordata$residsnona)
set2SEN<-(cordata$SEN.EHC)
pdf("../Graphs for inclusion/LOESSSENT1 .pdf")
plot(set2SEN,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="SEN",main="Tranche 1")
abline(lm(set1~set2SEN),col="red")
mtext("rho = 0.41, P = 0.10",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2SEN,method="spearman")

cordata$FSM = as.numeric(cordata$FSM)
cordata[cordata=="na"]<-NA
set1<-(cordata$residsnona)
set2FSM<-(cordata$FSM)

pdf("../Graphs for inclusion/LOESSFSMT1 .pdf")
plot(set2FSM,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="Percentage of FSM",main="Tranche 1")
abline(lm(set1~set2FSM),col="red")
mtext("rho = -0.46, P = 0.08",side=3,adj=0.98,line=-1.5)
dev.off()

cor.test(set1,set2FSM,method="spearman")


set1<-(cordata$residsnona)
set2TP<-(cordata$TPratio)

pdf("../Graphs for inclusion/LOESSTPT1 .pdf")
plot(set2TP,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="TP ratio",main="Tranche 1")
abline(lm(set1~set2TP),col="red")
mtext("rho = 0.26, P = 0.32",side=3,adj=0.98,line=-1.5)
dev.off()
cor.test(set1,set2TP,method="spearman")


## school type


cordata$Type <- as.factor(cordata$Type)
cordata$Type <- ordered(cordata$Type, levels=c("P","M"))
hist(subset(cordata,Type == "P")$residsnona)
hist(subset(cordata,Type == "M")$residsnona)
# not normal
wilcox.test(cordata$residsnona[cordata$Type=="P"], cordata$residsnona[cordata$Type=="M"])
res<-cliff.delta(cordata$residsnona[cordata$Type=="P"],cordata$residsnona[cordata$Type=="M"],return.dm=T)
print(res)

cat("No P = ", nrow(cordata[cordata$Type == "P" & !is.na(cordata$Type), ]))
cat("No M = ", nrow(cordata[cordata$Type == "M" & !is.na(cordata$Type), ]))

pdf("../Graphs for inclusion/LOESSTypeT1 .pdf")
boxplot(cordata$residsnona~cordata$Type, xlab="Type of school", ylab="LOESS Residuals",names=c("Primary (n = 9)","Middle (n = 8)"),main="Tranche 1")
mtext("P = 0.24",side=3,adj=0.98,line=-1.5)
dev.off()

# Religious status

cordata$Rel <- as.factor(cordata$Rel)
cordata$Rel <- ordered(cordata$Rel, levels=c("None","CofE"))


hist(subset(cordata,Rel == "None")$residsnona)
hist(subset(cordata,Rel == "CofE")$residsnona)
# not normally ditributed
wilcox.test(cordata$residsnona[cordata$Rel=="None"], cordata$residsnona[cordata$Rel=="CofE"])
res<-cliff.delta(cordata$residsnona[cordata$Rel=="None"],cordata$residsnona[cordata$Rel=="CofE"],return.dm=T)
print(res)

cat("No None = ", nrow(cordata[cordata$Rel == "None" & !is.na(cordata$Rel), ]))
cat("No cofE = ", nrow(cordata[cordata$Rel == "CofE" & !is.na(cordata$Rel), ]))


pdf("../Graphs for inclusion/LOESSRelT1 .pdf")
boxplot(cordata$residsnona~cordata$Rel, xlab="Religious affiliation of school", ylab="LOESS Residuals",names=c("None (n = 8)","CofE (n = 9)"),main="Tranche 1")
mtext("P = 0.28",side=3,adj=0.98,line=-1.5)
dev.off()


# school category

library(PMCMR)



cordata$residsnona <- as.numeric(cordata$residsnona)
cordata$Cat <- as.factor(cordata$Cat)
cordata$Cat <- ordered(cordata$Cat, levels=c("A","MS","I"))

cat("No A = ", nrow(cordata[cordata$Cat == "A" & !is.na(cordata$Cat), ]))
cat("No MS = ", nrow(cordata[cordata$Cat == "MS" & !is.na(cordata$Cat), ]))
cat("No I = ", nrow(cordata[cordata$Cat == "I" & !is.na(cordata$Cat), ]))

pdf("../Graphs for inclusion/LOESSCatT1 .pdf")
boxplot(cordata$residsnona~cordata$Cat, xlab="Category of school", ylab="LOESS Residuals",names=c("Academy (n = 5)","Maintained (n = 11)","Independent (n = 1)"),main="Tranche 1")
mtext("P = 0.45",side=3,adj=0.98,line=-1.5)
dev.off()

# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis +own post hoc
kruskal.test(cordata$residsnona~cordata$Cat)
library(rcompanion)
epsilonSquared(x=cordata$residsnona,g=cordata$Cat)

# not needed as not significant
posthoc <-posthoc.kruskal.nemenyi.test(x=cordata$Diff,g=cordata$SA, method="Chisq") 
posthoc

mean(cordata$residsnona[cordata$Cat == 'L'])
mean(cordata$residsnona[cordata$Cat == 'A'])
mean(cordata$residsnona[cordata$Cat == 'H'])

sd(cordata$residsnona[cordata$Cat == 'L'])
sd(cordata$residsnona[cordata$Cat == 'A'])
sd(cordata$residsnona[cordata$Cat == 'H'])


# ofsted rating
Rating<-as.numeric(cordata$Rating)
cordata$Rating <- ordered(cordata$Rating, levels=c("2","1"))

cat("No 2 = ", nrow(cordata[cordata$Rating == "2" & !is.na(cordata$Rating), ]))
cat("No 1 = ", nrow(cordata[cordata$Rating == "1" & !is.na(cordata$Rating), ]))

library(effsize)
wilcox.test(cordata$residsnona[cordata$Rating=="2"], cordata$residsnona[cordata$Rating=="1"])
res<-cliff.delta(cordata$residsnona[cordata$Rating=="2"],cordata$residsnona[cordata$Rating=="1"],return.dm=T)
print(res)

pdf("../Graphs for inclusion/LOESSOfstedT1 .pdf")
boxplot(cordata$residsnona~cordata$Rating, xlab="Ofsted rating", ylab="LOESS Residuals",names=c("2 (n = 15)","1 (n = 2)"),main="Tranche 1")
mtext("P = 0.62",side=3,adj=0.98,line=-1.5)
dev.off()


# EAL
EAL<-as.numeric(cordata$EAL)
shapiro.test(cordata$EAL)
set1<-(cordata$residsnona)
set2EAL<-(cordata$EAL)

pdf("../Graphs for inclusion/LOESSEALT1 .pdf")
plot(set2EAL,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="EAL",main="Tranche 1")
abline(lm(set1~set2EAL),col="red")
mtext("rho = 0.08, P = 0.77",side=3,adj=0.98,line=-1.5)
dev.off()
cor.test(set1,set2EAL,method="spearman")









