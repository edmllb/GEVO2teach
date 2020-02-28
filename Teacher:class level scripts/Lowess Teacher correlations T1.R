# Teacher level correlations using Lowess residuals 

setwd("~/Documents/Spreadsheets and data/Thesis/Teachers/")
teacherdata<-read.csv("Teacherdata Loess T1.csv",header=TRUE, stringsAsFactors = FALSE)
teacherdata[teacherdata=="na"]<-NA
View(teacherdata)

DiffPPA<-as.numeric(teacherdata$DiffPPA)
PreA<-as.numeric(teacherdata$PreA)

teacherdata$DiffPPA =  as.numeric(teacherdata$DiffPPA)

# Loess residue 
data.lo.nona <- loess(DiffPPA~PreA)

plot(DiffPPA~PreA,pch=19,cex=0.1)

j <- order(PreA)

lines(PreA[j],data.lo.nona$fitted[j],col="red",lwd=3)
summary(data.lo.nona)

residsnona <- data.lo.nona$residuals

# puts resids into dataset and refers to it in same order all of the time
teacherdata$residsnona <- data.lo.nona$residuals

# test for normality


qqnorm(teacherdata$residsnona)
shapiro.test(teacherdata$residsnona)
# data not nromally distributed as has significant p value. Null=normal.

hist(teacherdata$residsnona,ylab="Frequency",xlab="Loess residuals",main=" scores are skewed and not normally distributed")
# resids not normal

teacherdata$YoE = as.numeric(teacherdata$YoE)
hist(teacherdata$YoE,ylab="Frequency",xlab="Years of experience",main="YoE is not normally distributed")
    
# YoE not normal

set1<-(teacherdata$residsnona)
set2<-(teacherdata$YoE)
cor.test (set2,set1,method="spearman")


pdf("../Graphs for inclusion/LOESSYoET1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="Years of experience",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = -0.06, P = 0.715",side=3,adj=0.98,line=-1.5)
dev.off()


summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)


# YoE and mean class resid not signiificant

# now all repeated for MATE



teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)
teacherdata$MATE = as.numeric(teacherdata$MATE)
qqnorm(teacherdata$MATE[!is.na(teacherdata$MATE)])
shapiro.test(teacherdata$MATE[!is.na(teacherdata$MATE)])
hist(teacherdata$MATE,ylab="Frequency",xlab="MATE score",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$residsnona)
set2<-(teacherdata$MATE)
cor.test (set1,set2,method="spearman")


pdf("../Graphs for inclusion/LOESSMATET1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="MATE score",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = 0.27, P = 0.12",side=3,adj=0.98,line=-1.5)
dev.off()


summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)

# is not significant  for for MATE score and residual score.

#  now again for CINS




teacherdata$CINS = as.numeric(teacherdata$CINS)
qqnorm(teacherdata$CINS[!is.na(teacherdata$CINS)])
shapiro.test(teacherdata$CINS[!is.na(teacherdata$CINS)])
hist(teacherdata$CINS,ylab="Frequency",xlab="CINS score",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$residsnona)
set2<-(teacherdata$CINS)
cor.test (set1,set2,method="spearman")

pdf("../Graphs for inclusion/LOESSCINST1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="CINS score",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = 0.02, P = 0.91",side=3,adj=0.98,line=-2)
dev.off()

summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)




# now for NiC[number in class]



teacherdata$NiC = as.numeric(teacherdata$NiC)
qqnorm(teacherdata$NiC[!is.na(teacherdata$NiC)])
shapiro.test(teacherdata$NiC[!is.na(teacherdata$NiC)])
hist(teacherdata$NiC,ylab="Frequency",xlab="NiC",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$residsnona)
set2<-(teacherdata$NiC)
cor.test (set1,set2,method="spearman")

pdf("../Graphs for inclusion/LOESSNICT1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="Number in class",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = -0.15, P = 0.37",side=3,adj=0.98,line=-2)
dev.off()


summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)


# Negatove correlation between NIC and PPA mean score for class but not significant.

# now for percieved confidence increase in evolution

teacherdata$Diffevo = as.numeric(teacherdata$Diffevo)
qqnorm(teacherdata$Diffevo[!is.na(teacherdata$Diffevo)])
shapiro.test(teacherdata$Diffevo[!is.na(teacherdata$Diffevo)])
hist(teacherdata$Coninc,ylab="Frequency",xlab="Diff evo",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$residsnona)
set2<-(teacherdata$Diffevo)
cor.test (set1,set2,method="spearman")


pdf("../Graphs for inclusion/LOESSDiffevoT1 .pdf")
plot(set2,set1,pch=1,cex=0.7,ylab="LOESS Residuals",xlab="Difference in evolution score",main="Tranche 1")
abline(lm(set1~set2),col="red")
mtext("rho = 0.38, P = 0.034",side=1,adj=0.98,line=-1.5)
dev.off()


summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)




# now for boxplots



# remove na values for differencemaking values numeric and box plot whilst keeping Gender as characters

#Order of calling in data important this is my data for difference split it by gender.
teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
head(teacherdatanonas)
teacherdatanonas$residsnona <- as.numeric(teacherdatanonas$residsnona)


# Completion bias

teacherdatanonas$CQ<-as.factor(teacherdatanonas$CQ)
teacherdatanonas$CQ <- ordered(teacherdatanonas$CQ, levels=c("Y","N"))
qqnorm(teacherdatanonas$CQ)

cat("No Y = ", nrow(teacherdatanonas[teacherdatanonas$CQ == "Y" & !is.na(teacherdatanonas$CQ), ]))
cat("No N = ", nrow(teacherdatanonas[teacherdatanonas$CQ == "N" & !is.na(teacherdatanonas$CQ), ]))



pdf("../Graphs for inclusion/LOESSCompbiasT1 .pdf")
boxplot(teacherdatanonas$residsnona~teacherdatanonas$CQ, xlab="Questionnaire completed", ylab="LOESS Residuals",main="Tranche 1",names=c("Yes (n = 34)","No (n= 6)"))
mtext ("P = 0.18",side=3,adj=0.98,line=-1.5)
dev.off()

wilcox.test(teacherdatanonas$residsnona[teacherdatanonas$CQ=="Y"],teacherdatanonas$residsnona[teacherdatanonas$CQ=="N"])

# effect size
library(effsize)
res<-cliff.delta(teacherdatanonas$residsnona,teacherdatanonas$CQ,return.dm=T)
print(res)

#Highest Qualification


teacherdatanonas$Highestqual <- as.factor(teacherdatanonas$Highestqual)
teacherdatanonas$Highestqual <- ordered(teacherdatanonas$Highestqual, levels=c("N", "KS4", "KS5", "D", "MSc"))

pdf("../Graphs for inclusion/LOESSHQT1 .pdf")
boxplot(teacherdatanonas$residsnona~teacherdatanonas$Highestqual, xlab="Highest teacher qualification in Biology", ylab="LOESS Residuals",main="Tranche 1",names=c("NONE (n = 2)","KS4 (n = 12)", "KS5 (n = 6)", "D (n = 11)", "MSc (n = 3)"))
mtext ("P = 0.28",side=3,adj=0.98,line=-1.5)
dev.off()





### not normally distributed
hist(subset(teacherdatanonas, Highestqual == "N")$residsnona)
hist(subset(teacherdatanonas, Highestqual == "KS4")$residsnona)
hist(subset(teacherdatanonas, Highestqual == "KS5")$residsnona) 
hist(subset(teacherdatanonas, Highestqual == "D")$residsnona)
hist(subset(teacherdatanonas, Highestqual == "MSc")$residsnona)

# not normally distributed 
# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis + own post hoc


kruskal.test(teacherdatanonas$residsnona~teacherdatanonas$Highestqual)
# not sig so don't need post hoc

#library(PMCMR) 
#install.packages('dunn.test')
#library(dunn.test)
#posthoc <-dunn.test(x=teacherdatanonas$DiffPPA,g=teacherdatanonas$Highestqual, method="bonferroni") 
#posthoc

# effect size 

library(rcompanion)
epsilonSquared(x=teacherdatanonas$residsnona,g=teacherdatanonas$Highestqual)

#na.rm removes na

summary(teacherdatanonas$residsnona[teacherdatanonas$Highestqual == 'N'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$Highestqual == 'KS4'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$Highestqual == 'KS5'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$Highestqual == 'D'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$Highestqual == 'MSc'], na.rm = TRUE)


# &! is.na and is not na, ! in R means not
cat("No of N as highest qual = ", nrow(teacherdatanonas[teacherdatanonas$Highestqual == "N" & !is.na(teacherdatanonas$Highestqual), ]))
cat("No of KS4 as highest qual = ", nrow(teacherdatanonas[teacherdatanonas$Highestqual == "KS4" & !is.na(teacherdatanonas$Highestqual), ]))
cat("No of KS5 as highest qual = ", nrow(teacherdatanonas[teacherdatanonas$Highestqual == "KS5" & !is.na(teacherdatanonas$Highestqual), ]))
cat("No of D as highest qual = ", nrow(teacherdatanonas[teacherdatanonas$Highestqual == "D" & !is.na(teacherdatanonas$Highestqual), ]))
cat("No of MSc as highest qual = ", nrow(teacherdatanonas[teacherdatanonas$Highestqual == "MSc" & !is.na(teacherdatanonas$Highestqual) , ]))



sd(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'N'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'KS4'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'KS5'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'D'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'MSc'], na.rm = TRUE)

(subset(teacherdata, Highestqual == "N")$DiffPPA)
(subset(teacherdata, Highestqual == "KS4")$DiffPPA)
(subset(teacherdata, Highestqual == "KS5")$DiffPPA)
(subset(teacherdata, Highestqual == "D")$DiffPPA)
(subset(teacherdata, Highestqual == "MSc")$DiffPPA)

# Religion


teacherdatanonas$Religion <- as.factor(teacherdatanonas$Religion)
teacherdatanonas$Religion <- ordered(teacherdatanonas$Religion, levels=c("Y", "N", "PNS"))

pdf("../Graphs for inclusion/LOESSRelT1 .pdf")
boxplot(teacherdatanonas$residsnona~teacherdatanonas$Religion, xlab="Whether a religion is followed", ylab="LOESS Residual",main="Tranche 1",names=c("Yes (n = 9)", "No (n = 23)","Prefer not to say (n = 2)"))
mtext("P = 0.43",side=3,adj=0.98,line=-1.5)
dev.off()

### not normally distributed

hist(subset(teacherdata, Religion == "Y")$residsnona)
hist(subset(teacherdata, Religion == "N")$residsnona)
hist(subset(teacherdata, Religion == "PNS")$residsnona)




# not normally distributed 
# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis +own post hoc

kruskal.test(teacherdatanonas$residsnona~teacherdatanonas$Religion)

# no significant difference so post hoc not needed 

#posthoc <-posthoc.kruskal.nemenyi.test(x=teacherdata$DiffPPA,g=teacherdata$Religion, method="Chisq") 
#posthoc

# effect size 
epsilonSquared(x=teacherdatanonas$residsnona,g=teacherdatanonas$Religion)


summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'Y'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'N'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'PNS'], na.rm = TRUE)

sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'Y'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'N'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'PNS'], na.rm = TRUE)

# counts

cat("No of Y = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "Y" & !is.na(teacherdatanonas$Religion), ]))
cat("No of N = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "N" & !is.na(teacherdatanonas$Religion), ]))
cat("No of PNS = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "PNS" & !is.na(teacherdatanonas$Religion), ]))



##Gender, only 2 factors to compare so Wilcoxon

teacherdatanonas$Gender <- as.factor(teacherdatanonas$Gender)
teacherdatanonas$Gender <- ordered(teacherdatanonas$Gender, levels=c("M","F"))

pdf("../Graphs for inclusion/LOESSGenT1 .pdf")
boxplot(teacherdatanonas$residsnona~teacherdatanonas$Gender, xlab="Teacher Gender", ylab="LOESS Residuals",main="Tranche 1",names=c("Male (n = 13)","Female (n = 27)"))
mtext("P = 0.23",side=3,adj=0.98,line=-1.5)
dev.off()

# not normally distributed

hist(subset(teacherdata, Gender == "M")$DiffPPA)
hist(subset(teacherdata, Gender == "F")$DiffPPA)

wilcox.test(teacherdata$residsnona[teacherdata$Gender=="M"], teacherdata$residsnona[teacherdata$Gender=="F"])
# or
wilcox.test(teacherdatanonas$residsnona~teacherdatanonas$Gender)

# effect size
library(effsize)
res<-cliff.delta(teacherdatanonas$residsnona,teacherdatanonas$Gender,return.dm=T,conf.level = .95)
print(res)

summary(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'M'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'F'], na.rm = TRUE)
sd(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'M'], na.rm = TRUE)
sd(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'F'], na.rm = TRUE)
(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'M'])
(teacherdatanonas$residsnona[teacherdatanonas$Gender == 'F'])

cat("No of M = ", nrow(teacherdatanonas[teacherdatanonas$Gender == "M" & !is.na(teacherdatanonas$Gender), ]))
cat("No of F = ", nrow(teacherdatanonas[teacherdatanonas$Gender == "F" & !is.na(teacherdatanonas$Gender), ]))


# formal lessons



teacherdatanonas$FL. <- as.factor(teacherdatanonas$FL.)
teacherdatanonas$FL. <- ordered(teacherdatanonas$FL., levels=c("Y","N"))

pdf("../Graphs for inclusion/LOESSFLT1 .pdf")
boxplot(teacherdatanonas$residsnona~teacherdatanonas$FL., xlab="Formal lessons in evolution", ylab="LOESS Residuals",main="Tranche 1",names=c("Yes (n = 16)","No (n = 18)"))
mtext("P = 0.20",side=3,adj=0.98,line=-1.5)
dev.off()

# not normally distributed

hist(subset(teacherdata, FL. == "Y")$DiffPPA)
hist(subset(teacherdata, FL. == "N")$DiffPPA)

wilcox.test(teacherdata$residsnona[teacherdata$FL.=="Y"], teacherdata$residsnona[teacherdata$FL.=="N"])
# or 
wilcox.test(teacherdatanonas$residsnona~teacherdatanonas$FL.)


# effect size
library(effsize)
res<-cliff.delta(teacherdatanonas$residsnona,teacherdatanonas$FL.,return.dm=T)
print(res)

summary(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'Y'], na.rm = TRUE)
summary(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'N'], na.rm = TRUE)
sd(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'Y'], na.rm = TRUE)
sd(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'N'], na.rm = TRUE)
(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'Y'])
(teacherdatanonas$residsnona[teacherdatanonas$FL. == 'N'])

cat("No of Y = ", nrow(teacherdatanonas[teacherdatanonas$FL. == "Y" & !is.na(teacherdatanonas$FL.), ]))
cat("No of N = ", nrow(teacherdatanonas[teacherdatanonas$FL. == "N" & !is.na(teacherdatanonas$FL.), ]))

# correlation between MATE and rel
teacherdatanonas$Religion <- as.factor(teacherdatanonas$Religion)
teacherdatanonas$Religion <- ordered(teacherdatanonas$Religion, levels=c("Y", "N", "PNS"))

teacherdatanonas$MATE <-as.numeric(teacherdata$MATE)

cat("No of Y = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "Y" & !is.na(teacherdatanonas$Religion), ]))
cat("No of N = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "N" & !is.na(teacherdatanonas$Religion), ]))
cat("No of PNS = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "PNS" & !is.na(teacherdatanonas$Religion), ]))


pdf("../Graphs for inclusion/LOESSMATEFL.T1 .pdf")
boxplot(teacherdatanonas$MATE~teacherdatanonas$Religion, xlab="Whether a religion is followed", ylab="MATE score",main="Tranche 1",names=c("Yes (n = 9 )","No (n = 23)","Prefer not to say (n =2)"))
mtext("P = 0.12",side=3,adj=0.98,line=-1.5)
dev.off()

kruskal.test(teacherdatanonas$MATE~teacherdatanonas$Religion)

# effect size
library(rcompanion)
epsilonSquared(x=teacherdatanonas$MATE,g=teacherdatanonas$Religion)

