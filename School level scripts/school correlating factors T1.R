# School level correlations using raw scores

setwd("~/Documents/Spreadsheets and data/Thesis/School level factors")
cordata<- read.csv("Schooldata T1.csv", head=TRUE,stringsAsFactors = FALSE)
View(cordata)
head(cordata)

cordata$Diff = as.numeric(cordata$Diff)

# Tests for normality

qqnorm(cordata$IMD)
shapiro.test(cordata$IMD)

qqnorm(cordata$I)
shapiro.test(cordata$I)

qqnorm(cordata$E)
shapiro.test(cordata$E)

qqnorm(cordata$ES)
shapiro.test(cordata$ES)

qqnorm(cordata$HD)
shapiro.test(cordata$HD)

qqnorm(cordata$C)
shapiro.test(cordata$C)

qqnorm(cordata$BHS)
shapiro.test(cordata$BHS)

qqnorm(cordata$LE)
shapiro.test(cordata$LE)

qqnorm(cordata$IDACI)
shapiro.test(cordata$IDACI)

qqnorm(cordata$Roll)
shapiro.test(cordata$Roll)

cordata$MSE. = as.numeric(cordata$MSE.)
qqnorm(cordata$MSE.[!is.na(cordata$MSe.)])
shapiro.test(cordata$MSE.[!is.na(cordata$MSE.)])

qqnorm(cordata$SEN.EHC)
shapiro.test(cordata$SEN.EHC)

cordata$FSM = as.numeric(cordata$FSM)
qqnorm(cordata$FSM [!is.na(cordata$FSM)])
shapiro.test(cordata$FSM[!is.na(cordata$FSM)])

qqnorm(cordata$TPratio)
shapiro.test(cordata$TPratio)

###box plots
par(mfrow=c(2,2))

boxplot(cordata$Diff~cordata$Type,ylab="Difference in score",xlab="School type")
boxplot(cordata$Diff~cordata$Cat,ylab="Difference in score",xlab="School Category")
boxplot(cordata$Diff~cordata$Rel,ylab="Difference in score",xlab="Religious denomination")
boxplot(cordata$Diff~cordata$Rating,ylab="Difference in score",xlab="Ofsted")   

# correlations, scatter graphs and signifiance
## could add shaped line if ok using lines(lowess(set1,set2), col="blue")

par(mfrow=c(2,3))
set1<-(cordata$Diff)
set2<-(cordata$IMD)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="IMD")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$I)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="IDD")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$E)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="ESTD")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="spearman")

set1<-(cordata$Diff)
set2<-(cordata$ES)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="ES")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$HD)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="HD")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$C)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="C")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$BHS)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="BHS")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="spearman")

set1<-(cordata$Diff)
set2<-(cordata$LE)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="LE")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")



set1<-(cordata$Diff)
set2<-(cordata$IDACI)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="IDACI")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

set1<-(cordata$Diff)
set2<-(cordata$Roll)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="NoR")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="spearman")



cordata$MES. = as.numeric(cordata$MES.)
cordata[cordata=="na"]<-NA
set1<-(cordata$Diff)
set2<-(cordata$MES.)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="MES%")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")



set1<-(cordata$Diff)
set2<-(cordata$SEN.EHC)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="SEN")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")

cordata$FSM = as.numeric(cordata$FSM)
cordata[cordata=="na"]<-NA
set1<-(cordata$Diff)
set2<-(cordata$FSM)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="FSM")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")


set1<-(cordata$Diff)
set2<-(cordata$TPratio)
plot(set1,set2,pch=3,xlab="Difference in Score", ylab="TPratio")
lm(set1 ~ set2)
abline(lm(set2~set1), col="red") 
cor.test(set1,set2,method="pearson")


library(PMCMR)

cordata[cordata=="na"]<-NA
# above line to convert na to NA and not be counted
View(cordata)




# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis +own post hoc
kruskal.test(cordata$Diff~cordata$SA)
posthoc <-posthoc.kruskal.nemenyi.test(x=cordata$Diff,g=cordata$SA, method="Chisq") 
posthoc

mean(cordata$Diff[cordata$SA == 'L'])
mean(cordata$Diff[cordata$SA == 'A'])
mean(cordata$Diff[cordata$SA == 'H'])

sd(cordata$Diff[cordata$SA == 'L'])
sd(cordata$Diff[cordata$SA == 'A'])
sd(cordata$Diff[cordata$SA == 'H'])


## school type

cordata$Diff <- as.numeric(cordata$Diff)
cordata$Type <- as.factor(cordata$Type)
cordata$Type <- ordered(cordata$Type, levels=c("P","M"))
boxplot(cordata$Diff~cordata$Type, xlab="Type of school", ylab="difference in score",names=c("Primary","Middle"))

### not normally distributed

hist(subset(cordata,Type == "P")$Diff)
hist(subset(cordata,Type == "M")$Diff)
wilcox.test(cordata$Diff[cordata$Type=="P"], cordata$Diff[cordata$Type=="M"])
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'], na.rm = TRUE)
(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'])
(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'])


# Religious status
cordata$Diff <- as.numeric(cordata$Diff)
cordata$Rel <- as.factor(cordata$Rel)
cordata$Rel <- ordered(cordata$Rel, levels=c("None","CofE"))
boxplot(cordata$Diff~cordata$Rel, xlab="Type of school", ylab="difference in score",names=c("None","CofE"))

### not normally distributed

hist(subset(cordata,Rel == "None")$Diff)
hist(subset(cordata,Rel == "CofE")$Diff)
wilcox.test(cordata$Diff[cordata$Rel=="None"], cordata$Diff[cordata$Rel=="CofE"])










