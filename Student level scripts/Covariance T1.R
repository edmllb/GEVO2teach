setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydata<-read.csv("Student summaryT1.csv",header=TRUE, stringsAsFactors = TRUE)
summarydata[summarydata=="na"]<-NA
# above line to convert na to NA and not be counted
View(summarydata)
summarydatanonas <- summarydata[!is.na(summarydata$DiffprepostA),]
names(summarydatanonas)
attach(summarydatanonas)

# set SoW as a categorical factor 
SoW<-as.factor(SoW)

#need to make scores numeric so read in correctly
DiffprepostA<-as.numeric(DiffprepostA)
TotalpreA<-as.numeric(TotalpreA)

## plotting and abline

plot(TotalpreA[SoW=="1"],DiffprepostA[SoW=="1"],col="blue",pch=16,cex=0.5)
abline(lm(DiffprepostA[SoW=="1"]~TotalpreA[SoW=="1"]),col="blue")


points(TotalpreA[SoW=="2"],DiffprepostA[SoW=="2"],col="red",pch=16,cex=0.5)
abline(lm(DiffprepostA[SoW=="2"]~TotalpreA[SoW=="2"]),col="red")

points(TotalpreA[SoW=="3"],DiffprepostA[SoW=="3"],col="green",pch=16,cex=0.5)
abline(lm(DiffprepostA[SoW=="3"]~TotalpreA[SoW=="3"]),col="green")

points(TotalpreA[SoW=="4"],DiffprepostA[SoW=="4"],col="green",pch=16,cex=0.5)
abline(lm(DiffprepostA[SoW=="4"]~TotalpreA[SoW=="4"]))

# Ancova (Analysis of covariance) considered and carried out

#Dependent variable: Continuous (scale/interval/ratio),
#Independent variables: Categorical factors (at least 3 unrelated/ independent groups in each), Scale (continuous) covariatesCommon Applications: 
#ANCOVA is similar to traditional ANOVA but is used to detect a difference in means of 3 or more independent groups, whilst controlling for scale covariates. 
#A covariate is not usually part of the main research question but could influence the dependent variable and therefore needs to be controlled for.


lm<-lm(DiffprepostA~TotalpreA+SoW*TotalpreA)
lm
summary(lm)
ac<-lm(DiffprepostA~TotalpreA+SoW)
summary(ac)
anova(lm, ac)

# residual plot
res.1<-residuals(ac)
plot(res.1)

res.2<-residuals(lm)
plot(res.2)

num_points <- length(res.1)
t <- seq(1,num_points)
abline(lm(res.1~t))
shapiro.test(res.1)
hist(res.1)


#  ANCOVA not the best test because data is not normally distributed and the residuals are rubbish

#  Losess residual analysis therefore carried out

#y=difference in score x=pre teaching score f=SoW 


data.lo <- loess(DiffprepostA~TotalpreA)

plot(DiffprepostA~TotalpreA,pch=19,cex=0.1)

j <- order(TotalpreA)

lines(TotalpreA[j],data.lo$fitted[j],col="red",lwd=3)
summary(data.lo)


# Loess plot smoothes out extremes of data to form a smooth regression line
resids <- data.lo$residuals

#now compare their residuals

library(PMCMR) 
kruskal.test(resids~SoW)

# Dunn post hoc test as SoW different sizes
posthoc <-dunn.test(x=resids,g=SoW, method="bonferroni") 
posthoc

# use nemenyi if SoW same size

#posthoc <-posthoc.kruskal.nemenyi.test(x=resids,g=SoW, method="Chisq") 
#posthoc

# Signifcant difference between residuals for the 4 SoW

# and visualise the residuals:
pdf("../Rgraphs/box plot rsiduals and SoW.pdf")  
boxplot(resids~SoW,ylab="Residual", xlab="Teaching package")
dev.off()
#or even better, get the love function violin plot

install.packages("vioplot")

library(vioplot)
pdf("../Rgraphs/vioplot rsiduals and SoW.pdf")  
vioplot(resids[SoW=='1'],resids[SoW=='2'], resids[SoW=='3'],resids[SoW=='4'],col="gold")
dev.off()
