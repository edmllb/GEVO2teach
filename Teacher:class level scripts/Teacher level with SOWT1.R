setwd("~/Documents/Spreadsheets and data/Thesis/Teachers/")
teacherdata<-read.csv("Teacherdata Loess T1.csv",header=TRUE, stringsAsFactors = FALSE)
teacherdata[teacherdata=="na"]<-NA
View(teacherdata)







# YoE as a function of SOW
teacherdata$YoE = as.numeric(teacherdata$YoE)
teacherdata$SOW = as.numeric(teacherdata$SOW)

hist(teacherdata$YoE,ylab="Frequency",xlab="Years of experience",main="YoE is not normally distributed")
    
# YoE not normal

kruskal.test(teacherdata$YoE~teacherdata$SOW)
boxplot(teacherdata$YoE~teacherdata$SOW)


#  MATE
teacherdata$MATE = as.numeric(teacherdata$MATE)
kruskal.test(teacherdata$MATE~teacherdata$SOW)
boxplot(teacherdata$MATE~teacherdata$SOW)

# CINS
teacherdata$CINS = as.numeric(teacherdata$CINS)
kruskal.test(teacherdata$CINS~teacherdata$SOW)
boxplot(teacherdata$CINS~teacherdata$SOW)

# NiC
teacherdata$NiC = as.numeric(teacherdata$NiC)
kruskal.test(teacherdata$CINS~teacherdata$SOW)
boxplot(teacherdata$CINS~teacherdata$SOW)


#  percieved confidence increase in evolution
teacherdata$Coninc = as.numeric(teacherdata$Coninc)
kruskal.test(teacherdata$Coninc~teacherdata$SOW)
boxplot(teacherdata$Coninc~teacherdata$SOW)

library(dunn.test)
posthoc <-dunn.test(x=teacherdata$Coninc,g=teacherdatanonas$SOW, method="bonferroni") 
posthoc
library(rcompanion)
epsilonSquared(x=teacherdata$Coninc,g=teacherdatanonas$SOW)


# remove na values for differencemaking values numeric and box plot whilst keeping Gender as characters
#Order of calling in data important this is my data for difference split it by gender.
teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
head(teacherdatanonas)
teacherdatanonas$residsnona <- as.numeric(teacherdatanonas$residsnona)


# Completion bias

teacherdatanonas$CQ<-as.factor(teacherdatanonas$CQ)
teacherdatanonas$CQ <- ordered(teacherdatanonas$CQ, levels=c("Y","N"))

kruskal.test(teacherdatanonas$CQ~teacherdatanonas$SOW)
boxplot(teacherdatanonas[teacherdatanonas$CQ == "Y"]~teacherdatanonas$SOW)

cat("No Y = ", nrow(teacherdatanonas[teacherdatanonas$CQ == "Y" & !is.na(teacherdatanonas$CQ), ]))
cat("No N = ", nrow(teacherdatanonas[teacherdatanonas$CQ == "N" & !is.na(teacherdatanonas$CQ), ]))


# highest qualification
teacherdatanonas$Highestqual <- as.factor(teacherdatanonas$Highestqual)
teacherdatanonas$Highestqual <- ordered(teacherdatanonas$Highestqual, levels=c("N", "KS4", "KS5", "D", "MSc"))

kruskal.test(teacherdatanonas$Highestqual~teacherdatanonas$SOW)



# Religion

teacherdatanonas$Religion <- as.factor(teacherdatanonas$Religion)
teacherdatanonas$Religion <- ordered(teacherdatanonas$Religion, levels=c("Y", "N", "PNS"))

kruskal.test(teacherdatanonas$Religion~teacherdatanonas$SOW)
epsilonSquared(x=teacherdatanonas$residsnona,g=teacherdatanonas$Religion)


##Gender


teacherdatanonas$Gender <- as.factor(teacherdatanonas$Gender)
teacherdatanonas$Gender <- ordered(teacherdatanonas$Gender, levels=c("M","F"))
kruskal.test(teacherdatanonas$Gender~teacherdatanonas$SOW)

# formal lessons

teacherdatanonas$FL. <- as.factor(teacherdatanonas$FL.)
teacherdatanonas$FL. <- ordered(teacherdatanonas$FL., levels=c("Y","N"))
kruskal.test(teacherdatanonas$FL.~teacherdatanonas$SOW)


#Multivariate model

DiffPPA<-as.numeric(teacherdata$DiffPPA)
PreA<-as.numeric(teacherdata$PreA)

teacherdata$DiffPPA =  as.numeric(teacherdata$DiffPPA)
data.lo.nona <- loess(DiffPPA~PreA)

plot(DiffPPA~PreA,pch=19,cex=0.1)

j <- order(PreA)

lines(PreA[j],data.lo.nona$fitted[j],col="red",lwd=3)
summary(data.lo.nona)

residsnona <- data.lo.nona$residuals

# cant get model to work

res.1<-
  lm(formula=residsnona~YoE+MATE+CINS+Conic+CQ+Religion+HighestQual+NiC+FL.+SoW)
summary(res.1)

res.2<-lm(formula~YoE+SOW)
summary(res.2)
