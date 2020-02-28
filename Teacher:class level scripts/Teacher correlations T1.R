setwd("~/Documents/Spreadsheets and data/Thesis/Teachers/")
teacherdata<-read.csv("Teacherdata T1.csv",header=TRUE, stringsAsFactors = FALSE)
teacherdata[teacherdata=="na"]<-NA
View(teacherdata)

#to set working directory go to folder level then read data file in using read has to be done everytime

teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)

# string values are numeric rather than text

qqnorm(teacherdata$DiffPPA[!is.na(teacherdata$DiffPPA)])
# look at all data in file summary data and disregard any with NA in that line of table
# qqplot
shapiro.test(teacherdata$DiffPPA[!is.na(teacherdata$DiffPPA)])
# data not nromally distributed as has significant p value. Null=normal.

hist(teacherdata$DiffPPA,ylab="Frequency",xlab="mean class pre-post difference score",main=" scores are skewed and not normally distributed")
# DiffPPA not normal

teacherdata$YoE = as.numeric(teacherdata$YoE)
hist(teacherdata$YoE,ylab="Frequency",xlab="Years of experience",main="YoE is not normally distributed")
    
# YoE not normal

set1<-(teacherdata$DiffPPA)
set2<-(teacherdata$YoE)
cor.test (set2,set1,method="spearman")
plot(set2,set1)
abline(lm(set1~set2),col="red")
summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)
## to get numbers
(set1)
(set2)
### YoE and mean class PPA not signiificant

# now all repeated for MATE

teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)
teacherdata$MATE = as.numeric(teacherdata$MATE)
qqnorm(teacherdata$MATE[!is.na(teacherdata$MATE)])
shapiro.test(teacherdata$MATE[!is.na(teacherdata$MATE)])
hist(teacherdata$MATE,ylab="Frequency",xlab="MATE score",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$DiffPPA)
set2<-(teacherdata$MATE)
cor.test (set1,set2,method="spearman")
plot(set2,set1,ylab="PPA score" ,xlab=" MATE score",main="There is a significant postive correlation between PPA scores and MATE")
abline(lm(set1~set2),col="red")
summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)
(set1)
(set2)
# is a significant + correlation for for MATE score and PPA difference score.

#  now again for CINS
teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)
teacherdata$CINS = as.numeric(teacherdata$CINS)
qqnorm(teacherdata$CINS[!is.na(teacherdata$CINS)])
shapiro.test(teacherdata$CINS[!is.na(teacherdata$CINS)])
hist(teacherdata$CINS,ylab="Frequency",xlab="CINS score",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$DiffPPA)
set2<-(teacherdata$CINS)
cor.test (set1,set2,method="spearman")
plot(set2,set1,ylab="PPA score" ,xlab=" CINS score",main="There is a postive correlation between PPA scores and CINS but it's not significant")
abline(lm(set1~set2),col="red")
summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)
(set1)
(set2)
### now for NiC[number in class]
teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)
teacherdata$NiC = as.numeric(teacherdata$NiC)
qqnorm(teacherdata$NiC[!is.na(teacherdata$NiC)])
shapiro.test(teacherdata$NiC[!is.na(teacherdata$NiC)])
hist(teacherdata$NiC,ylab="Frequency",xlab="NiC",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$DiffPPA)
set2<-(teacherdata$NiC)
cor.test (set1,set2,method="spearman")
plot(set2,set1,ylab="PPA score" ,xlab=" NiC",main="There is a negative correlation between PPA scores and NiC but it's not significant")
abline(lm(set1~set2),col="red")
summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)
(set1)
(set2)

# Negatove correlation between NIC and PPA mean score for class but not significant.

# now for percieved confidence increase Coninc
teacherdata$DiffPPA = as.numeric(teacherdata$DiffPPA)
teacherdata$Coninc = as.numeric(teacherdata$Coninc)
qqnorm(teacherdata$Coninc[!is.na(teacherdata$Coninc)])
shapiro.test(teacherdata$Coninc[!is.na(teacherdata$Coninc)])
hist(teacherdata$Coninc,ylab="Frequency",xlab="Coninc score",main=" scores are skewed and not normally distributed")
set1<-(teacherdata$DiffPPA)
set2<-(teacherdata$Coninc)
cor.test (set1,set2,method="spearman")
plot(set2,set1,ylab="PPA score" ,xlab=" Coninc score",main="There is a postive correlation between PPA scores and Coninc but it's not significant")
abline(lm(set1~set2),col="red")
summary(set1,na.rm=TRUE)
summary(set2,na.rm=TRUE)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)


# now for boxplots



# remove na values for differencemaking values numeric and box plot whilst keeping Gender as characters
#Order of calling in data important this is my data for difference split it by gender.
teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
View(teacherdatanonas)
teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)

#Highest Qualification
teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)
teacherdatanonas$Highestqual <- as.factor(teacherdatanonas$Highestqual)
teacherdatanonas$Highestqual <- ordered(teacherdatanonas$Highestqual, levels=c("N", "KS4", "KS5", "D", "MSc"))
boxplot(teacherdatanonas$DiffPPA~teacherdatanonas$Highestqual, xlab="Highest teacher qualification in Biology", ylab="Difference in score",main="Difference against highest teacher qualification in Biology",names=c("NONE","KS4", "KS5", "D", "MSc"))

### not normally distributed
hist(subset(teacherdatanonas, Highestqual == "N")$DiffPPA)
hist(subset(teacherdatanonas, Highestqual == "KS4")$DiffPPA)
hist(subset(teacherdatanonas, Highestqual == "KS5")$DiffPPA)
hist(subset(teacherdatanonas, Highestqual == "D")$DiffPPA)
hist(subset(teacherdatanonas, Highestqual == "MSc")$DiffPPA)

# not normally distributed 
# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis +own post hoc
kruskal.test(teacherdatanonas$DiffPPA~teacherdatanonas$Highestqual)
## no significant difference so don't need post hoc
library(PMCMR) 
kruskal.test(teacherdatanonas$DiffPPA~teacherdatanonas$Highestqual)
install.packages('dunn.test')
library(dunn.test)
posthoc <-dunn.test(x=teacherdatanonas$DiffPPA,g=teacherdatanonas$Highestqual, method="bonferroni") 
posthoc



###na.rm removes na
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'N'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'KS4'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'KS5'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'D'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Highestqual == 'MSc'], na.rm = TRUE)


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

teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)
teacherdatanonas$Religion <- as.factor(teacherdatanonas$Religion)
teacherdatanonas$Religion <- ordered(teacherdatanonas$Religion, levels=c("Y", "N", "PNS"))
boxplot(teacherdatanonas$DiffPPA~teacherdatanonas$Religion, xlab="Whether teacher follows a religion", ylab="Difference in score",main="Difference against whether teacher follows a religion",names=c("Yes", "No","Prefer not to say"))

### not normally distributed

hist(subset(teacherdata, Religion == "Y")$DiffPPA)
hist(subset(teacherdata, Religion == "N")$DiffPPA)
hist(subset(teacherdata, Religion == "PNS")$DiffPPA)




# not normally distributed 
# wilcoxon alternative needed to compare 3 categories= Kruskal Wallis +own post hoc

kruskal.test(teacherdatanonas$DiffPPA~teacherdatanonas$Religion)
# no significant difference
posthoc <-posthoc.kruskal.nemenyi.test(x=teacherdata$DiffPPA,g=teacherdata$Religion, method="Chisq") 
posthoc


###na.rm removes na
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'Y'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'N'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'PNS'], na.rm = TRUE)

sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'Y'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'N'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Religion == 'PNS'], na.rm = TRUE)
## counts

cat("No of Y = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "Y" & !is.na(teacherdatanonas$Religion), ]))
cat("No of N = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "N" & !is.na(teacherdatanonas$Religion), ]))
cat("No of PNS = ", nrow(teacherdatanonas[teacherdatanonas$Religion == "PNS" & !is.na(teacherdatanonas$Religion), ]))



##Gender, only 2 factors to compare so Wilcoxon not Kruskal Wallis
teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)
teacherdatanonas$Gender <- as.factor(teacherdatanonas$Gender)
teacherdatanonas$Gender <- ordered(teacherdatanonas$Gender, levels=c("M","F"))
boxplot(teacherdatanonas$DiffPPA~teacherdatanonas$Gender, xlab="Teacher Gender", ylab="Difference in score",main="Difference against teacher gender",names=c("Male","Female"))

### not normally distributed

hist(subset(teacherdata, Gender == "M")$DiffPPA)
hist(subset(teacherdata, Gender == "F")$DiffPPA)
wilcox.test(teacherdata$DiffPPA[teacherdata$Gender=="M"], teacherdata$DiffPPA[teacherdata$Gender=="F"])

summary(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'M'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'F'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'M'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'F'], na.rm = TRUE)
(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'M'])
(teacherdatanonas$DiffPPA[teacherdatanonas$Gender == 'F'])

cat("No of M = ", nrow(teacherdatanonas[teacherdatanonas$Gender == "M" & !is.na(teacherdatanonas$Gender), ]))
cat("No of F = ", nrow(teacherdatanonas[teacherdatanonas$Gender == "F" & !is.na(teacherdatanonas$Gender), ]))


# formal lessons

teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)
teacherdatanonas$FL. <- as.factor(teacherdatanonas$FL.)
teacherdatanonas$FL. <- ordered(teacherdatanonas$FL., levels=c("Y","N"))
boxplot(teacherdatanonas$DiffPPA~teacherdatanonas$FL., xlab="Formal lessons in evolution", ylab="difference in score",main="Difference against foraml lessons in evolution",names=c("Yes","No"))

### not normally distributed

hist(subset(teacherdata, FL. == "Y")$DiffPPA)
hist(subset(teacherdata, FL. == "N")$DiffPPA)
wilcox.test(teacherdata$DiffPPA[teacherdata$FL.=="Y"], teacherdata$DiffPPA[teacherdata$FL.=="N"])
summary(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'Y'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'N'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'Y'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'N'], na.rm = TRUE)
(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'Y'])
(teacherdatanonas$DiffPPA[teacherdatanonas$FL. == 'N'])
cat("No of Y = ", nrow(teacherdatanonas[teacherdatanonas$FL. == "Y" & !is.na(teacherdatanonas$FL.), ]))
cat("No of N = ", nrow(teacherdatanonas[teacherdatanonas$FL. == "N" & !is.na(teacherdatanonas$FL.), ]))

## school type
teacherdatanonas <- teacherdata[!is.na(teacherdata$DiffPPA),]
teacherdatanonas$DiffPPA <- as.numeric(teacherdatanonas$DiffPPA)
teacherdatanonas$Type <- as.factor(teacherdatanonas$Type)
teacherdatanonas$Type <- ordered(teacherdatanonas$Type, levels=c("P","M"))
boxplot(teacherdatanonas$DiffPPA~teacherdatanonas$Type, xlab="Type of school", ylab="difference in score",names=c("Primary","Middle"))

### not normally distributed

hist(subset(teacherdata,Type == "P")$DiffPPA)
hist(subset(teacherdata,Type == "M")$DiffPPA)
wilcox.test(teacherdata$DiffPPA[teacherdata$Type=="P"], teacherdata$DiffPPA[teacherdata$Type=="M"])
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'], na.rm = TRUE)
summary(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'], na.rm = TRUE)
sd(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'], na.rm = TRUE)
(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'P'])
(teacherdatanonas$DiffPPA[teacherdatanonas$Type == 'M'])
