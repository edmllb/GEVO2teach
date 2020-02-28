library(PMCMR)
install.packages('dunn.test')
library(dunn.test)

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydata<-read.csv("Student summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
summarydata[summarydata=="na"]<-NA
# above line to convert na to NA and not be counted
View(summarydata)

#to set working directory go to folder level then read data file in using read has to be done everytime

summarydata$TotalpreA = as.numeric(summarydata$TotalpreA)



# remove na values for differencemaking values numeric and box plot whilst keeping Gender as characters
#Order of calling in data important this is my data for difference split it by SOW.
summarydatanonas <- summarydata[!is.na(summarydata$TotalpreA),]
summarydatanonas$TotalpreA <- as.numeric(summarydatanonas$TotalpreA)
summarydatanonas$SoW <- ordered(summarydatanonas$SoW, levels=c("1", "2", "3","4"))

cat("No given SoW 1 = ", nrow(summarydatanonas[summarydatanonas$SoW == "1", ]))
cat("No given SoW 2 = ", nrow(summarydatanonas[summarydatanonas$SoW == "2", ]))
cat("No given SoW 3 = ", nrow(summarydatanonas[summarydatanonas$SoW == "3", ]))
cat("No given SoW 4 = ", nrow(summarydatanonas[summarydatanonas$SoW == "4", ]))


boxplot(summarydatanonas$TotalpreA~summarydatanonas$SoW,ylab="Difference in score",main="preAT1 matched for post against SoW",names=c("1 (n= 171)","2 (n= 314)","3 (n= 241)","4 (n=262)"))


##### subsetting can call sepaprate bits of data from larger data set for analysis

hist(subset(summarydatanonas, SoW == 1)$TotalpreA)
hist(subset(summarydatanonas, SoW == 2)$TotalpreA)
hist(subset(summarydatanonas, SoW == 3)$TotalpreA)
hist(subset(summarydatanonas, SoW == 4)$TotalpreA)

### all are not naormally distributed

kruskal.test(summarydatanonas$TotalpreA,summarydatanonas$SoW)
library(rcompanion)
epsilonSquared(x=summarydatanonas$TotalpreA,summarydatanonas$SoW)


#run post hoc as significant
posthoc <-dunn.test(x=summarydatanonas$TotalpreA,g=summarydatanonas$SoW, method="bonferroni") 

posthoc


install.packages('PMCMR')
library(PMCMR)


#code for nemenyi test if equal numbers
posthoc <-posthoc.kruskal.nemenyi.test(x=summarydatanonas$TotalpreA,g=summarydatanonas$SoW, dist="Chisq") 
posthoc

#means and SD 
mean(summarydatanonas$TotalpreA[summarydatanonas$SoW == '1'])
mean(summarydatanonas$TotalpreA[summarydatanonas$SoW == '2'])
mean(summarydatanonas$TotalpreA[summarydatanonas$SoW == '3'])
mean(summarydatanonas$TotalpreA[summarydatanonas$SoW == '4'])


sd(summarydatanonas$TotalpreA[summarydatanonas$SoW == '1'])
sd(summarydatanonas$TotalpreA[summarydatanonas$SoW == '2'])
sd(summarydatanonas$TotalpreA[summarydatanonas$SoW == '3'])
sd(summarydatanonas$TotalpreA[summarydatanonas$SoW == '4'])

