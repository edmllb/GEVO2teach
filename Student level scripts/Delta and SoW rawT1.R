library(PMCMR)

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydata<-read.csv("Student summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
summarydata[summarydata=="na"]<-NA
# above line to convert na to NA and not be counted
View(summarydata)

#to set working directory go to folder level then read data file in using read has to be done everytime

summarydata$TotalpreA = as.numeric(summarydata$TotalpreA)



# remove na values for differencemaking values numeric and box plot whilst keeping Gender as characters
#Order of calling in data important this is my data for difference split it by gender.
summarydatanonas <- summarydata[!is.na(summarydata$DiffprepostA),]
summarydatanonas$DiffprepostA <- as.numeric(summarydatanonas$DiffprepostA)
summarydatanonas$SoW <- ordered(summarydatanonas$SoW, levels=c("1", "2", "3","4"))
boxplot(summarydatanonas$DiffprepostA~summarydatanonas$SoW,ylab="Difference in Score",xlab="SoW",names=c("1 (n=171)","2 (n=314)","3 (n=241)","4 (n=262)"))

##### subsetting can call sepaprate bits of data from larger data set for analysis
qqnorm(subset(summarydatanonas, SoW == 1)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 1)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 2)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 3)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 4)$DiffprepostA)

### to calculate mean % and sds for each SoW
sd(subset(summarydatanonas, SoW == 1)$DiffprepostA/15*100)
summary(subset(summarydatanonas, SoW == 1)$DiffprepostA/15*100)
sd(subset(summarydatanonas, SoW == 2)$DiffprepostA/15*100)
summary(subset(summarydatanonas, SoW == 2)$DiffprepostA/15*100)
sd(subset(summarydatanonas, SoW == 3)$DiffprepostA/15*100)
summary(subset(summarydatanonas, SoW == 3)$DiffprepostA/15*100)
sd(subset(summarydatanonas, SoW == 4)$DiffprepostA/15*100)
summary(subset(summarydatanonas, SoW == 4)$DiffprepostA/15*100)





### all are not naormally distributed

kruskal.test(summarydatanonas$DiffprepostA~summarydatanonas$SoW)
#need Dunn test here 
posthoc <-dunn.test(x=summarydatanonas$TotalpreA,g=summarydatanonas$SoW, method="bonferroni") 
posthoc




#means and SD 
mean(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
mean(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
mean(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
mean(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])


sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])


## for numbers
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])


