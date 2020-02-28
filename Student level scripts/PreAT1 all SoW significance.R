
library(PMCMR)
install.packages('dunn.test')
library(dunn.test)

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydata<-read.csv("Student_summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
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

pdf("../Rgraphs/delta and SoW (a) .pdf")
boxplot(summarydatanonas$DiffprepostA~summarydatanonas$SoW,ylab="Difference in score",main=" Difference in score against teaching package",names=c("1","2","3","4"))
mtext(expression("P = 2.821e-06"),side=1,adj=0.98,line=-1.5)
dev.off()

##### subsetting can call sepaprate bits of data from larger data set for analysis
qqnorm(subset(summarydatanonas, SoW == 1)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 1)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 2)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 3)$DiffprepostA)
hist(subset(summarydatanonas, SoW == 4)$DiffprepostA)
cat("No given SoW 1 = ", nrow(summarydatanonas[summarydatanonas$SoW == "1", ]))
cat("No given SoW 2 = ", nrow(summarydatanonas[summarydatanonas$SoW == "2", ]))
cat("No given SoW 3 = ", nrow(summarydatanonas[summarydatanonas$SoW == "3", ]))
cat("No given SoW 4 = ", nrow(summarydatanonas[summarydatanonas$SoW == "4", ]))
### all are not naormally distributed and not of same size so have to do Dunn 
# post hoc with bonoferoni 

kruskal.test(summarydatanonas$DiffprepostA~summarydatanonas$SoW)
library(PMCMR)
install.packages('dunn.test')
library(dunn.test)


posthoc <-dunn.test(x=summarydatanonas$DiffprepostA,g=summarydatanonas$SoW, method="bonferroni") 
posthoc


# use Tukey if SoW sizes are similar later
posthoc <-posthoc.kruskal.nemenyi.test(x=summarydatanonas$DiffprepostA,g=summarydatanonas$SoW, method="Chisq") 
posthoc

#means and SD 
summary(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])
# counts
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])

sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '1'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '2'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '3'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$SoW == '4'])


