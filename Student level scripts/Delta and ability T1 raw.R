# Difference in post and pre scores stratified by ability levels

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)
View(summarydatanonas)

summarydatanonas$Ability<-as.factor(summarydatanonas$Ability)

#need to make scores numeric so read in correctly
summarydatanonas$DiffprepostA<-as.numeric(summarydatanonas$DiffprepostA)


summarydatanonas$DiffprepostA <- as.numeric(summarydatanonas$DiffprepostA)
summarydatanonas$Ability <- ordered(summarydatanonas$Ability, levels=c("l", "m", "h"))

library(PMCMR) 
install.packages('dunn.test')
library(dunn.test)

kruskal.test(summarydatanonas$DiffprepostA~summarydatanonas$Ability)

# if significant do post hoc, Dunn as unequal numbers of ability subsets.

posthoc <-dunn.test(x=summarydatanonas$DiffprepostA,g=summarydatanonas$Ability, method="bonferroni") 
posthoc


# numbers of each ability subset
cat("No of High = ", nrow(summarydatanonas[summarydatanonas$Ability == "h", ]))
cat("No of Mid = ", nrow(summarydatanonas[summarydatanonas$Ability == "m", ]))
cat("No of Low = ", nrow(summarydatanonas[summarydatanonas$Ability == "l", ]))


pdf("../Rgraphs/Ability and delta.pdf")
boxplot(summarydatanonas$DiffprepostA~summarydatanonas$Ability,ylab="Difference in score",names=c("Low ability (n= 205)","Middle ability (n=431)","Low ability (n=352)"))
mtext(expression("P = 0.545"),side=1,adj=0.98,line=-1.5)
dev.off()

# means,medians and SD
summary(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'h'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'm'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'l'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'h'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'm'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$Ability == 'l'])

