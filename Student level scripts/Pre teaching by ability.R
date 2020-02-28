setwd("~/Documents/Spreadsheets and data/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summary.csv",header=TRUE, stringsAsFactors = FALSE)
View(summarydatanonas)


summarydatanonas$TotalpreA <- as.numeric(summarydatanonas$TotalpreA)
summarydatanonas$Ability <- as.factor(summarydatanonas$Ability)


cat("No of High = ", nrow(summarydatanonas[summarydatanonas$Ability == "l", ]))
cat("No of Mid = ", nrow(summarydatanonas[summarydatanonas$Ability == "m", ]))
cat("No of Low = ", nrow(summarydatanonas[summarydatanonas$Ability == "h", ]))

summarydatanonas$Ability <- ordered (summarydatanonas$Ability, levels=c("l", "m", "h"))

library(PMCMR)
install.packages('dunn.test')
library(dunn.test)

kruskal.test(summarydatanonas$TotalpreA~summarydatanonas$Ability)
posthoc <-dunn.test(x=summarydatanonas$TotalpreA,g=summarydatanonas$Ability, method="bonferroni") 
posthoc

pdf("../Rgraphs/Ability and prescore (fig) .pdf")
boxplot (summarydatanonas$TotalpreA~summarydatanonas$Ability,ylab="Pre-test score",names=c("Low ability (n= 205)","Middle ability (n= 503)","High ability (n= 393)"))
mtext(expression("P = <2.2e-16"),side=1,adj=0.05,line=-25)
dev.off()


