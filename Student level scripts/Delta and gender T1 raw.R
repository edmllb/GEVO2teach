setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)

View(summarydatanonas)



summarydatanonas$DiffprepostA <- as.numeric(summarydatanonas$DiffprepostA)

pdf("../Rgraphs/Gender and delta (fig4) .pdf")
boxplot(summarydatanonas$DiffprepostA~summarydatanonas$Gender,ylab="Difference in score",names=c("Female (n= 483)","Male (n=505)"))
wilcox.test(summarydatanonas$DiffprepostA~summarydatanonas$Gender)
mtext(expression("P = 0.143"),side=1,adj=0.98,line=-1.5)
dev.off()


#means and SD min and max and counts (can use cat or miss off function at beginning of line)


cat("No of Females = ", nrow(summarydatanonas[summarydatanonas$Gender == "M", ]))
cat("No of Males = ", nrow(summarydatanonas[summarydatanonas$Gender == "F", ]))
summary(summarydatanonas$DiffprepostA[summarydatanonas$Gender == 'M'])
summary(summarydatanonas$DiffprepostA[summarydatanonas$Gender == 'F'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$Gender == 'M'])
sd(summarydatanonas$DiffprepostA[summarydatanonas$Gender == 'F'])

