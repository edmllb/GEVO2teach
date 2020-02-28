library(effsize)
setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
PPdata<-read.csv("Student_summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
PPdata[PPdata=="na"]<-NA
View(PPdata)

#data to be read in for prepost comparison needs all nas removed so only matched pairs are read in

#correlation
set1<-(PPdata$TotalpreA)
set2<-(PPdata$TotalpostA)
cor.test(set1,set2,method="spearman")
plot(set1,set2,pch=3,xlab="pre teaching",ylab="post teaching")

wilcox.test(set2,set1, paired = FALSE,conf.int = TRUE)
res<-cliff.delta(set2,set1,return.dm=TRUE,conf.level=.95)
print(res)

# or 
wilcox.test(PPdata$TotalpreA,PPdata$TotalpostA, paired = FALSE)

pdf("../Graphs for inclusion/unmatched pre post boxplot T1  .pdf")
boxplot(PPdata$TotalpreA,PPdata$TotalpostA,ylim=c(0,15),ylab="Test score",main="Tranche 1",names=c("Pre teaching (n = 1151)","Post teaching (n = 988)"))
mtext(expression("P = < 2.2e-16"),side=1,adj=0.98,line=-1.5)
dev.off()




#means and SD 
mean(PPdata$TotalpreA)
mean(PPdata$TotalpostA,na.rm=TRUE)
sd(PPdata$TotalpreA)
sd(PPdata$TotalpostA,na.rm=TRUE)
summary(PPdata$TotalpreA)
summary(PPdata$TotalpostA)
