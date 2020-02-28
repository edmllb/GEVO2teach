library(effsize)
library(rcompanion)
setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
Lowabnonas<-read.csv("Student_summarylowabilityonlynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)
View(Lowabnonas)



#need to make scores numeric so read in correctly
Lowabnonas$TotalPreA<-as.numeric(Lowabnonas$TotalpreA)
Lowabnonas$TotalPostA<-as.numeric(Lowabnonas$TotalpostA)

wilcox.test(Lowabnonas$TotalpostA,Lowabnonas$TotalpreA,paired = TRUE)
res<-cliff.delta(Lowabnonas$TotalpostA,Lowabnonas$TotalpreA,return.dm=TRUE,conf.level=.95)
print(res)





boxplot(Lowabnonas$TotalPreA,Lowabnonas$TotalPostA,ylim=c(0,15),ylab="Test Score",names=c("Pre teaching","Post teaching"))
mtext(expression("P = < 2.2e-16"),side=1,adj=0.98,line=-1.5)
dev.off()
