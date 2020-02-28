# difference in score correlation with pre test score to show ceiling effect

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydata<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)

summarydata$TotalpreA = as.numeric(summarydata$TotalpreA)
summarydata$DiffprepostA = as.numeric(summarydata$DiffprepostA)
set1<-(summarydata$TotalpreA)
set2<-(summarydata$DiffprepostA)
cor.test(set1,set2,method="spearman")
pdf("../Rgraphs/delta and prescore correlation (fig) .pdf")
plot(set1,set2,xlab="Pre test score", ylab="Differnce in score")
abline(lm(set2~set1))
cor <- cor.test(set1,set2,method="spearman")
rho1 <- paste(signif(cor$estimate, digits = 3))
Pval <- paste(signif(cor$p.value, digits =3))
txt <- bquote(paste(italic(rho), " = ", .(rho1), ", ", italic(P), " = ", .(Pval)))
mtext(txt,side=1,adj=0.98,line=-1.5)
dev.off()
cor$p.value


summary(set1)
summary(set2)
sd(set1,na.rm=TRUE)
sd(set2,na.rm=TRUE)

