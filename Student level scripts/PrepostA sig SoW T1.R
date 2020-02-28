library(rcompanion)
library(effsize)
setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
PPdata<-read.csv("Student summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
View(PPdata)


SoW<-as.factor(PPdata$SoW)
TotalpreA<-as.numeric(PPdata$TotalpreA)
TotalpostA<-as.numeric(PPdata$TotalpostA)



# correlation lines for all SoW on single graph colour coded
plot(TotalpostA[SoW=="1"],TotalpreA[SoW=="1"],col="blue",pch=16,cex=0.5, xlab="Post-test Score", ylab="Pre-test Score",main="Tranche 1")
abline(lm(TotalpreA[SoW=="1"]~TotalpostA[SoW=="1"]),col="blue")
# 1.8,11 postition of lengend relative to graph axes
legend(1.8,11, legend=c("SoW 1", "SoW 2","SoW 3","SoW 4"),col=c("blue", "red", "green", "purple"), lty=1, cex=0.8, ncol=2,lwd=2)

points(TotalpostA[SoW=="2"],TotalpreA[SoW=="2"],col="red",pch=16,cex=0.5)
abline(lm(TotalpreA[SoW=="2"]~TotalpostA[SoW=="2"]),col="red")


points(TotalpostA[SoW=="3"],TotalpreA[SoW=="3"],col="green",pch=16,cex=0.5)
abline(lm(TotalpreA[SoW=="3"]~TotalpostA[SoW=="3"]),col="green")

points(TotalpostA[SoW=="4"],TotalpreA[SoW=="4"],col="purple",pch=16,cex=0.5)
abline(lm(TotalpreA[SoW=="4"]~TotalpostA[SoW=="4"]))
dev.off()


# testing for normality
shapiro.test(TotalpreA[SoW=="1"])
shapiro.test(TotalpreA[SoW=="2"])
shapiro.test(TotalpreA[SoW=="3"])
shapiro.test(TotalpreA[SoW=="4"])

cor.test(TotalpostA[SoW=="1"],TotalpreA[SoW=="1"],method="spearman")
cor.test(TotalpostA[SoW=="2"],TotalpreA[SoW=="2"],method="spearman")
cor.test(TotalpostA[SoW=="3"],TotalpreA[SoW=="3"],method="spearman")
cor.test(TotalpostA[SoW=="4"],TotalpreA[SoW=="4"],method="spearman")
# all parametric so Wilcoxon/


# individual boxplots and Wilcoxons 

pdf("../Graphs for inclusion/prepost boxplots by SoW T1.pdf")
# to get graphs 2x2
par(mfrow=c(2,2))
# to make margins right size tp print
par(mar=c(2,4,2,2))


par(oma=c(2,2,2,2))

boxplot(TotalpreA[SoW=="1"],TotalpostA[SoW=="1"],col="blue",pch=16,cex=0.5,ylab="Score",names=c("Pre teaching","Post teaching"),main="SoW 1")
title("Tranche 1", outer=TRUE)

# , not ~ for wilcoxon as comparing agianst each other not how they interact
wilcox.test(TotalpostA[SoW=="1"],TotalpreA[SoW=="1"],paired=TRUE)
# not epsilon squared as cliffs d better
# epsilonSquared(x=TotalpostA[SoW=="1"],TotalpreA[SoW=="1"],paired=TRUE)
res<-cliff.delta(TotalpostA[SoW=="1"],TotalpreA[SoW=="1"],return.dm=TRUE)
print(res)


boxplot(TotalpreA[SoW=="2"],TotalpostA[SoW=="2"],col="red",pch=16,cex=0.5,ylab="Score",names=c("Pre teaching","Post teaching"), main="SoW 2")
wilcox.test(TotalpreA[SoW=="2"],TotalpostA[SoW=="2"],paired=TRUE)
#epsilonSquared(x=TotalpostA[SoW=="2"],TotalpreA[SoW=="2"],paired=TRUE)
res<-cliff.delta(TotalpostA[SoW=="2"],TotalpreA[SoW=="2"],return.dm=TRUE)
print(res)


boxplot(TotalpreA[SoW=="3"],TotalpostA[SoW=="3"],col="green",pch=16,cex=0.5,ylab="Score",names=c("Pre teaching ","Post teaching"), main="SoW 3")
wilcox.test(TotalpreA[SoW=="3"],TotalpostA[SoW=="3"],paired=TRUE)
#epsilonSquared(x=TotalpostA[SoW=="3"],TotalpreA[SoW=="3"],paired=TRUE)
res<-cliff.delta(TotalpostA[SoW=="3"],TotalpreA[SoW=="3"],return.dm=TRUE)
print(res)



boxplot(TotalpreA[SoW=="4"],TotalpostA[SoW=="4"],col="purple",pch=16,cex=0.5,ylab="Score",names=c("Pre teaching","Post teaching"),main="SoW 4")
wilcox.test(TotalpreA[SoW=="4"],TotalpostA[SoW=="4"],paired=TRUE)
# epsilonSquared(x=TotalpostA[SoW=="4"],TotalpreA[SoW=="4"],paired=TRUE)

res<-cliff.delta(TotalpostA[SoW=="4"],TotalpreA[SoW=="4"],return.dm=TRUE)
print(res)

dev.off()


PPdata$SoW <- ordered(PPdata$SoW, levels=c("1", "2", "3","4"))

cat("No given SoW 1 = ", nrow(PPdata[PPdata$SoW == "1", ]))
cat("No given SoW 2 = ", nrow(PPdata[PPdata$SoW == "2", ]))
cat("No given SoW 3 = ", nrow(PPdata[PPdata$SoW == "3", ]))
cat("No given SoW 4 = ", nrow(PPdata[PPdata$SoW == "4", ]))

sd(TotalpreA[SoW=="1"])
summary(TotalpreA[SoW=="1"])
sd(TotalpostA[SoW=="1"])
summary(TotalpostA[SoW=="1"])

sd(TotalpreA[SoW=="2"])
summary(TotalpreA[SoW=="2"])
sd(TotalpostA[SoW=="2"])
summary(TotalpostA[SoW=="2"])

sd(TotalpreA[SoW=="3"])
summary(TotalpreA[SoW=="3"])
sd(TotalpostA[SoW=="3"])
summary(TotalpostA[SoW=="3"])

sd(TotalpreA[SoW=="4"])
summary(TotalpreA[SoW=="4"])
sd(TotalpostA[SoW=="4"])
summary(TotalpostA[SoW=="4"])



