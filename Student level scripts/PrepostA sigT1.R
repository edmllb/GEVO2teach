library(effsize)
setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
PPdata<-read.csv("Student summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)
View(PPdata)

#data to be read in for prepost comparison needs all nas removed so only matched pairs are read in

#correlation
set1<-(PPdata$TotalpreA)
set2<-(PPdata$TotalpostA)
cor.test(set1,set2,method="spearman")
# better to plot by colour density
plot(set1,set2,pch=3,xlab="pre teaching",ylab="post teaching")

# colour desnity plot 
plot_colorByDensity = function(x1,x2,xlab,ylab,
                               ylim=c(min(x2),max(x2)),
                               xlim=c(min(x1),max(x1)),title) {
  df <- data.frame(x1,x2)
  x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  
  # colours go from light to dark darker = more dense
  cols <-  colorRampPalette(c("#CCCCCC", "#A3A3A3", "#7A7A7A","#515151", "#282828", "#000000"))(256)
  df$col <- cols[df$dens]
  plot(x2~x1, data=df[order(df$dens),],pch=20,col=col, cex=1,xlab=xlab,ylab=ylab,main=title)
}

pdf("../Graphs for inclusion/correlation of pre-post T1 .pdf")
plot_colorByDensity(set2,set1,"Post-test score", "Pre-test score",title="Tranche 1")
abline(lm(set1~set2))
cor <- cor.test(set1,set2,method="spearman")
rho1 <- paste(signif(cor$estimate, digits = 2))
Pval <- paste(signif(cor$p.value, digits =2))
txt <- bquote(paste(italic(rho), " = ", .(rho1), ", ", italic(P), " = ", .(Pval)))
mtext(txt,side=1,adj=0.98,line=-1.5)
dev.off()


boxplot(PPdata$TotalpreA,PPdata$TotalpostA,ylim=c(0,15),ylab="score",main="boxplot of pre and post teaching scores T1",names=c("pre teaching","post teaching"))

wilcox.test(set2,set1, paired = TRUE,conf.int = TRUE)
# or
wilcox.test(PPdata$TotalpostA,PPdata$TotalpreA, paired = TRUE)
print()

#effect size
res<-cliff.delta(set2,set1,return.dm=TRUE,conf.level=.95)
print(res)

#means and SD 
mean(PPdata$TotalpreA)
mean(PPdata$TotalpostA)
sd(PPdata$TotalpreA)
sd(PPdata$TotalpostA)
summary(PPdata$TotalpreA)
summary(PPdata$TotalpostA)

