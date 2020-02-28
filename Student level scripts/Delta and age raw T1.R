# spearman for age v Diff pre/post A

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
summarydatanonas<-read.csv("Student_summarynonasT1.csv",header=TRUE, stringsAsFactors = FALSE)
View(summarydatanonas)

summarydatanonas$DiffprepostA = as.numeric(summarydatanonas$DiffprepostA)
summarydatanonas$Difference_in_days = as.numeric(summarydatanonas$Difference_in_days)

set1<-(summarydatanonas$DiffprepostA)
set2<-(summarydatanonas$Difference_in_days)
cor.test (set2,set1,method="spearman")

# normal scatterplot with abline
pdf("../Rgraphs/Age and delta normal plot .pdf")
plot(set2,set1,pch=3,xlab="Age on date of pre-test in days", ylab="Difference in score")
abline(lm(set1~set2), col="red") 
dev.off()

# plot by density
pdf("../Rgraphs/Age and delta (fig2.1) .pdf")

# plot_colorByDensity(set2,set1,"what ever x lab","what ever y lab is ")

plot_colorByDensity = function(x1,x2,xlab,ylab,
                               ylim=c(min(x2),max(x2)),
                               xlim=c(min(x1),max(x1))) {
  df <- data.frame(x1,x2)
  x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  
  #### colours go from light to dark darker = more dense
  cols <-  colorRampPalette(c("#CCCCCC", "#A3A3A3", "#7A7A7A","#515151", "#282828", "#000000"))(256)
  df$col <- cols[df$dens]
  plot(x2~x1, data=df[order(df$dens),],pch=20,col=col, cex=1,xlab=xlab,ylab=ylab)
}

pdf("../Rgraphs/Prepost correlation (fig3) .pdf")
plot_colorByDensity(set2,set1,"Age on date of pre-test (days)", "Difference in score")
abline(lm(set1~set2))
cor <- cor.test(set1,set2,method="spearman")
rho1 <- paste(signif(cor$estimate, digits = 3))
Pval <- paste(signif(cor$p.value, digits =3))
txt <- bquote(paste(italic(rho), " = ", .(rho1), ", ", italic(P), " = ", .(Pval)))
mtext(txt,side=1,adj=0.98,line=-1.5)
dev.off()


