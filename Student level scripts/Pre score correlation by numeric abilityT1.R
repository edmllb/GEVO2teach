# Check to see if student pre score before teaching corrleates with teacher assessment of relative ability

setwd("~/Documents/Spreadsheets and data/Thesis/Whole project data summaries/")
PSab<-read.csv("Student_summaryT1.csv",header=TRUE, stringsAsFactors = FALSE)

View(PSab)


# set  as a categorical factor 
PSab$Ability<-as.factor(PSab$Ability)

#need to make scores numeric so read in correctly
PSab$TotalpreA<-as.numeric(PSab$TotalpreA)

# ability levels as numbers not letters have to subsistute but onl use with multivariate at bottom.  

PSab$Abilitynumeric <-PSab$Ability
letters<-gsub("h",3,PSab$Abilitynumeric)
letters
letters <- gsub("m", 2, letters)
letters <- gsub("l", 1, letters)
numbers <- letters
numbers <- as.numeric(numbers)
numbers

PSab$Abilitynumeric <-numbers
set1<-(PSab$Abilitynumeric)
set2<-(PSab$TotalpreA)

#boxplot

pdf("../Graphs for inclusion/prescore by ability T1  .pdf")
boxplot(PSab$TotalpreA~PSab$Abilitynumeric,ylab="Pre-test score",main = "Tranche 1", names=c("Low ability (n = 255)","Middle ability (n = 503)","High ability (n = 393)"))
mtext(expression("P = < 2.2e-16"),side=1,adj=0.98,line=-1.5)
dev.off()



cat("No of High = ", nrow(PSab[PSab$Abilitynumeric == "3", ]))
cat("No of Mid = ", nrow(PSab[PSab$Abilitynumeric == "2", ]))
cat("No of Low = ", nrow(PSab[PSab$Abilitynumeric == "1", ]))


summary(PSab$TotalpreA[PSab$Abilitynumeric == '3'])
summary(PSab$TotalpreA[PSab$Abilitynumeric == '2'])
summary(PSab$TotalpreA[PSab$Abilitynumeric == '1'])
sd(PSab$TotalpreA[PSab$Abilitynumeric == '3'])
sd(PSab$TotalpreA[PSab$Abilitynumeric == '2'])
sd(PSab$TotalpreA[PSab$Abilitynumeric == '1'])

