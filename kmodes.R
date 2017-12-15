library(klaR)
setwd("C:/Users/arun.j/Desktop/Class/cluster")

master <- read.csv("HR-Attrition-Data.csv")
kmod <- kmodes(master[,3:8],4)
master$cluster <- kmod$cluster

library(vcd)

table(master$cluster,master$Department)
table(master$cluster,master$Attrition)

doubledecker(cluster ~ Attrition + Department, data=master[,c("Department","Attrition","cluster")])
doubledecker(Attrition ~ cluster + Department, data=master[,c("Department","Attrition","cluster")])


boxplot(master$DistanceFromHome ~ master$cluster)
boxplot(master$DistanceFromHome ~ master$Attrition)
table(master$Attrition,master$cluster)

kmod5 <- kmodes(master[,3:8],5)
master$cluster5 <- kmod5$cluster
table(master$Attrition,master$cluster5)
doubledecker(cluster5 ~ Attrition + Department, data=master[,c("Department","Attrition","cluster5")])

kmod3 <- kmodes(master[,3:8],3)
master$cluster3 <- kmod3$cluster
table(master$Attrition,master$cluster3)

dst <- dist(master[,c(1,4,6)],method="")
