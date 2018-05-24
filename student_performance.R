setwd("D:/base-analytics-master/data")
library(e1071)
students <- read.csv("students_performance.csv")

table(students$Gender)
table(students$Gender,students$Race)

hist(students$Read)
skewness(students$Read)

par(mfrow=c(2,1))
hist(students$Read[students$Gender==1])
hist(students$Read[students$Gender==0])


t.test(students$Math,mu=70)
t.test(students$Math~students$Gender)

t.test(students$Math, students$Read,paired = T)

cor(students$Math, students$Read)
plot(students$Math, students$Read)

anv_out <- aov(students$Write ~students$SEB )
summary(anv_out)
boxplot(students$Write ~students$SEB)


# Wilcoxon
wilcox.test(students$Math,mu=70)
wilcox.test(students$Math ~students$Gender)
wilcox.test(students$Math, students$Read,paired = T)
# Kruskal wallis
kruskal.test(students$Write ~students$SEB) 


chisq.test(table(students$Gender), p=c(.50,.50))
chisq.test(students$SEB, students$School)
