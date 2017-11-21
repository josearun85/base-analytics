setwd("C:/Users/arun.j/Desktop/Rcode/")
master <- read.csv("data/students_performance.csv")

# Exploring the dataset


# 1 - Number of students
nrow(master)

# 2 - Score in Math
mean(master$Math)
sd(master$Math)
median(master$Math)
hist(master$Math)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(master$Math)


# 3 - Relationship between variables
plot(master$Math, master$Science)
boxplot(master$Math ~ master$Gender)
boxplot(master$Math ~ master$Race)
