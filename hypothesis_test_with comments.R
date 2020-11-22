setwd("D://Work/Classes/base-analytics-master/data")

df <- read.csv("students_performance.csv")

# hypothesis: avg performance in math = 52
# I have no evidence to disprove that 52 is the average. Accept the null hypothesis.
t.test(df$Math, mu=52)

# hypothesis: avg performance in math = 80
# I am at least 95% confident that 80 is NOT the average. Reject the null hypothesis.
t.test(df$Math, mu=80)

# hypothesis: avg perf of science is 65 (xbar=65)
# confidence level = 95%, alpha = 0.05
# confidence level = 99%, alpha = 0.01
t.test(df$Science, mu=65)
# we are at least 99% confident that avg of science is NOT 65

# at conf of 99.5%, test if avg of write is 52
# null: avg_write = 52
# alpha = 0.005 (1 - 0.995)
t.test(df$Write, mu=52)
# t = 1.15
# p = 0.2489
# p > alpha
# hence, we accept the null hypothesis
# There is no evidence to disprove that the avg of write is 52


t.test(df$Read,mu=60)
# abg of read = 60
# alpha = 0.05 (95%)
# p = 2e-16
# p < alpha
# reject null, im 95% confident (alpha=0.05)


# Do males and females, perform differently on math
# null: avg math score for male = avg math score for female
# data types: gender=categoric (2 levels), math=numeric
# 2 independent sample t-test
# alpha = 0.05 (95%)

t.test(df$Math ~ df$Gender)
# t = 0.41
# p = 0.68
# p > alpha
# accept null
# males and females, do not perform differently!

t.test(df$Write ~ df$Gender)
# p = 0.0003
# t = -3.6
# p < alpha
# reject null
# Group 1 (females) perform better than group 0 (males)

# ANOVA
# 1 numeric, 1 cat (2+ levels)
anv_out <- aov(df$Write ~df$SEB )
summary(anv_out)
# null: avg_write_seb_1 = avg_write_seb_2 = avg_write_seb_3
# inference: p<alpha, reject null
# SEB does affect performance in write

# does race affect science?
# anova (1n,1c(4levels))
# 
anv_out <- aov(df$Science ~df$Race )
summary(anv_out)
# p < alpha, reject
# race does affect science perf

unique(df$Race)



# paired t-test
# 2 numeric, paired (same student, measured twice)

t.test(df$Write, df$Read, paired=T)
# avg (write - read) = 0
# p-value = 0.38
# accept null hypothesis

