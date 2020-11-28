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


table(df$Gender)

chisq.test(table(df$Gender),p=c(0.5,0.5))
# null: m:f = 50:50
# data type: 1cat, 1 exp prop
# chisq gof
# p - val : 0.2, > 0.05
# Accept null
# m:f = 50:50 (no significant different in male and female representation)


# l:m:h = (.2, .6, .2)
chisq.test(table(df$SEB),p=c(.25, .5, .25))

# test of independence
chisq.test(df$Gender, df$School)

# null - (m:f)pub = (m:f)prv [no diff in m:f between types of schools]
# data type - 2 c
# chisq test of indepdence
# p-val: 0.98
# accept
# no gender bias in pub and private schools


# Is there a relationship between SEB and School?
# null - (l:m:h)pub = (l:m:h)prv
chisq.test(df$School,df$SEB)
# reject
# seb affect which school i attend

# race - seb any relationship?
# R1:R2:R3:R4 (SEB1) = R1:R2:R3:R4 (SEB2) = R1:R2:R3:R4 (SEB3)
chisq.test(df$Race,df$SEB)
table(df$Race,df$SEB)

# race - school?
chisq.test(df$School,df$Race)

table(df$School,df$Race)

# fisher test as a replacement for chi-square, if any cell has less than 5 values
fisher.test(df$School,df$Race)


kruskal.test(df$Science ~df$Race)
# mean_rank_g1 != mean_rank_g2 != mean_rank_g3

wilcox.test(df$Math ~ df$Gender)

