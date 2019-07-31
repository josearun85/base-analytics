setwd("D:/Work/Classes/base-analytics-master/data")
library(MASS)

df <- read.csv("House_prices.csv")

df$Home <- NULL

fit1 <- lm(Price ~ ., df)
fit2 <- lm(Price ~ 1, df)

stepAIC(fit1,direction="backward")


stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))


stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))

library(car)
vif(fit1)

cd <- cooks.distance(fit1)
hist(cd,breaks=100)

# 4/(N-k-1)
cd > 4/(128-6-1)

dfnew = df[cd < 4/(128-6-1),]
fit3 <- lm(Price ~ . , dfnew )
summary(fit3)




