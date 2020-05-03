setwd("D:/Work/Classes/base-analytics-master/data/house_prices")

df <- read.csv("data_important.csv")

library(MASS)

fit1 <- lm(SalePrice ~ 1,data=df)

fit2 <- lm(SalePrice ~ .,data=df)

fitfn <-stepAIC(fit1, scope=list(lower=fit1, upper=fit2),method="both")
summary(fitfn)

pred <- predict(fitfn)
res <- residuals(fitfn)

plot(pred,df$SalePrice)
hist(res,breaks=100)
plot(pred,res)

# rebuilding model by removing high leverage observations
# issues exist in the dataset because of a few highly priced houses
cd <- cooks.distance(fitfn)

dff <- df[cd < 4/(1460 - 29 - 1),]


fit1 <- lm(SalePrice ~ 1,data=dff)

fit2 <- lm(SalePrice ~ .,data=dff)

fitfn <-stepAIC(fit1, scope=list(lower=fit1, upper=fit2),method="both")
summary(fitfn)

pred <- predict(fitfn)
res <- residuals(fitfn)

plot(pred,dff$SalePrice)
hist(res,breaks=100)
plot(pred,res)

# Much better performance but some non-linearities persist. 
# Requires investigation one variable at a time

plot(dff$YearBuilt^2,pred)

dff$g1980 <- ifelse(dff$YearBuilt > 1985,1,0)
