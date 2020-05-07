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

dff <- df[cd < 4/(1460 - 54 - 1),]


fit1 <- lm(SalePrice ~ 1,data=dff)

fit2 <- lm(SalePrice ~ .,data=dff)

fitfn <-stepAIC(fit1, scope=list(lower=fit1, upper=fit2),method="both")
summary(fitfn)

pred <- predict(fitfn,newdata=df)
res <- df$SalePrice - pred
plot(pred,df$SalePrice)
hist(res,breaks=100)
plot(pred,res)

# Much better performance but some non-linearities persist. 
# Requires investigation one variable at a time

plot(df$YearBuilt^2,pred)

dff$g1985 <- ifelse(dff$YearBuilt > 1985,1,0)

fit1 <- lm(SalePrice ~ 1,data=dff)

fit2 <- lm(SalePrice ~ .,data=dff)

fitfn <-stepAIC(fit1, scope=list(lower=fit1, upper=fit2),method="both")
summary(fitfn)
fitfn$terms
# cross validation

library(caret)

trctrl = trainControl(method="cv",number=15,verboseIter=T)
cv = train(SalePrice ~ OverallQual + BsmtFinSF1 + YearBuilt + 
             Neighborhood_NridgHt + GarageArea + TotalBsmtSF + YearRemodAdd + 
             LotFrontageImp + Neighborhood_StoneBr + Neighborhood_NoRidge + 
             Fireplaces + Neighborhood_Somerst + MSZoning_RL + Neighborhood_BrkSide + 
             WoodDeckSF + Condition1_Feedr + RoofStyle_Gable + Neighborhood_Veenker + 
              + GarageYrBlt + Condition2_PosA + RoofMatl_Membran + 
             OpenPorchSF + Condition1_RRAe + MasVnrArea + Neighborhood_NAmes + 
             FullBath + Neighborhood_Edwards + GarageCars + Neighborhood_Sawyer + 
             HouseStyle_2Story + X2ndFlrSF + X1stFlrSF + Neighborhood_OldTown + 
             Condition2_RRNn,data=df,method="lm",trControl =trctrl           )
 cv
