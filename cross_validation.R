library(caret)
setwd("C:/Users/home/Desktop/base-analytics-master/data")

house <- read.csv("house_prices.csv")
house$Home<-NULL

split<-createDataPartition(y = house$Price, p = 0.6, list = FALSE)

train<-house[split,]

test<-house[-split,]

fit <- train(Price ~ .,train,method = "lm", metric="Rsquared")
summary(fit)
fit

# Cross validation bootstrap
cvctrl<-trainControl(method = "cv" , repeats = 10)
fit <- train(Price ~ .,house,method = "lm", trControl = cvctrl,  metric="Rsquared")
fit
#plot(fit)
summary(fit)

# Cross validation cv+number = kfold
kctrl<-trainControl(method = "cv",number = 10)
fit <- train(Price ~ .,house,method = "lm", trControl = kctrl,  metric="Rsquared")
fit

rpkctrl<-trainControl(method = "repeatedcv",number = 10, repeats= 4)
fit <- train(Price ~ .,house,method = "lm", trControl = rpkctrl,  metric="Rsquared")
fit

# Cross validation jackknifing
jkctrl<-trainControl(method = "LOOCV")
fit <- train(Price ~ .,house,method = "lm", trControl = jkctrl,  metric="Rsquared")
fit
summary(fit)
getModelInfo("lm")
library(randomForest)
