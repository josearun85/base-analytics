setwd("C:/Users/home/Desktop/base-analytics-master/data")
df <- read.csv("Telecom_data.csv")

# Split dataset
set.seed(100)
indx <- sample(nrow(df),floor(nrow(df)*.7),replace=F)
train <- df[indx,]
test <- df[-indx,]

# Testing variables
names(train)
num_vars <- c("tenure","age","income")
cat_vars <- c("region","ed","marital")

useful_vars <- c()
for(var in num_vars){
  tt <- t.test(train[,var] ~ train$churn)
  if(tt$p.value < 0.05){
    print(var)
    print(tt$p.value)
    useful_vars = c(useful_vars,var)
  }
}

for(var in cat_vars){
  chi <- chisq.test(train[,var], train$churn)
  if(chi$p.value < 0.05){
    print(var)
    print(chi$p.value)
    useful_vars = c(useful_vars,var)
  }
}

print(useful_vars)
library(popbio)

for(var in useful_vars){
  logi.hist.plot(train[,var],train$churn,xlab=var)
}

# Creating model statement with all included
fml <- as.formula(paste0("churn~",paste(useful_vars,collapse="+")))

fit1 <- glm(fml, data=train,family=binomial())
summary(fit1)

library(MASS)
stpfit <- stepAIC(fit1)

# Predictions
steppred <- predict(stpfit,type="response")

library(ROCR)
prd = prediction(steppred,train$churn)
roc = performance(prd,x.measure = "fpr",measure="tpr")
plot(roc,colorize=TRUE)
