setwd("C:/Users/arun.j/Desktop/base-analytics-master/data")
library(popbio)

tele <- read.csv("Telecom_data.csv")

# Exploratory analysis
names(tele)

primary_var_list <- c("tenure",   "age","address",  "income")
primary_cat_var_list <- c("region" ,"marital")

behvr_var_list <- c("longten",  "tollten" ,"equipten", "cardten",  "wireten")

for(var in primary_var_list){
  print(logi.hist.plot(tele[,var], tele$churn),mainlabel=var,boxp = F,type = "hist")
}

for(var in primary_cat_var_list){
  print(table(tele[,var], tele$churn))
  print(chisq.test(tele[,var], tele$churn)$p.value)
}



# income lowers churn
# years spent at address lowers churn
# Age affects churn, mostly under ~36
# Tenure -> people who churn are most risky 
# in 5 to 10 months, risk drops considerable after 35
# No apparent relationship to region or marital status



# Data prep
tele$riskage <- ifelse(tele$age < 35, 1, 0)
tele$earlystage <- ifelse(tele$tenure < 9, 1, 0)

set.seed(10)
indx <- sample(1:nrow(tele), 750)
train = tele[indx,]
test = tele[-indx,]

fmla <- as.formula(paste0("churn ~",paste0(primary_var_list,collapse = " + "),"+ riskage + earlystage"))


fit1 <- glm(fmla, data = train )
summary(fit1)
library(MASS)
stepAIC(fit1)

final_fit <- glm(churn ~ tenure + age + earlystage, data=train)
summary(final_fit)

### Check sens and spec for base model

base_pred <- predict(final_fit,type="response")

library(ROCR)
pred <- prediction(base_pred,train$churn)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=T)

auc = performance(pred, measure = "auc")
auc@y.values[[1]]

# AuC of 0.76 for base with 3 variables only

base_pred_test <- predict(final_fit,type="response",newdata=test)

pred2 <- prediction(base_pred_test,test$churn)
roc.perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf2,colorize=T)

auc2 = performance(pred2, measure = "auc")
auc2@y.values[[1]]

# AuC of 0.76 , validation AuC of 0.71 -> close enough fit

# Using a cutoff of  0.327172285 from auc output to classify 
# at 25% false positive rate

class_pred_test <- ifelse(base_pred_test> 0.327172285,1,0)
table(pr=class_pred_test, act=test$churn)

# About 40% churn were right 1's
# Out of 65 churners we identified 46, sens above 80%

## Insert tests here
# 1.somers
# 2. hl test

## This model looks good. Now iterate through cross-validation
library(caret)
train$churn = as.factor(train$churn)
test$churn = as.factor(test$churn)
trct = trainControl(method="cv",number=10)

fit11 <- train(churn ~ tenure + age + earlystage,train,trControl=trct,method="glm" )
summary(fit11)
kfold_final_pred <- predict(fit11,newdata=test,type="prob")
pred3 <- prediction(kfold_final_pred[,2],test$churn)
roc.perf3 = performance(pred3, measure = "tpr", x.measure = "fpr")
plot(roc.perf3,colorize=T)

auc2 = performance(pred3, measure = "auc")
auc2@y.values[[1]]



fit22 <- train(churn ~ tenure + age + earlystage,train,trControl=trct,method="rf" )

rfoutput = as.numeric(predict(fit22,newdata=test))
pred4 <- prediction(rfoutput,test$churn)
roc.perf4 = performance(pred4, measure = "tpr", x.measure = "fpr")
plot(roc.perf4,colorize=T)

auc2 = performance(pred4, measure = "auc")
auc2@y.values[[1]]


## Attempted alternate classifier, worse performance compared to 
## logistic
## After cross validation, best ROC is still 0.718





