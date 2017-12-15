library(ROCR)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
train$bankruptcy <- as.factor(train$bankruptcy)

set1 <- c("workcap_totasst","profoperact_finexp","logtotasst","curasst_invt__lngtrmliab",
          "sales_totasst","profsales_sales","rotrecplusinvturnoverdays","retainear_totasst","recv365_sales")


(fmla <- as.formula(paste("bankruptcy ~ ",  paste(set1, collapse= "+"))))

fit <- glm(fmla,data=train,family=binomial())
summary(fit)

train$prob <- predict(fit,type="response")
pr <- prediction(train$prob,train$bankruptcy)
perf <- performance(pr,"tpr","fpr")
plot(perf,colorize=T)
trainauc <- performance(pr,"auc")
trainauc@y.values[[1]]

test$prob <- predict(fit,type="response",newdata=test)
pr <- prediction(test$prob,test$bankruptcy)
perf <- performance(pr,"tpr","fpr")
plot(perf,colorize=T)
testauc <- performance(pr,"auc")
testauc@y.values[[1]]

#### Model 2

set2 <- c("workcap_totasst","logtotasst",
          "sales_totasst","rotrecplusinvturnoverdays","recv365_sales")


(fmla2 <- as.formula(paste("bankruptcy ~ ",  paste(set2, collapse= "+"))))

fit2 <- glm(fmla2,data=train,family=binomial())
summary(fit2)

train$prob2 <- predict(fit2,type="response")
pr <- prediction(train$prob2,train$bankruptcy)
perf2 <- performance(pr,"tpr","fpr")
plot(perf,colorize=T)
trainauc <- performance(pr,"auc")
trainauc@y.values[[1]]

test$prob2 <- predict(fit2,type="response",newdata=test)
pr <- prediction(test$prob2,test$bankruptcy)
perf22 <- performance(pr,"tpr","fpr")
plot(perf,colorize=T)
testauc <- performance(pr,"auc")
testauc@y.values[[1]]

## Determine threshold
min_dst=9999999
for(i in 1:length(perf2@y.values[[1]])){
  dst = (0-perf2@x.values[[1]][i]) * (0-perf2@x.values[[1]][i]) + (1-perf2@y.values[[1]][i])*(1-perf2@y.values[[1]][i])
  if(dst < min_dst){
    threshold = perf2@alpha.values[[1]][i]
    min_dst=dst
    print(dst)
  }
}


print(threshold)

train$class = ifelse(train$prob >= threshold,1,0)
table(cl=train$class,bn=train$bankruptcy)

test$class = ifelse(test$prob >= threshold,1,0)
table(cl=test$class,bn=test$bankruptcy)

# rebuild model on train+test

a<-read.csv("train.csv")
b<-read.csv("test.csv")
c<-rbind(a,b)
c$bankruptcy <- as.factor(c$bankruptcy)

final_fit <- glm(fmla2,data=c,family=binomial())

c$prob <- predict(final_fit,type="response")
pr3 <- prediction(c$prob,c$bankruptcy)
perf3 <- performance(pr3,"tpr","fpr")
plot(perf3,colorize=T)
testauc <- performance(pr3,"auc")
testauc@y.values[[1]]

## Determine threshold
min_dst=9999999
for(i in 1:length(perf3@y.values[[1]])){
  dst = (0-perf3@x.values[[1]][i]) * (0-perf3@x.values[[1]][i]) + (1-perf3@y.values[[1]][i])*(1-perf3@y.values[[1]][i])
  if(dst < min_dst){
    final_threshold = perf3@alpha.values[[1]][i]
    min_dst=dst
    print(dst)
  }
}


print(final_threshold)
c$class = ifelse(c$prob >= final_threshold,1,0)
table(cl=c$class,bn=c$bankruptcy)


## Apply random forest
library(randomForest)
fit.rf <- randomForest(fmla, data=c, importance=TRUE,ntree=500,mtry=5,proximity=T)

c$rf <- predict(fit.rf,type = "response")
table(cl=c$rf,bn=c$bankruptcy)

