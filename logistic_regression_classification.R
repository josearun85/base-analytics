setwd("D:/Work/Classes/base-analytics-master/data")

df <- read.csv("Florence.csv")

table(df$Florence)

names(df)

# gender and florence (g0=female, 1=male)
# They are related, possibly an important variable
# 2 cat
# 1. pivot table
# 2. chi-square
table(g=df$Gender, f=df$Florence)
chisq.test(df$Gender,df$Florence)

# M and florence (m? = monetary value)
# RFM analysis
# R - recency (how many weeks since the last purchase)
# f - frequency (how many times has this customer purchased from me?)
# m - monetary ( how much money has this customer spent in the past?)
# firstpurch - (how many weeks ago was the first time purchase?)

hist(df$Recency)
hist(df$F)
hist(df$M)

plot(df$F , df$M)
hist(df$CookBks)
plot(df$F,df$CookBks)
plot(df$F,df$ArtBks)
plot(df$CookBks,df$ArtBks)

for(i in 4:17){
  t <- t.test(df[,i] ~ df$Florence)
  if(t$p.value < 0.05){
    print( paste(names(df)[i], " is interested in florence") ) 
    boxplot(df[,i] ~ df$Florence, main=names(df)[i])
  }
}

# 1. women are more likely to by
# 2. They should have last visited ~ 10 weeks or less
# 3. Spend more than $200 median
# 4. Firstpurch more than 5 weeks
# 5. They are interested in ref books, artbooks, geog bks and ital art


fit1 <- glm(Florence ~ Gender + M + Recency + F + FirstPruch + RefBks +
              ArtBks + GeogBks  + ItalArt, data = df,
            family=binomial())
fit2 <- glm(Florence ~ ., data = df,
            family=binomial())
pred_prb <- predict(fit1, type="response")
pred_prb <- predict(fit2, type="response")

boxplot(pred_prb~ df$Florence)

pred_class <- ifelse(pred_prb > 0.1, 1, 0)

table(p=pred_class, act=df$Florence)
# sens - 0.65
# spec - 0.80
5*86 -  0.5 * (357)

# 1. create at least 5 new variables - feature engineering

f(a) = a_dash
lartbks = log(artbks)
(artbks+geogbks)

5*89 - (257 + 89)*.5
library(ROCR)
prd <- prediction(pred_prb,df$Florence)
prf <- performance(prd, x.measure="fpr",measure="tpr")
plot(prf,colorize=T)
auc <- performance(prd, measure="auc")
auc@y.values[[1]]

pred_class <- ifelse(pred_prb > 0.1, 1, 0)
table(p=pred_class, act=df$Florence)


df$loyal <- ifelse(df$F > 6,1 ,0)
df$highspender <- ifelse(df$M > 300, 1, 0)
df$loyalhigh <- df$loyal + df$highspender

df$cook_youth <- df$CookBks + df$YouthBks
df$high_per_visit <- df$M/df$F
df$high_per_week <- df$M / (  df$FirstPruch - df$Recency + 1)

library(MASS)
fit1 <- glm(Florence ~ 1, data=df, family=binomial())
fit2 <- glm(Florence ~ . , data=df, family=binomial())
auto_fit <- stepAIC(fit1, scope=list(lower=fit1,upper=fit2),direction="forward")
auto_fit <- stepAIC(fit2, scope=list(lower=fit1,upper=fit2),direction="backward")
auto_fit <- stepAIC(fit2, scope=list(lower=fit1,upper=fit2),direction="both")

pred_prb <- predict(auto_fit, type="response")
boxplot(pred_prb~ df$Florence)
library(ROCR)
prd <- prediction(pred_prb,df$Florence)
prf <- performance(prd, x.measure="fpr",measure="tpr")
plot(prf,colorize=T)
auc <- performance(prd, measure="auc")
auc@y.values[[1]]

pred_class <- ifelse(pred_prb > 0.1, 1, 0)
table(p=pred_class, act=df$Florence)

5*90 - (257 + 90)*.5
276.5

pred_class <- ifelse(pred_prb > 0.1, 1, 0)
table(p=pred_class, act=df$Florence)
# USING STEPAIC $276.5
summary(auto_fit)


library(caret)
caret::modelLookup()$model
df$Florence <- as.factor(df$Florence)

fit <- train(Florence ~ ArtBks + Recency + Gender + GeogBks + 
        cook_youth + DoltYBks + F + ChildBks + loyal + Rela_pruchace + 
        RefBks, family = binomial(), data = df,method="glm")

summary(fit)
my_pred <- predict(fit,type="prob")

fit2 <- train(df[,c("ArtBks","Recency","Gender","GeogBks",
"cook_youth","DoltYBks","F","ChildBks","loyal","Rela_pruchace","RefBks")],df$Florence, 
    data = df, method="rf")



my_pred <- predict(fit2,type="prob")

prd <- prediction(my_pred$`1`,df$Florence)
prf <- performance(prd, x.measure="fpr",measure="tpr")
plot(prf,colorize=T)
auc <- performance(prd, measure="auc")
auc@y.values[[1]]
boxplot(my_pred$`1`~ df$Florence)
pred_class <- ifelse(my_pred$`1` > 0.2, 1, 0)
table(p=pred_class, act=df$Florence)
?train

5*86 - (11 + 86)*.5


