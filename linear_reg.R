setwd("D:/base-analytics-master/data")

house <- read.csv("house_prices.csv")

boxplot(house$Price ~house$Brick)
boxplot(house$Price ~house$Neighborhood)
out <- aov(house$Price ~house$Neighborhood)
summary(out)

a<- rnorm(100)
b <- rnorm(100)
cor(a,b)
plot(a,b)
a[100] <- 100

b[100] <- 100
cor(a,b)

cor(a,b,method="spearman")

## Create dummy

house$east <- ifelse(house$Neighborhood=="East",1,0)
house$west <- ifelse(house$Neighborhood=="West",1,0)
house$brickd <- ifelse(house$Brick=="yes",1,0)

# Split data in train and test
set.seed(1)
indexes <- sample(1:128, 80, replace=F)
train <- house[indexes,]
test <- house[-indexes,]

# Build model

fit1 <- lm(Price ~ SqFt, data = train)
summary(fit1)

fit2 <- lm(Price ~ SqFt + Bedrooms, data = train)
summary(fit2)
plot(fit2)
train$pred2 <- predict(fit2)
train$resd2 <- residuals(fit2)

plot(train$pred2, train$resd2)
hist(train$resd2)

### Validate your model

test$pred <- predict(fit2, newdata=test)
test$res <- test$Price - test$pred

plot(test$pred, test$res)
hist(test$res)

cor(test$Price,test$pred) * cor(test$Price,test$pred) 





fit2 <- lm(Price ~ SqFt + Bedrooms + Neighborhood:Brick, data = train)
summary(fit2)

fit2 <- lm(Price ~ SqFt + Bedrooms + SqFt * Bedrooms, data = train)
summary(fit2)


# Outliers

train$cook<-cooks.distance(fit2)
train_out<-train[train$cook < 4/(80-2-1),]


