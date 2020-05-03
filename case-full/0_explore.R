setwd("D:/Work/Classes/base-analytics-master/data/house_prices")

df <- read.csv("train.csv")
names(df)


hist(df$SalePrice)
boxplot(df$SalePrice ~ df$MSSubClass)
boxplot(df$SalePrice ~ df$Neighborhood)
boxplot(df$SalePrice ~ df$YearBuilt)

df$age <- df$YrSold - df$YearBuilt 
boxplot(df$SalePrice ~ df$age)
boxplot(df$SalePrice ~ df$PoolQC)

boxplot(df$SalePrice ~ df$GarageQual)
boxplot(df$SalePrice ~ df$HouseStyle)
boxplot(df$SalePrice ~ df$OverallQual)
boxplot(df$SalePrice ~ df$MiscFeature)
boxplot(df$SalePrice ~ df$MiscVal)
boxplot(df$SalePrice ~ df$GarageCars)

cor(df$SalePrice,df$age)
plot(df$age,df$SalePrice)

cor(df$SalePrice,df$LotArea)
plot(df$LotArea,df$SalePrice)

# Dropping extremely large houses as they are not representative

df<- df[df$LotArea < 25000,]
cor(df$SalePrice,df$LotArea)
plot(df$LotArea,df$SalePrice)



df$PoolQC<-as.character(df$PoolQC)
df$PoolQC[is.na(df$PoolQC)] <- "NOPOOL"

df$GarageQual<-as.character(df$GarageQual)
df$GarageQual[is.na(df$GarageQual)] <- "NOGARAGE"


df$MiscFeature<-as.character(df$MiscFeature)
df$MiscFeature[is.na(df$MiscFeature)] <- "NOFEAT"

df$GarageCars[is.na(df$GarageCars)] <- "NOG"


nh_type1 <- c("Blueste","BrkSide","ClearCr","CollgCr","Gilbert","NAmes","NPkVill","NWAmes","OldTown","SawyerW","SWISU")
df$Neighborhood = as.character(df$Neighborhood)
df$NHNEW <- ifelse(df$Neighborhood %in% nh_type1, "SIM",df$Neighborhood)
boxplot(df$SalePrice ~ df$NHNEW)

library(ggplot2)
ggplot(df) + 
  geom_point(aes(x=LotArea,y=SalePrice,color=factor(MSSubClass))) 
ggplot(df) + 
  geom_point(aes(x=LotArea,y=SalePrice,color=age)) 
hist(df$YearBuilt)


fit <- lm(SalePrice ~ age + LotArea+NHNEW,data=df)
summary(fit)



fit <- lm(SalePrice ~ age + LotArea+NHNEW+factor(PoolArea),data=df)
summary(fit)

fit <- lm(SalePrice ~ GrLivArea+age +NHNEW+ LotArea+PoolQC + GarageQual + 
            OverallQual +factor(GarageCars)+FullBath,data=df)
summary(fit)

fit <- lm(SalePrice ~ GrLivArea+age +NHNEW+ LotArea+PoolQC + GarageQual + 
            OverallQual +factor(GarageCars)+FullBath,data=df)


