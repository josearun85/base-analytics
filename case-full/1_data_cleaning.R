setwd("D:/Work/Classes/base-analytics-master/data/house_prices")

df <- read.csv("train.csv")
names(df)

# Data quality

null_report <- sapply(df, function(x) sum(is.na(x)))

missing_col_list <- names(null_report)[which(null_report>0)]

# Let's check the columns which have missing values

sapply(df[,missing_col_list], function(x) sum(is.na(x)))

# For all factor variables, we will temporarily create a new category 
# called missing

for(col in missing_col_list){
  if(class(df[,col]) == "factor"){
    # adding missing as a new category 
    df[,col] = addNA(df[,col])
  }
}

# Checking what's left

null_report <- sapply(df, function(x) sum(is.na(x)))
missing_col_list <- names(null_report)[which(null_report>0)]
sapply(df[,missing_col_list], function(x) sum(is.na(x)))

# Lotfrontage has a significant number of missing values

hist(df$LotFrontage)
plot(df$LotArea,df$LotFrontage)

# Due to relation between frontage and lotarea, we can explore model based imputation

hist(df$LotArea,breaks=100,xlim = c(0,200000))
hist(df$LotArea[is.na(df$LotFrontage)],breaks=100,xlim = c(0,200000))

# We can confirm that missing values don't seem to be for specific configuration, but occur for all values
# of lot area

df$LotFrontageImp = df$LotFrontage

library(Hmisc)
impute_arg <- aregImpute(LotFrontage~ LotArea, data = df,n.impute = 1)

for(i in 1:length(impute_arg$na$LotFrontage)){
  index = impute_arg$na$LotFrontage[i]
  value = impute_arg$imputed$LotFrontage[i,1]
  if(is.na(df$LotFrontageImp[index])){
    print("yes")
    df$LotFrontageImp[index] = value
  }else{
    print("not missing")
  }
  if(!is.na(df$LotFrontageImp[index])){
    print("no")
  }else{
    print(df$LotFrontageImp[index])
  }
}

sapply(df[,c("LotFrontageImp","LotFrontage")], function(x) sum(is.na(x)))

# Garage yrBlt
df[,c("GarageType","GarageYrBlt")]
plot(df$YearBuilt,df$GarageYrBlt)

 # majority of garages are built when the house is built, also, all missing garage missing years
# are when there is no garage in the house. For current purposes, missing values we will just 
# impute with year built

for(i in 1:nrow(df)){
  if(is.na(df$GarageYrBlt[i])){
    df$GarageYrBlt[i] = df$YearBuilt[i]
  }
}

sapply(df[,missing_col_list], function(x) sum(is.na(x)))

# last column to treat is "MasVnrType"     "MasVnrArea"

df[is.na(df$MasVnrArea),c("MasVnrType","MasVnrArea")]

# we will set all missing values here to 0, as the missing values are based on no
# veneer being present

df[is.na(df$MasVnrArea),"MasVnrArea"]=0
sapply(df[,missing_col_list], function(x) sum(is.na(x)))

# saving cleaned data for eda after dropping lotfrontage

df$LotFrontage <- NULL
write.csv(df,"cleaned_train.csv",row.names = F)

