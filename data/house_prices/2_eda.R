setwd("D:/Work/Classes/base-analytics-master/data/house_prices")

df <- read.csv("cleaned_train.csv")
# recode categories as strings if numeric values
catvars = c()
numvars = c()
for(i in names(df)[names(df)!="SalePrice"]){
  if(class(df[,i])=="factor"){
    catvars = c(catvars,i)
  }else{
    numvars = c(numvars,i)
  }
}


library(ggplot2)
library(corrplot)
corrplot(cor(df[,c("SalePrice",numvars)]))

imp_num_vars = c()
for(i in numvars){
  if(abs(cor(df$SalePrice,df[,i])) > .3){
    plot(df[,i],df$SalePrice,main=paste(i,cor(df$SalePrice,df[,i])))
    imp_num_vars = c(imp_num_vars,i)
    #readline(prompt="Press [enter] to continue")
  }
}

imp_cat_vars = c()
dff <- df[df$SalePrice < 300000,]
#dff<-df
for(i in catvars){
    means <- aggregate(as.formula(paste("SalePrice ~ ",i)),dff, mean)
    maxp = max(means$SalePrice)
    minp = min(means$SalePrice)
    if(maxp - minp > 75000){
      boxplot(dff$SalePrice ~ dff[,i],main=paste(i,round(maxp - minp)))
      imp_cat_vars = c(imp_cat_vars,i)
      #readline(prompt="Press [enter] to continue")
    }else{
      #print(i)
    }
}

#imp_num_vars

#imp_cat_vars
library(fastDummies)
results <- dummy_cols(df[,imp_cat_vars], remove_first_dummy = TRUE,
                      remove_selected_columns=TRUE)

results$SalePrice = df$SalePrice
results$`RoofMatl_Tar&Grv`

names(results) <- make.names(names(results))


imp_cat_vars=c()
for(i in names(results)){
  if(length(unique(results[,i]))==2){
    means <- aggregate(as.formula(paste("SalePrice ~ ",i)),results, mean)
    maxp = max(means$SalePrice)
    minp = min(means$SalePrice)
    #print(maxp)
    #print(minp)
    
    if(maxp - minp > 40000){
      boxplot(results$SalePrice ~ results[,i],main=paste(i,round(maxp - minp)))
      imp_cat_vars = c(imp_cat_vars,i)
      #print(length(imp_cat_vars))
      #readline(prompt="Press [enter] to continue")
    }else{
      print(i)
    }    
  }
}
imp_cat_vars
results = results[,c(imp_cat_vars,"SalePrice")]
results = cbind(df[,imp_num_vars],results)


write.csv(results,"data_important.csv",row.names = F)
names(results)
