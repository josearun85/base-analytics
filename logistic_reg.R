setwd("D:/base-analytics-master/data")
flor <- read.csv("florence.csv")
florence <- flor
library(popbio)

# Does M affect Florence purchase?

t.test(florence$M ~ florence$Florence)

# Visual
logi.hist.plot(florence$M, florence$Florence)
logi.hist.plot(florence$M, florence$Florence,boxp = F)
logi.hist.plot(florence$M, florence$Florence,boxp = F,type = "hist")


fit <- glm(Florence ~ M + ArtBks, data=flor, family = binomial())

summary(fit)


flor$pred <- predict(fit,type = "response") 
flor$florpurch <- ifelse(flor$pred > 0.1, 1, 0) 

table(flor$Florence)
table(FL=flor$Florence,PR=flor$florpurch)




library(InformationValue)
somersD(flor$Florence,flor$pred)

library(ROCR)
pred <- prediction(flor$pred,flor$Florence)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

auc = performance(pred, measure = "auc")
auc@y.values[[1]]

dist <- rep(9999,length(roc.perf@x.values[[1]]))
for(i in 1:length(roc.perf@x.values[[1]])){
  cur_x <- roc.perf@x.values[[1]][i]
  cur_y <- roc.perf@y.values[[1]][i]
  #cur_alpha <- roc.perf@alpha.values[[1]][i]
  dist[i] <- (0 - cur_x) * (0 - cur_x) + (1 - cur_y)*(1 - cur_y) 
}

ideal_cutoff <- roc.perf@alpha.values[[1]][dist==min(dist)]
ideal_cutoff



library(ResourceSelection)
hoslem.test(flor$Florence,flor$pred)


logitgof
library(MASS)
fit2 <- glm(Florence ~ M + ArtBks + F + GeogBks + CookBks+Recency + YouthBks, data=flor, family = binomial())
stepAIC(fit2)


pairs(flor[,3:5])

