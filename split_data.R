
subset <- master_log[,c("workcap_totasst","profoperact_finexp","logtotasst","curasst_invt__lngtrmliab",
                    "sales_totasst","profsales_sales","rotrecplusinvturnoverdays","retainear_totasst","recv365_sales","bankruptcy")]
subset <- subset[complete.cases(subset), ]


spec = c(train = .4, test = .3, validate = .3)

g = sample(cut(
  seq(nrow(subset)), 
  nrow(subset)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(subset, g)


write.csv(res$train,"train.csv",row.names = F)
write.csv(res$test,"test.csv",row.names = F)
write.csv(res$validate,"validate.csv",row.names = F)
