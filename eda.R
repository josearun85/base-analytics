setwd("C:/Users/arun.j/Desktop/Class/log_reg")
library(foreign)
master<-read.arff("5year.arff")
names(master) <- c("netprof_totasst","totliab_totasst","workcap_totasst","curasst_shtrmliab","cash_sec_rec_liab__opexp_dep","retainear_totasst","ebit_totasst","ookeqt_totliab","sales_totasst","eqt_totasst","grsprof_extritm_finexp__totasst","grsprof_shtrmliab","grsprof_dep__sales","grsprof_intrst__totast","totliab365__grsprof_dep","grsprof_dep__totliab","totasst_totliab","grsprof_totasst","grsprof_sales","inv365_sales","salesn_salesnminus1","pofoperact_totasst","netprof_sales","grspros3yrs_totasst","eqt_shrcap__totasst","net_prof_dep__totliab","profoperact_finexp","workcap_fixasst","logtotasst","totliab_cash__sales","grsprod_inter__sales","curliab365_costprdsold","operexp_shtrmliab","operexp_totliab","profsales_totasst","totales_totasst","curasst_invt__lngtrmliab","constcap_totasst","profsales_sales","currasst_inv_recv__shtrmliab","totliab__profoperact_dep_12_365","profoperactsales","rotrecplusinvturnoverdays","recv365_sales","netprof_invt","curasst_invt__shtrmliab","inv365_costprdsold","ebitda_totasst","ebitda_sales","curasst_totliab","shtrmliab_totasst","shtrmliab365_costprodsold","eqt_fixasst","constcap_fixasst","workcap","sales_costprod__sales","curasst_invshtrmliab__sales_grsprof_dep","totcost_totsales","lngtrmliab_eqt","sales_inv","sales_recv","shtrmliab365_sales","sales_shtrmliab","sales_fixasst","bankruptcy")


hist(log(master[,2]),breaks=50)

fn = function(x){
  return (x+1-min(x,na.rm=T))
}
master_log <- as.data.frame(apply(master[,-65], 2, fn))
master_log$bankruptcy <- master$bankruptcy

# Generating plots for visual analysis
for(i in 1:(ncol(master)-1)){
  png(filename=paste0("images/",names(master)[i],".png"))
  boxplot(master_log[,i] ~ master_log$bankruptcy,main=names(master_log)[i])
  dev.off()
}


# Tests
t.test(master_log$workcap_totasst ~ master_log$bankruptcy)
t.test(master_log$profoperactsales ~ master_log$bankruptcy) # Not sig
t.test(master_log$profoperact_finexp ~ master_log$bankruptcy)
t.test(master_log$logtotasst ~ master_log$bankruptcy)
t.test(master_log$netprof_sales ~ master_log$bankruptcy) # not sig
t.test(master_log$operexp_shtrmliab ~ master_log$bankruptcy) # not sig
t.test(master_log$profoperact_finexp ~ master_log$bankruptcy) 
t.test(master_log$sales_totasst ~ master_log$bankruptcy) 
t.test(master_log$curasst_invt__lngtrmliab ~ master_log$bankruptcy) 
t.test(master_log$eqt_fixasst ~ master_log$bankruptcy) # not sig
t.test(master_log$profsales_sales ~ master_log$bankruptcy)
t.test(master_log$rotrecplusinvturnoverdays ~ master_log$bankruptcy) 
t.test(master_log$retainear_totasst ~ master_log$bankruptcy) # not sig
t.test(master_log$recv365_sales ~ master_log$bankruptcy)

