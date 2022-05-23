#  step 3b fixes the attempts at summarizing the output of step 3, counting votes differently, etc.  
#  the fixed version of this step does a better job summarizing uncertainty.  
#  load needed libraries and functions.
rm(list=ls())
library(data.table)
#library(sf)
#library(h2o)
library(plyr)
library(dplyr)
#h2o.init(nthreads = 20)

pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble"
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1")

#  open the weights vectors for later
we_la=read.csv(paste0(inDir, "\\weights_testDataAcc_LatAm.csv"))
we_as=read.csv(paste0(inDir, "\\weights_testDataAcc_Asia.csv"))
we_af=read.csv(paste0(inDir, "\\weights_testDataAcc_Africa.csv"))
wt_la=as.numeric(we_la[,2])/sum(we_la[,2])
wt_as=as.numeric(we_as[,2])/sum(we_as[,2])
wt_af=as.numeric(we_af[,2])/sum(we_af[,2])


#  okay let's do this:
#i=12
for(i in 1:30){
print("start")
print(i)
in_name=paste0(outDir, "\\Pred_FinEnsRF", "_", i, "_predRF.csv")

pred.out=fread(in_name)
predict.out=pred.out[,1:60]
  
predict.out$predict.Af=predict.out$predict
predict.out$prob.RF=with(predict.out, ifelse(predict.Af=="regrowth", regrowth, plantation))
data.vote=predict.out[,3:57]
data.vote2=data.vote>=0.5
data.vsumP=apply(data.vote2, 1, sum)
data.vsumR=55-data.vsumP
predict.out$vote.Ens=pmax(data.vsumP, data.vsumR)/55
predict.out$maj.Ens=ifelse(data.vsumP>data.vsumR, "plantation", "regrowth")
#  now for some weighted action:
fun_wtla=function(x){weighted.mean(x, wt_la)}
fun_wtas=function(x){weighted.mean(x, wt_as)}
fun_wtaf=function(x){weighted.mean(x, wt_af)}

data.wtLa=apply(data.vote, 1, fun_wtla)
data.wtAs=apply(data.vote, 1, fun_wtas)
data.wtAf=apply(data.vote, 1, fun_wtaf)
data.meanP=apply(data.vote2, 1, mean)

predict.out$wt.Mean=with(predict.out, ifelse(continent=="Latin_America", data.wtLa, ifelse(continent=="Asia", data.wtAs, ifelse(continent=="Africa", data.wtAf, -999))))
predict.out$wt.Pred=ifelse(predict.out$wt.Mean>=0.5, "plantation", "regrowth")

#  let's wrap it all up in a bow:
predict.out$conf.all=with(predict.out, ifelse((predict.Af!=wt.Pred | predict.Af!=maj.Ens | prob.RF<=0.6 | vote.Ens<=0.6), "Low", "High"))


out_name=paste0(outDir, "\\Pred_FinEnsRF", "_", i, "_predRF2.csv")
fwrite(predict.out, out_name)
}
#