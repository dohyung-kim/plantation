#  Time to predict:
# final uses an 11-fold split (balanced differently) of teh expanded training data
#  pass4 v3 final joins with additional information from a point file
#  _gbm run uses a GBM from automML
#  _dl used a DL-only autoML to create good DL prediction
#  _rf used the RF from autoML
#  _glm used the GLM from autoML 

#  p1 ran just the model predictions, but be prepared that you may need to first filter out NA data in the polygons.  You can fill it back in with -999.
#  p2 takes all the predictions from p1, and saves just the predictions in one file (plantation prob), and the prediction votes (LC class) in another.  
#  p3 runs the RF ensemble prediction for each continent: it First checks purity (one continent per tile) to make sure this is okay by tile.


#  load needed libraries and functions.
rm(list=ls())
library(data.table)
#library(sf)
library(h2o)
library(plyr)
library(dplyr)
#h2o.init(nthreads = 20)

pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble"
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

h2o.init(nthreads = 31, min_mem_size="45g")
# CHANGE EACH TIME:
in_africa=paste0(inDir, "\\rforest_pass4v3_bal3try1_3391_30vAf")
in_asia=paste0(inDir, "\\rforest_pass4v3_bal3try1_9793_31vAs")
in_latam=paste0(inDir, "\\rforest_pass4v3_bal3try1_8902_32vLa")
#  let's try loading multiple h2o models, then predicting from each using if statements and nrow counts of subsets in each continent.

mod_afr=h2o.loadModel(in_africa)
mod_asa=h2o.loadModel(in_asia)
mod_lam=h2o.loadModel(in_latam)


# now let's get started!
#i=1
for(i in 1:30){
print("start")
print(i)

in_name=paste0(inDir, "\\PredEnsALL", "_Fin", versN, "_", i, "_dataPred.csv")
data1<-fread(in_name)
names(data1)
#names(data1) <- gsub(x = names(data1), pattern = "best", replacement = "gbm") 
#names(data1) <- gsub(x = names(data1), pattern = "drf", replacement = "rf")
names(data1) 
#  now let's get down to continent 
data_latam=NULL
#  Latin_America
if(sum(data1$continent=="Latin_America")>0) {
data_latam=data1[data1$continent=="Latin_America"]
}
#
if(!is.null(data_latam)){
test.latam <- as.h2o(data_latam)
# "Latin_America" "Africa" "Asia"
predict.lam <- as.data.table(h2o.predict(mod_lam, test.latam))
pred.lam=data.table(data_latam, predict.lam)
} else {
pred.lam=NULL
}

#  asia:
data_asia=NULL
if(sum(data1$continent=="Asia")>0) {
data_asia=data1[data1$continent=="Asia"]
}
#
if(!is.null(data_asia)){
test.asia <- as.h2o(data_asia)
# "Latin_America" "Africa" "Asia"
predict.asa <- as.data.table(h2o.predict(mod_asa, test.asia))
pred.asa=data.table(data_asia, predict.asa)
}  else {
pred.asa=NULL
}

#  Africa:
data_afr=NULL
if(sum(data1$continent=="Africa")>0) {
data_afr=data1[data1$continent=="Africa"]
}
#
if(!is.null(data_afr)){
test.afr <- as.h2o(data_afr)
# "Latin_America" "Africa" "Asia"
predict.afr <- as.data.table(h2o.predict(mod_afr, test.afr))
pred.afr=data.table(data_afr, predict.afr)
} else {
pred.afr=NULL
}

#  bind and write out the result:
predict.out=bind_rows(pred.afr, pred.asa, pred.lam)
#  Do I want to add anything, like variability in vote?
#
#  I did on the first run, but will remove and replace in p3b.
predict.out$predict2=with(predict.out, ifelse(regrowth>=0.5, "regrowth", "plantation"))
predict.out$prob.RF=with(predict.out, ifelse(predict2=="regrowth", regrowth, plantation))
predict.out$conf.RF=with(predict.out, ifelse(predict2=="regrowth", (regrowth-0.5)/0.5, (plantation-0.5)/0.5))
data.vote=predict.out[,3:57]
data.vote2=data.vote>=0.5
data.vsum=apply(data.vote2, 1, sum)
data.conv=ifelse(data.vsum>27.5, (data.vsum-27)/28, (28-data.vsum)/28)
predict.out$vote.mean=apply(data.vote, 1, mean)
predict.out$vote.sd=apply(data.vote, 1, sd)
predict.out$vote.class=ifelse(predict.out$vote.mean>=0.5, "plantation", "regrowth")
predict.out$vote.mod=data.vsum
predict.out$vote.conf=data.conv
#
out_name=paste0(outDir, "\\Pred_FinEnsRF", "_", i, "_predRF.csv")
fwrite(predict.out, out_name)

suppressWarnings(rm(predict.out, data.vote, data.vote2, data.vsum, data.conv, pred.afr, pred.asa, pred.lam, predict.afr, predict.asa, predict.lam, test.latam, test.asia, test.afr, data_latam, data_asia, data_afr))

}
#

#
h2o.removeAll()
h2o.shutdown(prompt=FALSE)	










# #  now to subset and join to other files:
# data2=data1[, c(1,43, seq(52,83, 3))]
# data_lab=data1[, c(1,43, seq(51,83, 3))]
# col_nam=paste0(grepIN, "_", 1:11, "plant")
# names(data2)=c("INDEX3", "continent", col_nam)
# if(ya==1){
# data_out=data2
# datalab_out=data_lab
# } else {
# data_out=cbind(data_out, data2[,3:13])
# datalab_out=cbind(datalab_out, data_lab[,3:13])
# }
# #
# fwrite(data_out, paste0(outDir, "\\PredEnsALL", "_Fin", versN, "_", i, "_dataPred.csv"))
# fwrite(datalab_out, paste0(outDir, "\\PredEnsALL", "_Fin", versN, "_", i, "_dataLabels.csv"))
# print(typerun)
# }
# print(paste0(i, " all done"))
# ############
# print("done predicting")
# ##############################
# }	
# #	check wierd 0.5 thing with the INDEX3 variables.
