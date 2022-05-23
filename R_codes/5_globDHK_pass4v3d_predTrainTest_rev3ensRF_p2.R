#  Time to predict:
# final uses an 11-fold split (balanced differently) of teh expanded training data
#  pass4 v3 final joins with additional information from a point file
#  _gbm run uses a GBM from automML
#  _dl used a DL-only autoML to create good DL prediction
#  _rf used the RF from autoML
#  _glm used the GLM from autoML to look at just shapefile number 5, for comparison among all available model types.  

#  p2 takes all the predictions from p1, and saves just the predictions in one file (plantation prob), and the prediction votes (LC class) in another.  
#pass4v3c uses corrrected Africa data, and 3d the corrected asia as well.
#  load needed libraries and functions.
rm(list=ls())
library(data.table)
#library(sf)
#library(h2o)
library(plyr)
library(dplyr)
#h2o.init(nthreads = 20)

typers=c("gbm", "xrt", "rf","glm",  "dl")
TYPERS=c("GBM", "XRT", "RF","GLM",  "DL")
typegrep=c("best", "xrt", "drf","glm", "dl")
pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3"
inDir2="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\case_data\\plantation_ALL_2015_v3"

#  open session
#i=30
for(i in 1:30){
print("start")
print(i)

#ya=1
for(ya in 1:5){
#for(ya in 1:length(typers)){
#for(ya in c(1:2, 4)){
typerun=typers[ya]
TYPERUN=TYPERS[ya]
grepIN=typegrep[ya]

inDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\", typerun)

outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\", "ensemble")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

in_name=paste0(inDir, "\\Pred",TYPERUN, "_Fin", versN, "_", i, "_data.csv")
data1<-fread(in_name)

#  now to subset and join to other files:
data2=data1[, c(1,43, seq(52,83, 3))]
data_lab=data1[, c(1,43, seq(51,83, 3))]
col_nam=paste0(grepIN, "_", 1:11, "plant")
names(data2)=c("INDEX3", "continent", col_nam)
if(ya==1){
data_out=data2
datalab_out=data_lab
} else {
data_out=cbind(data_out, data2[,3:13])
datalab_out=cbind(datalab_out, data_lab[,3:13])
}
#
fwrite(data_out, paste0(outDir, "\\PredEnsALL", "_Fin", versN, "_", i, "_dataPred.csv"))
fwrite(datalab_out, paste0(outDir, "\\PredEnsALL", "_Fin", versN, "_", i, "_dataLabels.csv"))
print(typerun)
}
print(paste0(i, " all done"))
############
print("done predicting")
##############################
}	
#