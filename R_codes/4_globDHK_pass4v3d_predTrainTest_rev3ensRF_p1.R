#  Time to predict:
# final uses an 11-fold split (balanced differently) of teh expanded training data
#  pass4 v3 final joins with additional information from a point file
#  _gbm run uses a GBM from automML
#  _dl used a DL-only autoML to create good DL prediction
#  _rf used the RF from autoML
#  _glm used the GLM from autoML to look at just shapefile number 5, for comparison among all available model types.  

#  load needed libraries and functions.
rm(list=ls())
library(data.table)
library(sf)
library(h2o)
library(plyr)
library(dplyr)
h2o.init(nthreads = 30)

typers=c("gbm", "glm", "rf", "dl", "xrt")
TYPERS=c("GBM", "GLM", "RF", "DL", "XRT")
typegrep=c("GBM", "GLM", "DRF", "DeepLearning", "XRT")
pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3"
inDir2="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\case_data\\plantation_ALL_2015_v3"
#train_data=fread(paste0(inDir, "\\train_data_Range9_", versN, "_1", ".csv"))
#rm(train_data)
#  now load up joining data
##  Next, add in information on new metrics (v9)
newdata=fread(paste0(inDir2, "//mergeArea_v3pass4_ptsALL_DISTNearest_pass42015v4.csv"))
newdata2=subset(newdata, select=c("INDEX3","shparea", "shplen",   "shpcmpt", "shprat",  "distR_km", "distC_km", "continent", "X", "Y"))
rm(newdata)

#  now load up an example tile:
#tilex=st_read(paste0(inDir2, "\\subset_size\\join30\\", "joinALL_gain_merge30.gpkg"))
#rm(tilex)

#  after looking at the data, looks like all I need to do is merge the tile with the newdata2 by INDEX3.

#ya=5
for(ya in 1:length(typers)){
#for(ya in c(1:2, 4)){
typerun=typers[ya]
TYPERUN=TYPERS[ya]
grepIN=typegrep[ya]
if(typerun=="dl") {
inDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\extraDL")
} else {
inDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3")
}


outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\", typerun)
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
#  data in seeds
# seeds=read.csv("H:\\DHK_globalRefor\\tree plantations pass2_mergedArea\\seeds_run50.csv", header=TRUE, row.names=1)
# seeds=seeds[,1]

#  open session
#i=30
for(i in 1:30){
print("start")
print(i)
in_name=paste0(inDir2, "\\subset_size\\join30\\", "joinALL_gain_merge", i, ".gpkg")
data1<-st_read(in_name)
data1_df <- as.data.table(data1 %>% st_set_geometry(NULL))
rm(data1)

dimcol=ncol(data1_df)
selcol=names(data1_df)[c(3,1:2,4:dimcol)]
data1_df=data1_df[,selcol, with=FALSE]
#  plant_sel2b, reg_sel are my data to join
merge_test=left_join(data1_df, newdata2, by="INDEX3")
#  okay it worked; let's overwrite old values and rm old variables
data1_df=merge_test; rm(merge_test)
data1_df$continent=factor(data1_df$continent)


#  deal with missing columns:  "plant_type"  "source2"     "type2"       "cvID"        "LCtype"      "LCtype2" 
#  check data order, etc.  
data1_df$plant_type=rep("bob", nrow(data1_df))
data1_df$source2=factor(rep("bob", nrow(data1_df)))
data1_df$type2=rep("bob", nrow(data1_df))
data1_df$cvID=rep(1, nrow(data1_df))
data1_df$LCtype=factor(rep("bob", nrow(data1_df)))
#data1_df$LCtype2=factor(rep("bob", nrow(data1_df)))
names(data1_df)

#  get rid of NA values:
data1_dfsel=data1_df[complete.cases(data1_df),]
data1_dfNA=data1_df[!complete.cases(data1_df),]
rm(data1_df)
#  see if I can just load up h2o min, and load up the models, and predict in R!


#  predict testing
test.h2o <- as.h2o(data1_dfsel)
ind_list=NULL
#train_data_fold1.csv
#j=1
for(j in 1:11){

#  first load model:
#
# bofname=grep("StackedEnsemble_BestOfFamily", model_ids, value = TRUE)[1]
# ensname=grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1]
# gbmname=grep("GBM", model_ids, value = TRUE)[1]

mod_path=paste0(inDir, "\\fold", j) 
listdir=list.files(path = mod_path, full.names=TRUE)
gbm_in=grep(grepIN, listdir, value = TRUE)[1]
#bof_in=grep("StackedEnsemble_BestOfFamily", listdir, value = TRUE)[1]

m_gbm1=h2o.loadModel(gbm_in)
#m_bof1=h2o.loadModel(bof_in)
mod1=paste0(TYPERUN,"_", j)
#mod2=paste0("BOF_", j)

predict.gbm2 <- as.data.table(h2o.predict(m_gbm1, test.h2o))
#predict.bof <- as.data.table(h2o.predict(m_bof1, test.h2o))
if(j==1){
predict.out=data.table(data1_dfsel, predict.gbm2)
} else {
predict.out=data.table(predict.out, predict.gbm2)
}
nam1=names(predict.out)
rep1=paste0(nam1[(length(nam1)-2):length(nam1)], mod1)
names(predict.out)[(length(nam1)-2):length(nam1)]<-rep1
ind_list=c(ind_list, length(nam1)-2)
print(nam1[ind_list])
#
h2o.rm(m_gbm1)
rm(mod1, mod_path, nam1, rep1, predict.gbm2)
}
############
print("done predicting")
##############################

fwrite(predict.out, paste0(outDir, "\\Pred",TYPERUN, "_Fin", versN, "_", i, "_data.csv"))

print("done")
print(i)

#
h2o.removeAll()

}
#
#
#
}
#
h2o.shutdown(prompt=FALSE)	
#		