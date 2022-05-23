#  Okay, now I am going to analyze the Hansen/ALOS/Sentinel1A data, matched to training by spatial join (see spatSelectTraining for pass4v3).
#  version b just fixes lat am error--no change to this code.
#  goal today is to run AML models on each of the 5 data subsets.
#  load needed libraries and functions
#  now on version 3d as added data in africa (c) and asia (d).

#  part 2 of this code simply runs only deep learning models, and give them as much time as possible.  
#  v2 of this code uses balance set 1, not 0.

rm(list=ls())
library(data.table)
library(h2o)
rm(list=ls())
#set.seed(41234)
setwd("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3")
outdir="step1pred_output3"
versN="p4v3"

inDir=paste0(getwd())
#outDir=paste0(getwd(), "\\", outdir)
outDir=paste0(getwd(), "\\", outdir, "\\extraDL")

# On first run, create directories for output
for(i in 1:11){
modDir=paste0(outDir, "\\fold", i)
dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
}


#  data in (test)
test_data=fread(paste0("test_data_", versN, "_v2.csv"))
seeds=read.csv(paste0(getwd(), "/seeds_run50.csv"), header=TRUE, row.names=1)
seeds=seeds[,1]
#  open session
h2o.init(nthreads = 31, min_mem_size="45g")


###################################################################################
## Start model run

#train_data_fold1.csv
#i=1
for(i in 1:11){
modDir=paste0(outDir, "\\fold", i)
test.h2o <- as.h2o(test_data)
proj_name=paste0("amlDL_final_", versN, "_", i)
train_data=fread(paste0(inDir, "\\train_data_Range9_", versN, "_", i, ".csv"))
trainU.h2o <- as.h2o(train_data)
colnames(trainU.h2o)

#  set as classification#dependent variable (Purchase)
y.dep <- 51
#independent variables (dropping ID variables)
x.indep <- c(8:9, 11:43)
trainU.h2o[,"continent"] <- as.factor(trainU.h2o[,"continent"])
trainU.h2o[,"LCtype"] <- as.factor(trainU.h2o[,"LCtype"])
trainU.h2o[,"LCtype2"] <- as.factor(trainU.h2o[,"LCtype2"])
trainU.h2o[,"source2"] <- as.factor(trainU.h2o[,"source2"])


############################################
##########  now to run autoML

ui=i+1 #  first run, seventh seed, etc.
print(i)
aml.U <- h2o.automl(y=y.dep, x=x.indep, training_frame = trainU.h2o, fold_column="cvID", max_runtime_secs=10800, exclude_algos = c("GLM",
"DRF", "GBM"), seed=seeds[ui], project_name=proj_name)
aml.U
print(i)

tester=aml.U@leaderboard
head(tester, 60)
outUa=paste0(outDir, "\\fold", i, "\\leaderboard_DLmodels_", i, ".csv")
write.csv(as.data.frame(tester), outUa)

model_ids <- as.data.frame(aml.U@leaderboard$model_id)[,1]
model_ids

#############################################################################
#  save and predict the second and third model: best non-ensemble, and best of family model:
# 
# StackedEnsemble_BestOfFamily
bofname=grep("StackedEnsemble_BestOfFamily", model_ids, value = TRUE)[1]
ensname=grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1]
# gbmname=grep("GBM", model_ids, value = TRUE)[1]
# glmname=grep("GLM", model_ids, value = TRUE)[1]
# drfname=grep("DRF", model_ids, value = TRUE)[1]
# dlgname=grep("DeepLearning", model_ids, value = TRUE)[1]
# xrtname=grep("XRT", model_ids, value = TRUE)[1]

#ens_mat=match(c(bofname,ensname),model_ids)
bestmod=model_ids[! model_ids %in% c(bofname,ensname)]
name1=bestmod[1]

#  predict with Best Deep Learning model
m_dl1 <- h2o.getModel(name1)
predict.amlU <- as.data.frame(h2o.predict(m_dl1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "final_AML1dl1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save dl
#modDir=paste0(outDir, "\\fold", i)
##dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
h2o.saveModel(object=m_dl1, path = modDir, force=TRUE)
#

# #  the following code is for getting additional models saved in a subdirectory, so I can compare how different DL models do within a fold.  
# outDir2=paste0(outDir, "\\fold", i, "\\testmods")
# dir.create(outDir2, showWarnings = FALSE, recursive = TRUE)
# mod_list=model_ids[1:7]
# for(k in 1:7){
# m_dl2 <- h2o.getModel(mod_list[k])
# predict.amlU2 <- as.data.frame(h2o.predict(m_dl2, test.h2o))
# predict.amlU2=data.frame(test_data, predict.amlU2)
# #  save the predicted data frames from h2o
# outU2=paste0(outDir2, "\\predict_pass7final_AML1dl", k, "_fold", i, ".csv")
# write.csv(predict.amlU2, outU2)
# #  now save dl
# h2o.saveModel(object=m_dl2, path = outDir2, force=TRUE)
# #
# }
# #


#  Now to clean up for the next model run.
suppressWarnings(rm(aml.U, predict.amlU, predict.amlU2, outU, outU2, proj_name, train_data, tester, bestmod, name1, bofname, gbmname, ensname, trainU.h2o, m_dl1))
h2o.removeAll()
print(i)
}
#

#

#
h2o.shutdown(prompt=FALSE)	
#		