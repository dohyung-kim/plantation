#  Okay, now I am going to analyze the Hansen/ALOS/Sentinel1A data, matched to training by spatial join (see spatSelectTraining for pass4v3).

#  goal today is to run AML models on each of the 5 data subsets.
#  load needed libraries and functions

#  part 2 of this code simply runs only deep learning models, and give them as much time as possible.  
#  pass43c v2 of this code uses balance set 2, not 0 or 1.
#  pass4v3d uses balance set 3

rm(list=ls())
library(data.table)
library(h2o)
rm(list=ls())
#set.seed(41234)
setwd("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3")
outdir="step1pred_output3"
versN="p4v3"
# version b fixes Latin America error
inDir=paste0(getwd())
outDir=paste0(getwd(), "\\", outdir)
#outDir=paste0(getwd(), "\\", outdir, "\\extraDL")

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
#h2o.init(nthreads = 1)



h2o.init(nthreads = 31, min_mem_size="45g")


###################################################################################
## Start model run

#train_data_fold1.csv
i=1
#for(i in 1:11){
modDir=paste0(outDir, "\\fold", i)
test.h2o <- as.h2o(test_data)
proj_name=paste0("amlAll_final_", versN, "_", i)
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

h2o.levels(trainU.h2o["LCtype2"])
############################################
##########  now to run autoML

ui=i+1 #  first run, seventh seed, etc.
print(i)
aml.U <- h2o.automl(y=y.dep, x=x.indep, training_frame = trainU.h2o, fold_column="cvID", max_runtime_secs=7200, seed=seeds[ui], project_name=proj_name)
aml.U
print(i)

tester=aml.U@leaderboard
head(tester, 60)
outUa=paste0(outDir, "\\fold", i, "\\leaderboard_models_", i, ".csv")
write.csv(as.data.frame(tester), outUa)

model_ids <- as.data.frame(aml.U@leaderboard$model_id)[,1]
model_ids

#############################################################################
#  save and predict the second and third model: best non-ensemble, and best of family model:
# 
# StackedEnsemble_BestOfFamily
bofname=grep("StackedEnsemble_BestOfFamily", model_ids, value = TRUE)[1]
ensname=grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1]
gbmname=grep("GBM", model_ids, value = TRUE)[1]
glmname=grep("GLM", model_ids, value = TRUE)[1]
drfname=grep("DRF", model_ids, value = TRUE)[1]
dlgname=grep("DeepLearning", model_ids, value = TRUE)[1]
xrtname=grep("XRT", model_ids, value = TRUE)[1]

#ens_mat=match(c(bofname,ensname),model_ids)
bestmod=model_ids[! model_ids %in% c(bofname,ensname)]
name1=bestmod[1]


#  predict with Best of Family
m_bof1 <- h2o.getModel(bofname)
predict.amlU <- as.data.frame(h2o.predict(m_bof1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1bof1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save BOF

h2o.saveModel(object=m_bof1, path = modDir, force=TRUE)

#  Now do the same with best non-ensemble model (GBM).
print(gbmname)
print(name1) 
m_best1 <- h2o.getModel(gbmname)
predict.amlU <- as.data.frame(h2o.predict(m_best1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1best1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save BOF
h2o.saveModel(object=m_best1, path = modDir, force=TRUE)

#

#  predict with GLM
m_glm1 <- h2o.getModel(glmname)
predict.amlU <- as.data.frame(h2o.predict(m_glm1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1glm1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save glm
#modDir=paste0(outDir, "\\fold", i)
##dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
h2o.saveModel(object=m_glm1, path = modDir, force=TRUE)

#
#  predict with DRF
m_drf1 <- h2o.getModel(drfname)
predict.amlU <- as.data.frame(h2o.predict(m_drf1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1drf1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save drf
#modDir=paste0("H:\\DHK_globalRefor\\tree plantations pass2_mergedArea\\", outdir, "\\fold", i)
##dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
h2o.saveModel(object=m_drf1, path = modDir, force=TRUE)

#
#  predict with XRT
m_xrt1 <- h2o.getModel(xrtname)
predict.amlU <- as.data.frame(h2o.predict(m_xrt1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1xrt1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save xrt
#modDir=paste0("H:\\DHK_globalRefor\\tree plantations pass2_mergedArea\\", outdir, "\\fold", i)
##dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
h2o.saveModel(object=m_xrt1, path = modDir, force=TRUE)

#
#  predict with DeepLearning
m_dl1 <- h2o.getModel(dlgname)
predict.amlU <- as.data.frame(h2o.predict(m_dl1, test.h2o))
predict.amlU=data.frame(test_data, predict.amlU)
#  save the predicted data frames from h2o
outU=paste0(outDir, "\\fold", i, "\\predict_pass", versN, "_AML1dl1_", i, ".csv")
write.csv(predict.amlU, outU)
#  now save dl
##modDir=paste0("H:\\DHK_globalRefor\\tree plantations pass2_mergedArea\\", outdir, "\\fold", i)
#dir.create(modDir, showWarnings = FALSE, recursive = TRUE)
h2o.saveModel(object=m_dl1, path = modDir, force=TRUE)

#

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