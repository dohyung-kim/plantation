# let's try that again, and create a dataset to predict on:
library(data.table)
library(h2o)
rm(list=ls())

inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble"

train_data=fread(paste0(inDir, "\\allpredEns_bal3__step1pred3.csv"))
setwd("H:/DHK_globalRefor/tp_pass4_2019/2015v3/training_data/traindata_bal3/")

seeds=read.csv("H:/DHK_globalRefor/tp_pass4_2019/2015v3/training_data/traindata_bal3/seeds_run50.csv", header=TRUE, row.names=1)
seeds=seeds[,1]
#  subset into three sets of training data, by continent.
train_africa=train_data[continent=="Africa"]
train_asia=train_data[continent=="Asia"]
train_latin=train_data[continent=="Latin_America"]

#  now to start h2o
h2o.init(nthreads = 20, min_mem_size="30g")
####################  model time!!

######### vAf
vers="vAf"
  # start with Africa because it's most important.
trainU.h2o <- as.h2o(train_africa)
colnames(trainU.h2o)
#  set as classification#dependent variable (LCtype2)
y.dep <- 3
colnames(trainU.h2o)[3]
#independent variables (dropping ID variables)
x.indep <- c(4:58)
colnames(trainU.h2o)[4:58]
trainU.h2o[,"LCtype2"] <- as.factor(trainU.h2o[,"LCtype2"])


# randomForest
i=30
modID=paste0("rforest_pass4v3_bal3try1_", seeds[i], "_", i, vers)
rforest.modelD <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainU.h2o, ntrees = 1000, mtries = -1, max_depth = 20, seed = seeds[i], balance_classes=FALSE, binomial_double_trees=TRUE, model_id=modID)
#
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink(paste0(inDir, "\\model_perfRF_", vers, ".txt"))
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink()

h2o.saveModel(object=rforest.modelD, path = inDir, force=TRUE)
#  save the predicted data frames from h2o
#outD=paste0(inDir, "predicted_RF1_v4rough_", i, ".csv")
#write.csv(predict.rforestD, outD)
h2o.rm(rforest.modelD)
rm(trainU.h2o, modID)
#}
############


######### vAs
vers="vAs"
  # next Asia
trainU.h2o <- as.h2o(train_asia)
colnames(trainU.h2o)
#  set as classification#dependent variable (LCtype2)
y.dep <- 3
#independent variables (dropping ID variables)
x.indep <- c(4:58)
trainU.h2o[,"LCtype2"] <- as.factor(trainU.h2o[,"LCtype2"])


# randomForest
i=31
modID=paste0("rforest_pass4v3_bal3try1_", seeds[i], "_", i, vers)
rforest.modelD <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainU.h2o, ntrees = 1000, mtries = -1, max_depth = 20, seed = seeds[i], balance_classes=FALSE, binomial_double_trees=TRUE, model_id=modID)
#
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink(paste0(inDir, "\\model_perfRF_", vers, ".txt"))
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink()

h2o.saveModel(object=rforest.modelD, path = inDir, force=TRUE)
#  save the predicted data frames from h2o
#outD=paste0(inDir, "predicted_RF1_v4rough_", i, ".csv")
#write.csv(predict.rforestD, outD)
h2o.rm(rforest.modelD)
rm(trainU.h2o, modID)
#}
########


######### vLa
vers="vLa"
  # latin america
trainU.h2o <- as.h2o(train_latin)
colnames(trainU.h2o)
#  set as classification#dependent variable (LCtype2)
y.dep <- 3
#independent variables (dropping ID variables)
x.indep <- c(4:58)
trainU.h2o[,"LCtype2"] <- as.factor(trainU.h2o[,"LCtype2"])


# randomForest
i=32
modID=paste0("rforest_pass4v3_bal3try1_", seeds[i], "_", i, vers)
rforest.modelD <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainU.h2o, ntrees = 1000, mtries = -1, max_depth = 20, seed = seeds[i], balance_classes=FALSE, binomial_double_trees=TRUE, model_id=modID)
#
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink(paste0(inDir, "\\model_perfRF_", vers, ".txt"))
h2o.performance(rforest.modelD)
h2o.varimp(rforest.modelD)
sink()

h2o.saveModel(object=rforest.modelD, path = inDir, force=TRUE)
#  save the predicted data frames from h2o
#outD=paste0(inDir, "predicted_RF1_v4rough_", i, ".csv")
#write.csv(predict.rforestD, outD)
h2o.rm(rforest.modelD)
rm(trainU.h2o, modID)
#}
########

#
h2o.removeAll()
#
h2o.shutdown(prompt=FALSE)	
#		