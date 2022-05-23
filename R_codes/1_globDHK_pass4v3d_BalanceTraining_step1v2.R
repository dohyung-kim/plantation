#  NOTES:
#  v2 of this code is identical to v1, except that it creates 11 different balancings of the data, with regrowth as 10% of the total data to 90%.  Skip to lines 35 and then 234 and on for changes.  

#  What I am going to do here is generate a training data set that is balanced a few different ways.  
#  I am going to start with step1, balanced equally through upsampling if needed.  Regrowth 50%, oil palm 25%, and other plantation 25%.  Then I am going to do a five-fold cross CV.  By default, training data will be concentrated in southern Brazil and SE Asia.  
#  A future iteration will explore continent and species-specific balancing.  But first, let's just see how we do with a default.
#  pass4v3b fixes latin america location error
#  pass4v3c fixes Africa plantation and parks errors in east Africa.
#  pass4v3d adds training data in SE asia.

#  First, some basic packages, and directories
rm(list=ls())
set.seed(12345)
#library(sf)
library(data.table)
setwd("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data")
#  Now to set in and out directories:
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data"
inDir2="H:\\DHK_globalRefor\\accAssessDHK\\v2"
outDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data"

in_name=paste0(outDir, "\\", "zzjoinALL_gain_mergeALL_train_ALL3d.csv")

#  let's load the training.

train_init=fread(in_name)
names(train_init)
summary(train_init) # no NA values
sum(duplicated(train_init$INDEX3)) # no dups
hist(train_init$lossyr_mn)
hist(train_init$lossyr_mn)$breaks
hist(train_init$lossyr_mn)$count
#  going to fix the 2012 plus disturbance means right now; 2017 should have been eliminated, but previous code left the 2017 loss year data, which averaged in.  okay error to have; sd lossyr will be affected but not end of world. 
train_init$lossyr_mn=ifelse(train_init$lossyr_mn>16.99, 0, ifelse(train_init$lossyr_mn>11,11, train_init$lossyr_mn))

train_init[, .(.N), by= .(plant_type)]
train_init[, .(.N), by= .(source2)]
train_init[, .(.N), by= .(type2)]

#  this was already done:
# plant_sel=plant[!Spec_1=="n/a"]  #  remove recently cleared
# #plant_sel=plant2[HV_mean>-2000] # remove NA values
# plant[Type_text=="Clearing/ very young plantation", .(.N), by= .(Type_text, type2)]
#  plant_sel2=plant_sel[Type_text=="Large industrial plantation"]

#############################################
#  okay, looks good
 

#########################################
##  First run below
#subset to oil palm and otherplant
oplant=train_init[type2=="Oil palm" | type2=="gr-Palms" | type2=="Coconut palm" | type2=="gr-Palms, gr-Fruit" | type2=="Oil palm, Hevea" | type2=="Oil palm, Banana, Coconut palm" |type2=="Oil palm, gr-Mixture" | type2=="Oil palm, Acacia" | type2=="Coconut palm, Cashew" | type2=="Oil palm, Unknown" | type2=="Oil palm, Coconut palm"]
oplant$plant_type="Oil palm"
oplant[, .(.N), by= .(plant_type, type2)]

othplant=train_init[!(type2=="NatRegen" | type2=="Oil palm" | type2=="gr-Palms" | type2=="Coconut palm" | type2=="gr-Palms, gr-Fruit" | type2=="Oil palm, Hevea" | type2=="Oil palm, Banana, Coconut palm" |type2=="Oil palm, gr-Mixture" | type2=="Oil palm, Acacia" | type2=="Coconut palm, Cashew" | type2=="Oil palm, Unknown" | type2=="Oil palm, Coconut palm")]
othplant[, .(.N), by= .(plant_type, type2)]

reg_sel=train_init[type2=="NatRegen"]
reg_sel[, .(.N), by= .(plant_type, source2, type2)]

nrow(reg_sel)+nrow(oplant)+nrow(othplant)
#

#  now, subsample 10% from each set of data, for testing.
inTest_opalm <- oplant[,sample(.N, floor(.N*.10), replace=FALSE)]
opalm_train <- oplant[-inTest_opalm]
opalm_test <- oplant[inTest_opalm]
#
inTest_oplant <- othplant[,sample(.N, floor(.N*.10), replace=FALSE)]
oplant_train <- othplant[-inTest_oplant]
oplant_test <- othplant[inTest_oplant]
#
inTest_reg <- reg_sel[,sample(.N, floor(.N*.10), replace=FALSE)]
reg_train <- reg_sel[-inTest_reg]
reg_test <- reg_sel[inTest_reg]  #  
#
# inTest_ifl <- ifl_sel[,sample(.N, floor(.N*.90))]  
oplant_train[, .(.N), by= .(type2)]


##########################################
#  Now, I want to create cross-validation groups in training for more advanced analysis.
#  one way to solve this problem:  in a for loop, break data.table into chunks by type2 
#  then for each chunk, repeat 1:5 until end of nrow.  then randomize.  and join back together
#  re-sort by IF.  
#  Now have almost exactly balanced groups for 5-fold cross val within the training.  

###########  training
#  do oil palm
opalm_split=split(opalm_train, by="type2")
for(i in 1:length(opalm_split)){
rand1=rep(1:5, length=nrow(opalm_split[[i]]))
rand2=rand1[sample(rand1, replace=FALSE)]
opalm_split[[i]]$cvID=rand2
}

opalm_train2b=rbindlist(opalm_split)
opalm_train2=opalm_train2b[order(opalm_train2b$INDEX3),]
opalm_train2[, .(.N), by= .(type2)]

#  now for other plantations
oplant_split=split(oplant_train, by="type2")
for(i in 1:length(oplant_split)){
rand1=rep(1:5, length=nrow(oplant_split[[i]]))
rand2=rand1[sample(rand1, replace=FALSE)]
oplant_split[[i]]$cvID=rand2
}

oplant_train2b=rbindlist(oplant_split)
oplant_train2b=oplant_train2b[order(oplant_train2b$INDEX3),]
oplant_train2b[, .(.N), by= .(type2)]

#  and for regrowth:
rand1=rep(1:5, length=nrow(reg_train))
rand2=rand1[sample(rand1, replace=FALSE)]
reg_train$cvID=rand2
rm(rand1, rand2, oplant_split, opalm_split)

###########  testing
#  do oil palm testing
opalm_split2=split(opalm_test, by="type2")
for(i in 1:length(opalm_split2)){
rand1t=rep(1:5, length=nrow(opalm_split2[[i]]))
rand2t=rand1t[sample(rand1t, replace=FALSE)]
opalm_split2[[i]]$cvID=rand2t
}

opalm_test2b=rbindlist(opalm_split2)
opalm_test2=opalm_test2b[order(opalm_test2b$INDEX3),]
opalm_test2[, .(.N), by= .(type2)]

#  now for other plantations testing
oplant_split2=split(oplant_test, by="type2")
for(i in 1:length(oplant_split2)){
rand1t=rep(1:5, length=nrow(oplant_split2[[i]]))
rand2t=rand1t[sample(rand1t, replace=FALSE)]
oplant_split2[[i]]$cvID=rand2t
}

oplant_test2=rbindlist(oplant_split2)
oplant_test2=oplant_test2[order(oplant_test2$INDEX3),]
oplant_test2[, .(.N), by= .(type2)]

#  and for regrowth testing:
rand1t=rep(1:5, length=nrow(reg_test))
rand2t=rand1t[sample(rand1t, replace=FALSE)]
reg_test$cvID=rand2t

rm(inTest_opalm, inTest_oplant, inTest_reg, opalm_split2, opalm_test, opalm_train) 
rm(oplant, oplant_split2, oplant_test, oplant_train, othplant)
rm(plant_sel2, plant_sel2b, rand1t, rand2t,reg_sel)     

###########################################################
####  now let's create 
#   2) Rubber upsampled, regrowth same (2 class).   RubRegUP
#  let's create RubRegDN: rubber upsampled, regrowth same, downsampled  plantations

#  below is code for bootstrapping UP
#  Code for bootstrapping rubber (Hevea) to a semi arbitrary 10K rows (about a 1/4 of final oplant sample of plR/2 (below))

subRub="yes"
if(subRub=="yes"){
hevea1=oplant_train2b[type2=="Hevea"]
hev_samp=hevea1[,.SD[sample(.N, 10000-nrow(hevea1), replace=TRUE)]]
hev2=rbind(hevea1, hev_samp)
oplant_train2=oplant_train2b[type2!="Hevea"]
} else {
oplant_train2=oplant_train2b
}
oplant_train2b[, .(.N), by= .(type2)]
oplant_train2[, .(.N), by= .(type2)]
oplant_train2[, .(.N), by= .(cvID)]



reg_train$LCtype="regrowth"
reg_test$LCtype="regrowth"
opalm_train2$LCtype="oil_palm"
opalm_test2$LCtype="oil_palm"
oplant_train2$LCtype="other_plant"
hev2$LCtype="other_plant"
oplant_test2$LCtype="other_plant"

rm("hev_samp", "hevea1")

###############################################
#  now form base train data

train_data=rbind(reg_train, opalm_train2, oplant_train2)
#train_dataRub=rbind(reg_train, opalm_train3, oplant_train3b)
test_data=rbind(reg_test, opalm_test2, oplant_test2)
test_data[, .(.N), by= .(source2)]

# first remove duplicates:
sum(duplicated(train_data$INDEX3))
sum(duplicated(test_data$INDEX3))
#train_data=train_data[!(duplicated(train_data$INDEX3) | duplicated(train_data$INDEX3, fromLast=TRUE)),]
#test_data=test_data[!(duplicated(test_data$INDEX3) | duplicated(test_data$INDEX3, fromLast=TRUE)),]

#  then remove the INDEX3 values in the final testing data, from the training
fin_test=read.csv(paste0(inDir2, "\\joinALL_gain_mergeALL_accPolyv2_IND3.csv"))
head(fin_test)

train_data=train_data[!(train_data$INDEX3 %in% fin_test$x),]

#  stop here when running, to check if worked well before proceeding.
##################
#################
##################


#  create final LC class
train_data$LCtype2=ifelse(train_data$LCtype=="regrowth", "regrowth", "plantation")
hev2$LCtype2=ifelse(hev2$LCtype=="regrowth", "regrowth", "plantation")
test_data$LCtype2=ifelse(test_data$LCtype=="regrowth", "regrowth", "plantation")

test_data[, .(.N), by= .(LCtype)]
train_data[, .(.N), by= .(LCtype)]
hev2[, .(.N), by= .(LCtype)]

####################

# #  To balance the classes, for final7, is simpler.  The regrowth class is already pretty comparable in size to the other two.  So i am going to create three different balancings: 
#1)  Raw output so far (for kmeans later.)  later, i will try out a kmeans approach to eliminate secondary forest from my plantations training data.  h2o.kmeans, on follow-up 
#2) equal sample size for each class (three class prediction, with minor loss of OP and Other plantation training).  
#3) two class prediction, equal sampling in both (five CV, two random halves, ensuring all data are used).

######  1  Raw output of training data (might as well write test data here too)
hev3=hev2

#  commented out to avoid overwriting output of v1 of this code.
outfold="/traindata_bal3/"
train_1=rbind(train_data, hev3)
outtrain_1=paste0(getwd(), outfold, paste0("train_data_raw_p4v3_v2.csv"))
fwrite(train_1, outtrain_1)
outtest_1=paste0(getwd(), outfold, paste0("test_data_p4v3_v2.csv"))
fwrite(test_data, outtest_1)

#  okay, this is new balancing code.  First, I am going to create tenfold training data breaks (see below for details.)
###############
##  outfold="/traindata_bal0/"
##  train_data=fread(getwd(), outfold, paste0("train_data_raw_p4v3_v2.csv")))

##  new to v2
###### tenfold two class training, with othplant and op exactly balanced.
rm(hev2, fin_test, opalm_test2, opalm_test2, opalm_train2, opalm_train2b, oplant_test2b, oplant_train2, oplant_train2b, reg_test, reg_train, train_1, train_init, opalm_test2b)

#  now lets do some counting
op1=train_data[LCtype=="oil_palm"]
reg1=train_data[LCtype=="regrowth"]
oth1=train_data[LCtype=="other_plant"]
pl1=nrow(op1);pl1
plR=nrow(reg1); plR
plO=nrow(oth1); plO
plH=nrow(hev3); plH

train_data[, .(.N), by= .(LCtype)]
train_data[, .(.N), by= .(LCtype2)]

#  from here on, deleted v1 code and wrote new code for v2:
maxR=plR*2
#  so the midpoint will bbe the same for all three classes, plR

#  let's do this:
#samp_size=c(0.2, 0.4, 0.6, 0.8, 1)*plR

samp_reg=c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 1, 1, 1, 1, 1)*plR
#samp_plant=round(c(1.8, 1.6, 1.4, 1.2, 1, 0.8, 0.6, 0.4,0.2)*plR)
samp_plant=c(1, 1, 1, 1, 1, 1, 0.9, 0.8, 0.7, 0.6, 0.5)*plR

for(p in 1:11){
samp_reg2=samp_reg[p]
samp_plant2=samp_plant[p]
if(p<7){
print(paste("fold", p))
out_reg=reg1[,.SD[sample(.N, samp_reg2, replace=FALSE)]]
up_palm=op1[,.SD[sample(.N, (samp_plant2-pl1), replace=TRUE)]]
out_palm=rbind(op1, up_palm)
up_oth=oth1[,.SD[sample(.N, (samp_plant2-plO-plH), replace=TRUE)]]
out_oth=rbind(oth1, hev3, up_oth)
train_2=rbind(out_reg, out_palm, out_oth)
print(paste("reg", nrow(out_reg)))
print(paste("palm", nrow(out_palm)))
print(paste("other", nrow(out_oth)))
print(paste("total", nrow(train_2)))
outtrain_2=paste0(getwd(), outfold, paste0("train_data_Range9_p4v3_", p, ".csv"))
fwrite(train_2, outtrain_2)
}
#
if(p>6 & p<10){
print(paste("fold", p))
out_reg=reg1[,.SD[sample(.N, samp_reg2, replace=FALSE)]]
up_palm=op1[,.SD[sample(.N, (samp_plant2-pl1), replace=TRUE)]]
out_palm=rbind(op1, up_palm)
up_oth=oth1[,.SD[sample(.N, (samp_plant2-plO-plH), replace=TRUE)]]
out_oth=rbind(oth1, hev3, up_oth)
train_2=rbind(out_reg, out_palm, out_oth)
print(paste("reg", nrow(out_reg)))
print(paste("palm", nrow(out_palm)))
print(paste("other", nrow(out_oth)))
print(paste("total", nrow(train_2)))
outtrain_2=paste0(getwd(), outfold, paste0("train_data_Range9_p4v3_", p, ".csv"))
fwrite(train_2, outtrain_2)
}
#

if(p>9){
print(paste("fold", p))
out_reg=reg1[,.SD[sample(.N, samp_reg2, replace=FALSE)]]
out_palm=op1[,.SD[sample(.N, (samp_plant2), replace=FALSE)]]
oth2=rbind(oth1, hev3)
out_oth=oth2[,.SD[sample(.N, (samp_plant2), replace=FALSE)]]
train_2=rbind(out_reg, out_palm, out_oth)
print(paste("reg", nrow(out_reg)))
print(paste("palm", nrow(out_palm)))
print(paste("other", nrow(out_oth)))
print(paste("total", nrow(train_2)))
outtrain_2=paste0(getwd(), outfold, paste0("train_data_Range9_p4v3_", p, ".csv"))
fwrite(train_2, outtrain_2)
}

}

#################
