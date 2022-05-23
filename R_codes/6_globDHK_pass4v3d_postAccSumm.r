#  original name: globDHK_pass4v3d_postAccSumm.r

# Note inconsistency in naming--this is correct in order however as numbered at start.
#  this examines internal accuracy, which affects the final ensemble prediction.

library(data.table)
rm(list=ls())

inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3"
modtype=c("best", "xrt", "drf", "glm", "dl")
index1=NULL
out_ans=list()
for(i in 1:5){
mod_in=modtype[i]
if(mod_in=="dl"){
indirect=paste0(inDir, "\\extraDL")
} else {
indirect=inDir
}
for(k in 1:11){
count2=(i-1)*11+k
print(count2)
print(paste(k, mod_in))
inDir2=paste0(indirect, "\\fold", k)
#predict_passp4v3_AML1best1_2
if(mod_in=="dl"){
da6=fread(paste0(inDir2, "\\predict_passp4v3final_AML1", mod_in, "1_", k, ".csv"))
} else {
da6=fread(paste0(inDir2, "\\predict_passp4v3_AML1", mod_in, "1_", k, ".csv"))
}

ans <- da6[, .N, by = .(continent, LCtype2, predict)]
ans$fold=rep(k, nrow(ans))
ans$modtype=rep(mod_in, nrow(ans))

out_ans[[count2]]=ans
}

}
#
#  Now to process the data and calculate accuracy
acc_La=NULL
acc_As=NULL
acc_Af=NULL
#b=1
for(b in 1:55){
out_ans2=out_ans[[b]]
out_ord=setorder(out_ans2, continent, LCtype2, predict)
list_con=c("Latin_America", "Asia", "Africa")
for(bb in 1:3){
con=list_con[bb]
out_ordLa=out_ord[continent==con]
plant_acLa=out_ordLa[LCtype2=="plantation" & predict=="plantation",N]/(sum(out_ordLa[LCtype2=="plantation", N]))
reg_acLa=out_ordLa[LCtype2=="regrowth" & predict=="regrowth",N]/(sum(out_ordLa[LCtype2=="regrowth", N]))
meanA_perclass=mean(plant_acLa, reg_acLa)
if(bb==1){
acc_La=c(acc_La, meanA_perclass)
}
if(bb==2){
acc_As=c(acc_As, meanA_perclass)
}
if(bb==3){
acc_Af=c(acc_Af, meanA_perclass)
}
#
}
}
# go with weights as a cube, and write out original acc and the weights as csv files.  
wt_La=acc_La^3
wt_As=acc_As^3
wt_Af=acc_Af^3

out_La=data.frame(acc_La, wt_La)
out_As=data.frame(acc_As, wt_As)
out_Af=data.frame(acc_Af, wt_Af)
fwrite(out_La, paste0(inDir, "\\ensemble\\weights_testDataAcc_LatAm.csv"))
fwrite(out_As, paste0(inDir, "\\ensemble\\weights_testDataAcc_Asia.csv"))
fwrite(out_Af, paste0(inDir, "\\ensemble\\weights_testDataAcc_Africa.csv"))

#length(unique(index1))
#length(unique(da6$INDEX3))

##########  RUN FOR OUTPUT!!
# let's try that again, and create a dataset to predict on:
library(data.table)
rm(list=ls())

inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3"
modtype=c("best", "xrt", "drf", "glm", "dl")
for(i in 1:5){
mod_in=modtype[i]
if(mod_in=="dl"){
indirect=paste0(inDir, "\\extraDL")
} else {
indirect=inDir
}
for(k in 1:11){
print(paste(k, mod_in))
inDir2=paste0(indirect, "\\fold", k)
#predict_passp4v3_AML1best1_2
if(mod_in=="dl"){
da6=fread(paste0(inDir2, "\\predict_passp4v3final_AML1", mod_in, "1_", k, ".csv"))
} else {
da6=fread(paste0(inDir2, "\\predict_passp4v3_AML1", mod_in, "1_", k, ".csv"))
}

if(i==1 & k==1){
out_data=da6[, .(INDEX3, continent, LCtype2, plantation)]
max1=ncol(out_data)
names(out_data)[max1]=paste0(mod_in, "_", k, "plant")
} else {
out_data=cbind(out_data, da6[, plantation])
max1=ncol(out_data)
names(out_data)[max1]=paste0(mod_in, "_", k, "plant")
}
# 
}
}
#
fwrite(out_data, paste0(inDir, "/ensemble/allpredEns_bal3__step1pred3.csv"))
#  