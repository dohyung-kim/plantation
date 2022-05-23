#  Time to predict:
# final 
#  p1 ran just the model predictions.
#  p2 takes all the predictions from p1, and saves just the predictions in one file (plantation prob), and the prediction votes (LC class) in another.  
#  p3 runs the RF ensemble prediction for each continent: it First checks purity (one continent per tile) to make sure this kosher by tile.
#  p4 joins the output csvs back to the vector files for display.
#  there's another bit of code which shrinks the shapefile, but I will re-run at end.
#  p5 outputs to a mangrove subfolder, it is the attempt to calculate the area of overlap for each gain polygon with Global Mangrove Watch 2015 shapefile, -60 m buffered in to eliminate accidental overlap.  
#  This script p6 creates a mangrove mask column, creates a column to threshold based on inspection for open, then applying said masks to the different internal accuracy metrics, and then quickly evaluating which one is the best, and then outputting a set for display.

#  load needed libraries and functions.
rm(list=ls())
library(data.table)
library(sf)
#library(h2o)
library(plyr)
library(dplyr)

#  now to load up mangrove layer:
library(sf)
pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble\\predRF1"
inDir2="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\case_data\\plantation_ALL_2015_v3"
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1\\mangrove_mask")
#

#  
mang2=st_read(paste0(outDir, "\\", "mang_prelimALL.gpkg"))
#
mang3=mang2 %>% st_set_geometry(NULL)

mang3=data.table(mang3)
mang_ans <- mang3[, .(prop=sum(int_prop), area=mean(area4326)), by = .(INDEX3)]
mang_ans$conv_reg=ifelse(mang_ans$area>=20000 & mang_ans$prop<0.05, 0, 1)

fwrite(mang_ans, paste0(outDir, "\\mang_selected.csv"))

mang2b=left_join(mang2, mang_ans, "INDEX3")

st_write(mang2b, paste0(outDir, "\\mang_selectALL.gpkg"), delete_layer=TRUE)
#
mang_sel=mang_ans[conv_reg==1

##################
#######
###
#  okay now to load up the various files and calculate what I want to calculate.  this is going to make some massive csv files

#  load up the data
explore2=fread("H:/DHK_globalRefor/tp_pass4_2019/2015v3/data_explore/Explore_cent_SpectralSARdata_plusPreds.csv")
explore2$openMask=with(explore2, ifelse(pNDVI<=0.48 & HVp_mn<1900, 1, 0))
explore2sel=subset(explore2, select=c("INDEX3", "openMask"))

#i=30
for(i in 1:30){
print(i)
fin_join=fread(paste0(inDir, "\\Pred_FinEnsRF_", i, "_predRF2.csv"))
#fin_join$mangMask=rep(nrow(fin_join), -1)
fin_join$mangMask=ifelse(fin_join$INDEX3 %in% mang_sel$INDEX3, 1, 0)

# dimcheck=ifelse(fin_join$INDEX3 %in% mang_sel$INDEX3, 1, 0)
# dim(dimcheck)
# dim(fin_join)
#names(fin_join)

fin_join2=left_join(fin_join, explore2sel, "INDEX3")


#  now to mask the prediction:
fin_join2$predMang=with(fin_join2, ifelse(mangMask==1, "regrowth", fin_join2$predict))

fin_join2$pred3class=with(fin_join2, ifelse(openMask==1, "open", fin_join2$predMang))

#  and let's derive a confidence layer intermediate:

signsign=with(fin_join2, ifelse((predict.Af==wt.Pred & predict.Af==maj.Ens & maj.Ens==wt.Pred), 1, -1))

prob.MJ=with(fin_join2, ifelse(predict.Af==maj.Ens, vote.Ens, (1-vote.Ens)))
wt.PredM=with(fin_join2, ifelse(wt.Pred=="plantation", wt.Mean, 1-wt.Mean))
prob.WP=with(fin_join2, ifelse(predict.Af==wt.Pred, wt.PredM, (1-wt.PredM)))


##  okay let's dive in:
# predict.out$conf.all=with(predict.out, ifelse((predict.Af!=wt.Pred | predict.Af!=maj.Ens | prob.RF<=0.6 | vote.Ens<=0.6), "Low", "High"))

fin_join2$fin_conf=((fin_join2$prob.RF+prob.WP+prob.MJ)/3)
#
fin_join2$conf_ord=with(fin_join2, ifelse((mangMask==1 | openMask==1), 0, signsign))
fin_join2$conf_rank=with(fin_join2, ifelse((predict.Af!=wt.Pred | predict.Af!=maj.Ens | wt.Pred!=maj.Ens | mangMask==1 | openMask==1), "Low", ifelse((prob.RF<=0.7 | vote.Ens<=0.7 |  wt.PredM<=0.7), "Intermediate", "High")))

fwrite(fin_join2, paste0(inDir, "\\Pred_FinEnsRF_", i, "_predRF2b.csv"))
print("done")
}
#
