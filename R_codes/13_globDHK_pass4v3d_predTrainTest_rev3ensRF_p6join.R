#  Time to predict:
# final uses an 11-fold split (balanced differently) of teh expanded training data
#  pass4 v3 final joins with additional information from a point file
#  _gbm run uses a GBM from automML
#  _dl used a DL-only autoML to create good DL prediction
#  _rf used the RF from autoML
#  _glm used the GLM from autoML to look at just shapefile number 5, for comparison among all available model types.  

#  p1 ran just the model predictions, but be prepared that you may need to first filter out NA data in the polygons.  You can fill it back in with -999.
#  p2 takes all the predictions from p1, and saves just the predictions in one file (plantation prob), and the prediction votes (LC class) in another.  
#  p3 runs the RF ensemble prediction for each continent: it First checks purity (one continent per tile) to make sure this is correct by tile.
#  p4 joins the output csvs back to the vector files for display.

#  next step is to link the predictions to the original vector data, omitting all the 55 model predictions and making sure to create an NA class where there is not a matching INDEX3 (since I omitted the no data classes).  Then inspect and share.  


#  load needed libraries and functions.
rm(list=ls())
library(data.table)
library(sf)
#library(h2o)
library(plyr)
library(dplyr)
#h2o.init(nthreads = 20)

pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble\\predRF1"
inDir2="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\case_data\\plantation_ALL_2015_v3"
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

#i=30
for(i in 1:30){
print("start")
print(i)
in_name=paste0(inDir2, "\\subset_size\\join30\\", "joinALL_gain_merge", i, ".gpkg")
data1<-st_read(in_name)

#  now to load and subset data (dropping 55 predictions)
in_nameP=paste0(inDir, "\\Pred_FinEnsRF", "_", i, "_predRF2b.csv")
pred1=fread(in_nameP)
  names(pred1)
pred_sel=pred1[,c(1:2, 58:74)]
names(pred_sel)
rm(pred1)

#  now join and write out:
data2=left_join(data1, pred_sel, by="INDEX3")
#data2=st_bind_cols(data1, pred3)

out_name=paste0(outDir, "\\PredEnsRF1_Fin", versN, "_mergeArea_",  pass, "passBb_", i, ".gpkg")
st_write(data2, out_name, delete_layer = TRUE)


data2sel=data2[,c(1:5, 37, 41:46, 48:55)]
names(data2sel)

out_nameD=paste0(outDir, "\\display\\PredEnsRF1_Fin", versN, "_mergeArea_",  pass, "passBbDisp_", i, ".gpkg")

st_write(data2sel, out_nameD, delete_layer = TRUE)

print("done")
print(i)
}
#



