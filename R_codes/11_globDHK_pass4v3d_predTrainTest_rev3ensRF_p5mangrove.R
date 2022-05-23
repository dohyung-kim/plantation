#  Time to predict:
# final 
#  p1 ran just the model predictions, but be prepared that you may need to first filter out NA data in the polygons.  You can fill it back in with -999.
#  p2 takes all the predictions from p1, and saves just the predictions in one file (plantation prob), and the prediction votes (LC class) in another.  
#  p3 runs the RF ensemble prediction for each continent: it First checks purity (one continent per tile) to make sure this kosher by tile.
#  p4 joins the output csvs back to the vector files for display.
#  p5  will output to a mangrove subfolder, it is the attempt to calculate the area of overlap for each gain polygon with Global Mangrove Watch 2015 shapefile, -60 m buffered in to eliminate accidental overlap.  
#  Tasks left after are creating a mangrove mask column, quickly creating a column to threshold based on inspection for open, then applying said masks to the different accuracy metrics, and then quickly evaluating which one is the best, and then outputting a set for display.

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
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1\\mangrove_mask")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
#  load up mangrove buffered data:
# mang=st_read("H:\\DHK_globalRefor\\mangroves\\gmw_2015_v2_prj_inBuff60.shp")
aeqd <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# library(lwgeom)
# mang2= st_make_valid(mang)
# st_write(mang2, "H:\\DHK_globalRefor\\mangroves\\gmw_2015_v2_prj_inBuff60val.shp")
#mang2=st_read("H:\\DHK_globalRefor\\mangroves\\gmw_2015_v2_prj_inBuff60val.shp")
mang2=st_read("H:\\DHK_globalRefor\\mangroves\\gmw_2015_v2_prj.shp")
library(raster)

#i=22
for(i in 1:30){
print("start")
print(i)
in_name=paste0(inDir2, "\\subset_size\\join30\\", "joinALL_gain_merge", i, ".gpkg")
data1<-st_read(in_name) %>% st_transform(aeqd)
#data2 <- st_make_valid(data1)  
#  now to run intersection, and calculate area
  zint <- st_join(data1,mang2) 
#  zint$intarea=st_area(zint)
zsel=zint[complete.cases(zint$pxlval),]

# if(i==22) {
# library(lwgeom) 
# zsel <- st_make_valid(zsel)
# }

ztry= st_intersection(zsel, mang2)

if(nrow(ztry)>0) {
ztry2=ztry %>% st_transform(4326)
ztry2$intarea=area(as(ztry2, "Spatial"))
# prelim, not prelim2, was based on the buffered mangrove
ztry2$int_prop=with(ztry2, intarea/area4326)
st_write(ztry2, paste0(outDir, "\\mang_prelim2_", i, ".gpkg"), , delete_layer = TRUE)
} 
print(i)
print("done!")
rm(data1, zint, zsel, ztry)
}
#

#  now to load and bind all of them:
library(sf)
pass="traindata_bal3"
versN="p4v3"
inDir="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\traindata_bal3\\step1pred_output3\\ensemble\\predRF1"
inDir2="H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\case_data\\plantation_ALL_2015_v3"
outDir=paste0("H:\\DHK_globalRefor\\tp_pass4_2019\\2015v3\\training_data\\", pass, "\\step1pred_output3\\ensemble", "\\predRF1\\mangrove_mask")
#
listdir=list.files(outDir)
#  pause and subset to version 2
for(i in 1:length(listdir)){
in_name=paste0(outDir, "\\", listdir[i])
if(i==1){
mang3=st_read(in_name)
} else {
mang3b=st_read(in_name)
mang3=rbind(mang3, mang3b)
}
#
}
#

st_write(mang3, paste0(outDir, "\\", "mang_prelimALL_2.gpkg"))