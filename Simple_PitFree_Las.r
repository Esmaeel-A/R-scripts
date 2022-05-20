#load the required packages
require(raster)
require(dplyr)
require(sf)
require(ggplot2)
require(rgdal)
require(rgeos)
require(rGEDI)
require(lidR)
require(plot3D)
require(sp)
require(RCSF)
require(gstat)
require(mapview)

##setting the workplace
# choosing input & output dir
indir = "D:/Esmaeel GEDI R/Data from forest institure Malaysia/FRIM_campus_selangor/Point_Cloud"
dir_out= "D:/Esmaeel GEDI R/LASprocessing/output/FRIM site"



# Creating Output folders  #if you do it once, don't do it again..
Create_folders <- function(){
  dir.create(paste0(dir_out, "/Classified"))
  dir.create(paste0(dir_out,"/Normalized"))
  dir.create(paste0(dir_out, "/Filtered"))
  dir.create(paste0(dir_out,"/Extracted_Plots_forGEDI_footPrint"))
  dir.create(paste0(dir_out, "/Simulated"))
  
}
#execute create output folders
Create_output_folders <- Create_folders()

#setting dir
dir_classified <-paste0(dir_out,"/Classified")
dir_normalized <- paste0(dir_out,"/Normalized")
dir_filtered <- paste0(dir_out,"/Filtered")
dir_extracted <- paste0(dir_out, "/Extracted_Plots_forGEDI_footPrint")
dir_simulated <- paste0(dir_out, "/Simulated")
dir_CHM <- paste0(dir_out, "/CHM")
setwd(dir_extracted)

# Make a catalog of the fils
ctg <- readLAScatalog(indir)

plot(ctg)
summary(ctg$Min.Z)
#validation
las_check(ctg, deep = TRUE)  # add the argument , deep = TRUE for deep inspection

spplot(ctg, "Min.Z")
#ground classification
opt_output_files(ctg) <- paste0(dir_classified, "{*}_classified")
##Option#2: Using Cloth simulation function Algorithm for ground classification
classified_ctg <- classify_ground(ctg, csf())

# Making a catalog of classified las
ctg <- readLAScatalog(dir_classified)
plot(ctg)

#making dsm
dir_dsm <- "D:/Esmaeel GEDI R/LASprocessing/output/FRIM site/DSM"
opt_output_files(ctg) <-  paste0(dir_dsm, "/{*}_dsm")

dsm_pitfree_csf <- lidR::grid_canopy(ctg, res = 1, 
                                     lidR::pitfree(c(0,2,5,10,15), c(0, 1)))
summary(dsm_pitfree_csf)

#making DTM
ctg <- readLAScatalog(dir_classified)
dir_dtm <- "D:/Esmaeel GEDI R/LASprocessing/output/FRIM site/DTM"
opt_output_files(ctg)<-paste0(dir_dtm,"/{*}_dtm")

dtm_knn_csf = lidR::grid_terrain(ctg, res=1,  algorithm = lidR::knnidw(k=50, p=3))

#CHM


dir_out <- "D:/Esmaeel GEDI R/LASprocessing/output/FRIM site/CHM_pitfree"
summary(dsm_pitfree_csf - dtm_knn_csf)
CHM_pitfree1m <- dsm_pitfree_csf - dtm_knn_csf
projection(CHM_pitfree1m) <- "+proj=omerc +lat_0=4 +lonc=102.25 +alpha=323.0257905 +k=0.99984 +x_0=804670.24 +y_0=0 +no_uoff +gamma=323.1301023611111 +a=6377295.664 +b=6356094.667915204 +units=m +no_defs"


plot(CHM_pitfree1m, mapview = TRUE, map.type = "Esri.WorldImagery")
mapview(CHM_pitfree1m, map.type = "Esri.WorldImagery")
writeRaster(CHM_pitfree1m, paste0(dir_out, "/CHM_pitfree1m.tif"))
