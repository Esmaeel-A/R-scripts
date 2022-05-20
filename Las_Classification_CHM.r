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

#point cloud-based Height normalization
opt_output_files(ctg) <-  paste0(dir_normalized, "/{*}_norm")
##option 1 Using Kriging for interpolation
#ctg_norm <- normalize_height(ctg, kriging(k = 40))

##option 2 Using TIN for interpolation
ctg_norm <- normalize_height(ctg, tin(), na.rm = TRUE)



###-----------------------------Filtering nomalized las---------------------------###

# Making a catalog of dir_Normalized las
ctg <- readLAScatalog(dir_normalized)
plot(ctg)

#checking for invalid values
summary(ctg)
spplot(ctg, "Max.Z")
spplot(ctg, "Min.Z")
summary(ctg$Max.Z)
summary(ctg$Min.Z)

# make a list and loop filter ove Las files and write output
list <- list.files(dir_normalized,".las",full.names = T)
for (i in seq_along(list)){

l <- readLAS(list[i])

#keep everything above zero and below the 90 quantile
filtered <- filter_poi(l, Z >= 0 & Z < 70) 

writeLAS(filtered, paste0(dir_filtered, "/Filtered_", basename(list[i]))
         
        , index = FALSE)
}



#check the filtered data

ctg <- readLAScatalog(dir_filtered)

summary(ctg)
spplot(ctg, "Max.Z")
spplot(ctg, "Min.Z")
summary(ctg$Max.Z)
summary(ctg$Min.Z)

###-----------------------------Making CHM-----------------------------------------------------------------------------------------###
# MAKING CHM in Multiple resolutions
# Making a catalog of dir_filtered las
ctg <- readLAScatalog(dir_filtered)
plot(ctg)


#Check las crs and modify if needed
ctg
plot(ctg, mapview = TRUE, map.type = "Esri.WorldImagery")
projection(ctg) <- 'EPSG:3168'   ##3168 is RSo kartau coordinates  for Malaysia
##32648 is UTm zone 48 N  for Malaysia
plot(ctg, mapview = TRUE, map.type = "Esri.WorldImagery")



#output
opt_output_files(ctg) <- paste0(dir_CHM, "/chm_{*}")




#making 1m chm

chm1 <- grid_canopy(ctg, 1, p2r(0.2))
plot(chm1, col = height.colors(50))
writeRaster(chm1, paste0(dir_out, "/CHM_1m.tif"))
plot(chm1, mapview = TRUE, map.type = "Esri.WorldImagery")

#making 5m chm

chm5 <- grid_canopy(ctg, 5, p2r(0.15))
plot(chm5, col = height.colors(50))
writeRaster(chm5, paste0(dir_out, "/CHM_5m.tif"))
plot(chm5, mapview = TRUE, map.type = "Esri.WorldImagery")



#making 30 m resolution 
chm30 <- grid_canopy(ctg, 30, p2r())
plot(chm30, col = height.colors(50))
writeRaster(chm30, paste0(dir_out, "/CHM_30m.tif"))


#making 250 m resolution 
chm250 <- grid_canopy(ctg, 250, p2r())
plot(chm250, col = height.colors(50))
writeRaster(chm250, paste0(dir_out, "/CHM_250m.tif"))

#making 1000m resolution

chm1000 <- grid_canopy(ctg, 1000, p2r())
plot(chm1000, col = height.colors(50))
writeRaster(chm1000, paste0(dir_out, "/CHM_1km.tif"))





###-----------------------------Clipping las to GEDI footprints---------------------------###
#load normalized and filtered las into a new catalog
# Making a catalog of dir_filtered las
ctg <- readLAScatalog(dir_filtered)
plot(ctg)

#Check las crs and modify if needed
ctg
plot(ctg, mapview = TRUE, map.type = "Esri.WorldImagery")
projection(ctg) <- 'EPSG:3168'
plot(ctg, mapview = TRUE, map.type = "Esri.WorldImagery")
#Extracting plots #like for GEDI footprint simulator

#read shp with GEDI footprints and extract footprints coordinates

#dir for the shapefile with GEDI shots over the site   #pay attention to the coords and the transforamtion to LAS coords and back to wgs at later stage


dir.shapefiles <- "D:/Esmaeel GEDI R/LASprocessing/output/FRIM site/GEDI_footprints_shp"
shapefile_name <- "GEDI_raw_FRIM_Project.shp"

aoi_shp <- shapefile(file.path(dir.shapefiles,
                               shapefile_name))

#Check coordinates and reproject to align with crg if needed
plot(aoi_shp, mapview = TRUE, map.type = "Esri.WorldImagery")
aoi_shp
#reproject if necessary # here using Kertau (RSO) / RSO Malaya (m)
aoi_shp <- spTransform(aoi_shp,"+proj=omerc +lat_0=4 +lonc=102.25 +alpha=323.0257905 +k=0.99984 +x_0=804670.24 +y_0=0 +no_uoff +gamma=323.1301023611111 +a=6377295.664 +b=6356094.667915204 +units=m +no_defs")

#specifyin gpoints coordinates and radius
x <- coordinates(aoi_shp)[,1]
y <- coordinates(aoi_shp)[,2]
r <- 12.5



#writing output
opt_output_files(ctg) <- paste0(dir_extracted, "/{XCENTER}_{YCENTER}_{ID}")



##Extracting plots  multiple
rois <- clip_circle(ctg, x, y, r)
rois
rois$filename


###---------------------------------------------#################
###Simulating GEDI footprint


# Creating Output sub-folders  #if you do it once, don't do it again..
Create_folders <- function(){
  dir.create(paste0(dir_simulated, "/H5"))
  dir.create(paste0(dir_simulated,"/TXT"))
  dir.create(paste0(dir_simulated,"/SHP"))
}
#execute create output folders
Create_output_folders <- Create_folders()

# Read clipped las files (GEDI LAS footprint)

list <- list.files(dir_extracted,".las",full.names = T)

for (i in seq_along(list)) {
  las <- readLAS(list[i])
  # Extracting plot center geolocations
  xcenter = mean(las@bbox[1,])
  ycenter = mean(las@bbox[2,])
  
  # Simulating GEDI full waveform
  wf <-gediWFSimulator(list[i],
                           output=file.path(paste0(paste0(dir_simulated,"/H5"), "/", substr(basename(list[i]),0,nchar(basename(list[i]))-4),".h5")),
                           coords = c(xcenter, ycenter))

  #metric
  
  
  tryCatch({  wf_metrics<-gediWFMetrics(input= wf,
                                        outRoot=file.path(paste0(paste0(dir_simulated,"/TXT"), "/reg_", substr(basename(list[i]),0,nchar(basename(list[i]))-4))))
  
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  
  
  
#metric_noise
  tryCatch({  wf_metrics_noise<-gediWFMetrics(input=wf,
                                              outRoot= file.path(paste0(paste0(dir_simulated,"/TXT"), "/noise", substr(basename(list[i]),0,nchar(basename(list[i]))-4))),
                                              linkNoise= c(3.0103,0.95),
                                              maxDN= 4096,
                                              sWidth= 0.5,
                                              varScale= 3)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  
  
}

###creating shp from txt metrics

#list noised and regular simulated metrics
list_Metrics <- list.files(paste0(dir_simulated,"/TXT"), pattern = "*reg_*",full.names = T)
list_Metrics_noise <- list.files(paste0(dir_simulated,"/TXT"), pattern = "*noise*", full.names = T)


#make a list to all metrics & extract header names from first txt &clean $ name long-lat
H_name <- readLines(list_Metrics[1])
H_name <- unlist(strsplit(H_name[1],","))
H_name <- make.names(H_name, unique = T)
H_name <- paste0(substring(H_name, 6, 10), substring(H_name, nchar(H_name)-4, nchar(H_name)))
# #make a list to all type stat
Simulated_metric <- vector(length = length(H_name))
#assign the name

names(Simulated_metric) <- H_name

Reg_sim_Metric <- Simulated_metric
Noise_sim_Metric <- Simulated_metric



#loop over the list to populate the list with metrics

for (i in seq_along(list_Metrics)) {

# some cleaning and organizing
l <- readLines(list_Metrics[i])
l_noise <- readLines(list_Metrics_noise[i])


k2 <- unlist(strsplit(l[2]," ")) 
k2_noise <- unlist(strsplit(l_noise[2]," ")) 

k2 <- k2[-98:-100]
k2[98] <- substr(k2[98],16,nchar(k2[98])-3)
k2_noise <- k2_noise[-98:-100]
k2_noise[98] <- substr(k2_noise[98],16,nchar(k2_noise[98])-3)
k2 <- as.numeric(k2)
k2_noise <- as.numeric(k2_noise)

Reg_sim_Metric <- rbind.data.frame(Reg_sim_Metric, k2)
Noise_sim_Metric <- rbind.data.frame(Noise_sim_Metric, k2_noise)


}
Reg_sim_Metric <- Reg_sim_Metric[-1,]
Noise_sim_Metric <- Noise_sim_Metric[-1,]
names(Reg_sim_Metric) <- H_name
names(Noise_sim_Metric) <- H_name



xy <- Reg_sim_Metric[,c(107,108)]
#using Kertau (RSO) / RSO Malaya 3168
Reg_sim_shp <- SpatialPointsDataFrame(coords = xy, data = Reg_sim_Metric,
                               proj4string = CRS("+proj=omerc +lat_0=4 +lonc=102.25 +alpha=323.0257905 +k=0.99984 +x_0=804670.24 +y_0=0 +no_uoff +gamma=323.1301023611111 +a=6377295.664 +b=6356094.667915204 +units=m +no_defs"))

projection(Reg_sim_shp) <- 'EPSG:3168'

Noise_sim_shp <- SpatialPointsDataFrame(coords = xy, data = Noise_sim_Metric,
                                      proj4string = CRS("+proj=omerc +lat_0=4 +lonc=102.25 +alpha=323.0257905 +k=0.99984 +x_0=804670.24 +y_0=0 +no_uoff +gamma=323.1301023611111 +a=6377295.664 +b=6356094.667915204 +units=m +no_defs"))

projection(Noise_sim_shp) <- 'EPSG:3168'


plot(Reg_sim_shp, mapview = TRUE, map.type = "Esri.WorldImagery")
plot(Noise_sim_shp, mapview = TRUE, map.type = "Esri.WorldImagery")

# export as ESRI shp 

raster::shapefile(Reg_sim_shp, paste0(paste0(dir_simulated,"/SHP"), "/Reg_sim2.shp"))
raster::shapefile(Noise_sim_shp, paste0(paste0(dir_simulated,"/SHP"), "/Noise_sim.shp"))





