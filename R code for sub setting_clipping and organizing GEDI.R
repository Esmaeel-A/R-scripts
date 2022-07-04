####Instructions####
#1 Create a main folder named "GEDI_DATA" with subfolders as follow:
#1.1 "RawL2A" #put GEDI raw level 2A here
#1.2 "RawL2B" #put GEDI raw level 2B here
#1.3 "AOI" #put clipping shape file here


#2 Insert the path for the main folder (the path for 'GEDI_DATA' folder)
dir.GEDI_DATA <- "C:/Users/Esmae/Desktop/Conference_paper_data/GEDI_DATA"

#3 Insert the name of your clipping shapefile with ".shp"
shapefile_name <- "Main_Forest_Polys.shp"

#4 Specify the splitting field name (if it's not "id")
split_by = 'id'
#------------------------------------------------------------------
#in case memory issues /worked for Windows 64 bit 8G RAM/
###checking memory size/limit
gc()
memory.size()
memory.limit()
#increase the limit
memory.limit(size=56000)
#--------------------------------------------------------------------------------
                     ###START###
#--------------------------------------------------------------------------------
##setting the workplace
# Creating Output folders  #if you do it once, don't do it again..
Create_folders <- function(){
  dir.create(paste0(dir.GEDI_DATA, "/Output"))
  dir.create(paste0(dir.GEDI_DATA,"/Output/Clipped_h5files_L2A"))
  dir.create(paste0(dir.GEDI_DATA,"/Output/Clipped_h5files_L2B"))
  dir.create(paste0(dir.GEDI_DATA, "/Output/Final_shps")) 
}
#execute create output folders
Create_output_folders <- Create_folders()

#setting dir
dir.output.Clipped_L2A <-paste0(dir.GEDI_DATA,"/Output/Clipped_h5files_L2A")
dir.output.Clipped_L2B <- paste0(dir.GEDI_DATA,"/Output/Clipped_h5files_L2B")
dir.output <- paste0(dir.GEDI_DATA, "/Output/Final_shps")
dir.shapefiles <- paste0(dir.GEDI_DATA, "/AOI")
dir.input2a <- paste0(dir.GEDI_DATA, "/RawL2A")
dir.input2b <- paste0(dir.GEDI_DATA, "/RawL2B")

#load the required packages
require(rgdal)
require(rgeos)
require(rGEDI)
require(raster)
require(dplyr)
require(sf)
require(sp)
require(ggplot2)
#------------------------------------------------------------------
##Clipping GEDI data to AOI

# Read aoi shp
aoi_shp <- shapefile(file.path(dir.shapefiles,
                               shapefile_name))

# List of file paths
list_h5_2a <- list.files(dir.input2a,".h5",full.names = T)
list_h5_2b <- list.files(dir.input2b,".h5",full.names = T)


#clip function
Clip <- function(list_h5_2a,list_h5_2b, aoi_shp,
                             write_output = FALSE){ 
  for (i in seq_along(list_h5_2b)){
    
    print(list_h5_2b[i])
    
    # Read datasets
    l2a <- readLevel2A(list_h5_2a[i])
    l2b <- readLevel2B(list_h5_2b[i])
    

    # Clip aoi
    l2a <- clipLevel2AGeometry(l2a,polygon_spdf=aoi_shp,output=paste0(dir.output.Clipped_L2A,"//clipped_", basename(list_h5_2a[i]), split_by=split_by))
    
    l2b <- clipLevel2BGeometry(l2b,polygon_spdf=aoi_shp,output=paste0(dir.output.Clipped_L2B,"//clipped_", basename(list_h5_2b[i]), split_by=split_by))
}
}

#Execute Clipping
Clipping_GEDI <- Clip(list_h5_2a, list_h5_2b, aoi_shp)

#--------------------------------------------------------------------------------------
#Extracting and Filtering metrics


#List of file paths
list_clipped_h5_2a <- list.files(dir.output.Clipped_L2A,".h5",full.names = T)
list_clipped_h5_2b <- list.files(dir.output.Clipped_L2B,".h5",full.names = T)

#function for extracting metrics and filtering
Extract_Filter <- function(list_clipped_h5_2a,list_clipped_h5_2b,
                 write_output = FALSE){ 
  for (i in seq_along(list_clipped_h5_2b)){
    
    print(list_clipped_h5_2b[i])
    
    # Read datasets
    l2a <- readLevel2A(list_clipped_h5_2a[i])
    l2b <- readLevel2B(list_clipped_h5_2b[i])

    # Calculate metrics
    l2a <- getLevel2AM(l2a)
    l2b_VPM <- getLevel2BVPM(l2b)
    l2b_PAIProfile <- getLevel2BPAIProfile(l2b)
    l2bPAVDProfile <- getLevel2BPAVDProfile(l2b)
    
    # Filtering usable data using quality_flag
    l2a <- subset(l2a,quality_flag >= 0.9)
    l2b_VPM <- subset(l2b_VPM,l2b_quality_flag >= 0.9)
    l2b_PAIProfile <- subset(l2b_PAIProfile,l2b_quality_flag >= 0.9)
    l2bPAVDProfile <- subset(l2bPAVDProfile,l2b_quality_flag >= 0.9)
    
    # Filtering usable data for Full power beam only
    l2a <- subset(l2a,substr(l2a$beam, 5, 6) == "01" | substr(l2a$beam, 5, 6) == "10")
    l2b_VPM <- subset(l2b_VPM,substr(l2b_VPM$beam, 5, 6) == "01" | substr(l2b_VPM$beam, 5, 6) == "10")
    l2b_PAIProfile <- subset(l2b_PAIProfile,substr(l2b_PAIProfile$beam, 5, 6) == "01" | substr(l2b_PAIProfile$beam, 5, 6) == "10")
    l2bPAVDProfile <- subset(l2bPAVDProfile,substr(l2bPAVDProfile$beam, 5, 6) == "01" | substr(l2bPAVDProfile$beam, 5, 6) == "10")
    
   
    
        # Converting shot_number as "integer64" to "character"
        l2a$shot_number <- paste0(l2a$shot_number)
        l2b_VPM$shot_number <- paste0(l2b_VPM$shot_number)
        l2b_PAIProfile$shot_number <- paste0(l2b_PAIProfile$shot_number)
        l2bPAVDProfile$shot_number <- paste0(l2bPAVDProfile$shot_number)
        
        # add data names abbreviations 
        names(l2b_VPM) <- paste0('b',names(l2b_VPM))
        names(l2b_PAIProfile) <- paste0('PAI',names(l2b_PAIProfile))
        names(l2bPAVDProfile) <- paste0('PAVD',names(l2bPAVDProfile))
        
        # Merge files
        gedimetrics <- rbind(
          cbind(l2a,l2b_VPM, l2b_PAIProfile, l2bPAVDProfile)
        )
        
  
  # Converting as data.table to SpatialPointsDataFrame
        tryCatch({metrics_spdf<-SpatialPointsDataFrame(
          cbind(gedimetrics$blongitude_bin0,
                gedimetrics$blatitude_bin0),
          data=gedimetrics)
        
        if (nrow(gedimetrics) <= 0) {
        }
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n", labels =  print(paste0("#Attention: ", basename(list_clipped_h5_2b[i]), " is execluded because of  Error in .bboxCoords(coords)"))
        )})
  
  #Exporting as ESRI shp  
    rgdal::writeOGR(metrics_spdf,
                    dsn=paste0(dir.GEDI_DATA, "/Output"),
                    layer= paste0("ALL_Metrics_", substr(basename(list_clipped_h5_2b[i]),18,54)),
                    driver = "ESRI Shapefile", overwrite=T)
  }
  return(metrics_spdf)
}

#Execute Extracting and Filtering metrics
Extract_Filter_GEDI <- Extract_Filter(list_clipped_h5_2a, list_clipped_h5_2b)

#--------------------------------------------------------------------------------------
#Merging the results and getting ready for analysis data


#listing the names
file_list <- list.files(dir.output, pattern = "*shp$", full.names = TRUE)

#reading shp(s)
shapefile_list <- lapply(file_list, read_sf)

#merging with sf output
all_data_shp <-sf::st_as_sf(data.table::rbindlist(shapefile_list))


#spatial join with forest poly
    #convert aoi to df
     aoi_df <- as(aoi_shp, "sf")
     #spatial join
      analysis_ready_shp <- st_join(all_data_shp, aoi_df)
      
#Exporting 
      
    #convert to sp
      analysis_ready_shp_sp <- as_Spatial(analysis_ready_shp)
      # export as ESRI shp 
rgdal::writeOGR(analysis_ready_shp_sp, dsn=dir.output,
                layer= "analysis_ready_shp",
                driver = "ESRI Shapefile", overwrite=T)

#--------------------------------------------------------------------------------------
######################### Basic Stat and summary##############################
#1 Subset data for each type and create total observations summary
#2 Calculate metrics summary  
# RH98, RH95, PAI, PAI profile, PAVD, canopy cover, Foliage Height Diversity (FHD)

#------------------------------Start-----------------------####
# Read aoi shp
aoi_shp <- shapefile(file.path(dir.shapefiles,
                               shapefile_name))

# Read analysis_ready_shp and converting to df (skip this step if you executed the previous chunk)

analysis_ready_shp <- shapefile(file.path(dir.output,
                                          "analysis_ready_shp"))

analysis_ready_shp <- as(analysis_ready_shp, "sf")
#------------------------------Start-----------------------####

     # convert forest type id to integer 64 and make a vector of id
      analysis_ready_shp$id <- as.integer(analysis_ready_shp$id)
      q <- sort(unique(analysis_ready_shp$id), decreasing = FALSE)
     # set empty list to populate with summary for each type and metric
      Stat_summary <- list()
    
 
     #function to calculate metrics
      metric_stat <- function(x) {
        metrics = list(summary(x), sd = sd(x))
        return(metrics)
      }
      
      # calculate metrics for total observation (All types)
      #make a list to all type stat
      total_metric <- vector(mode = "list", length = 9)
      names(total_metric) <- c("rh95", "rh98", "pai", "pai_z", "pavd", "FHD", "cover", "total_observ.", "Eng.name")
      #subset
      rh95 <- analysis_ready_shp$rh95
      rh98 <- analysis_ready_shp$rh98
      pai <- analysis_ready_shp$bpai
      pai_z <- analysis_ready_shp$PAI_0_5
      pavd <- analysis_ready_shp$PAVD_0_
      FHD <- analysis_ready_shp$bfhd_nr
      cover <- analysis_ready_shp$bcover
      temp_list <- list(rh95, rh98, pai, pai_z, pavd, FHD, cover)
      #populate the total with number of observation and type name(all types)
      total_metric[[8]] <- nrow(analysis_ready_shp)
      total_metric[[9]] <- "ALL Types"
      #populate the rest of the metrics
           for (j in seq_along(temp_list)){
              total_metric[[j]] <- metric_stat(temp_list[[j]])
           }
   
      
      # Calculate metrics for each type separately
      for (i in min(q):max(q)){
      #subset according to types
      subset <- analysis_ready_shp %>% filter(id == i)
      #subset according to metrics
      rh95 <- subset$rh95
      rh98 <- subset$rh98
      pai <- subset$bpai
      pai_z <- subset$PAI_0_5
      pavd <- subset$PAVD_0_
      FHD <- subset$bfhd_nr
      cover <- subset$bcover

      
      # make a sublist to host the metrics' category for each type and populate it
      metric_list <- vector(mode = "list", length = 9)
      names(metric_list) <- c("rh95", "rh98", "pai", "pai_z", "pavd", "FHD", "cover", "total_observ.", "Eng.name")
      temp_list <- list(rh95, rh98, pai, pai_z, pavd, FHD, cover)
         #populate the total summary
         metric_list[[8]] <- nrow(subset)
         # extract the English name of the type  i and populate it
         metric_list[[9]] <- paste("Type_", i, " (", unique(subset$Type_Eng), ")", sep = "")
         #populate the rest of the metrics
         for (j in seq_along(temp_list)){
         metric_list[[j]] <- metric_stat(temp_list[[j]])
         }
         
     #Organizing the data into one list and generating names  
  
     #populate the main list  with types list
     Stat_summary[[i+1]] <- list(metric_list)
     #generate names for the lists
     names(Stat_summary[[i+1]]) <- paste("Type_", i, sep = "")
      
      }
      
      #populate the main list #with all types list 
      Stat_summary[[length(Stat_summary)+1]] <- total_metric  
      #name the last item as All-Types 
      names(Stat_summary[[length(Stat_summary)+1]]) <- "All Types"
     
      #print
      Stat_summary

         
####---------------PLOT and analyze results----------------------------------------

