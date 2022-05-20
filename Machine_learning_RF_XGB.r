#############################################################
# clear and extend memory limit 
#############################################################
rm(list=ls()) 
gc()
memory.size()
memory.limit()
memory.limit(size = 1280000)
#############################################################
# get needed Packages
#############################################################
require(parallel)
require(doParallel)
require(foreach)
require(iterators)
require(dplyr)
require(ggplot2)
require(caret)
require(randomForest)
require(xgboost)
require(vegan)
require(formattable)

#############################################################
# Register cores to allow parellel processing and improve performance
#############################################################

cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)


#############################################################
#Import data, check and do some cleaning
#############################################################

DD <- read.csv(file = "D:/Esmaeel GEDI R/Latest/Latest/Data_peninsular_Forest_mask.csv")
TT <- read.csv(file = "D:/Esmaeel GEDI R/Latest/Latest/Data_sabah_Forest_mask.csv")


DD<- rbind(TT,DD)
DD <- na.omit(DD)



#choose initial variables 

DD<-DD[,c('GEDI90',
          'P_PET', 'AMT',#'MTDQ', 'MTWQ' ,#'PWM'
          'Elevation', 'Slope',
          'MeanCurvature',
          'Aspect' #'Eastness', 'Northness' ,'Hillshade',
          #  'GaussianCurvature', 'MeanCurvature', 
          #'VerticalCurvature','HorizontalCurvature', 
          #'#'MaximalCurvature', 'MinimalCurvature',
)]


# round and tidy it up
DD$Slope = round(DD$Slope,1)
DD$MeanCurvature  = DD$MeanCurvature * 100
DD$MeanCurvature = round(DD$MeanCurvature,1)
DD$Aspect = round(DD$Aspect,1)
DD$Elevation = floor(DD$Elevation)



#Clean the data  
DD <- na.omit(DD)
DD <- DD %>% filter(DD$P_PET > 0)
DD <- DD %>% filter(DD$AMT > 0)

# Check correaltion 
#ggpairs(DD)

#cor2pcor(cov(DD))
#############################################################
#Split data; training and validation samples
#############################################################

set.seed(2000)

# take sample in case you want to play and test

TT <- DD
DD<-DD[sample(seq_len(nrow(DD)), size = 1000), ]
DD <- TT


#split
TrainDD <- DD[sample(seq_len(nrow(DD)), size = floor(0.8 * nrow(DD))), ]
ValidDD <- DD[-sample(seq_len(nrow(DD)), size = floor(0.8 * nrow(DD))), ]

y = TrainDD[,1]
x = TrainDD[,2:ncol(DD), drop =FALSE]



#############################################################
#Train models :functions
#############################################################
TrainModel<-function(method){
  
  # Set-up timer to check how long it takes to run the model
  timeStart <- proc.time()
  set.seed(2000)
  
  # Train the Linear model
  if (method=="glm"){
    model<- glm(y ~ ., data = x, family = gaussian)
  }
  # Train the RF model
  if (method=="rf"){
    model<- randomForest(y=y, x=x, ntree = 500, 
                         trControl = trainControl(method="cv", number=5, verboseIter = TRUE, allowParallel = TRUE)
    )
  }
  # Train the xgbTree model
  if (method=="xgbTree"){
    model<- train( y= y,x= x,
                   trControl = trainControl(
                     method = 'cv', number = 5, verboseIter = TRUE, allowParallel = TRUE
                   ),
                   tuneGrid = expand.grid(
                     nrounds = 1500,#c(500,1000,1500), # number of trees
                     max_depth = 10,#c(2,4,6),
                     eta = 0.3, # learning rate
                     gamma = 0, # pruning 
                     colsample_bytree = 1,
                     min_child_weight = 1,
                     subsample= 1
                   ),
                   method = 'xgbTree',
                   verbose= TRUE)
  }
  # Train the xgbDART model
  if (method=="xgbDART"){
    model<- train(y= y,x= x, 
                  trControl = trainControl(
                    method = 'cv', number = 5, verboseIter = TRUE, allowParallel = TRUE
                  ),
                  tuneGrid = expand.grid(
                    nrounds = 1500,#c(500,1000,1500), # number of trees
                    max_depth = 10,#c(2,4,6),
                    eta = 0.3, # learning rate
                    gamma = 0, # pruning 
                    colsample_bytree = 1,
                    min_child_weight = 1,
                    subsample= 1,
                    rate_drop = 0.1, skip_drop = 0.5
                  ),
                  method = 'xgbDART',
                  verbose= TRUE)}
  
  #close Timer
  print(proc.time() - timeStart)
  
  
  return(model)
  
}

#############################################################
#Evaluation stat / plot models function
#############################################################
# calculate evaluation metrics 
StatModel <- function(model, data)
{
  
  y <- data[,1]
  y_pred <- predict(model, newdata = data)
  d <- y_pred - y 
  
  # RMSE
  RMSE <-sqrt( sum((d)^2)/length(y) )
  rRMSE <- 100 * RMSE / mean( y )
  # bias
  Bias<- mean(d)
  rBias <- 100 * Bias / mean( y )
  # Mean absolute error
  MAE<- sum(abs(d))/length(y)
  # Mean relative error
  MRE <- sum(sqrt(((d)/y)^2)/length(y))*100 
  # R & R squared
  R <- cor(y_pred,y)
  R2<-summary(lm(y~y_pred))$r.squared
  
  Stat<-data.frame( Stat=c("RMSE","rRMSE","Bias","rBias","MAE","MRE","R","R2"),
                    Values=round(c(RMSE,rRMSE,Bias,rBias,MAE,MRE,R,R2),2)) 
  return(Stat)
}


#plot function 
PlotModel<- function(model, data){
  y <- data$GEDI90
  y_pred <- predict(model, newdata = data)
  
  ggplot(data, aes(x= y, y = y_pred, ymin=0, ymax=90, xmin=0, xmax=90)) +
    geom_point(col = "midnightblue", size = 1.5, alpha = 0.2) + coord_equal() + 
    stat_density_2d(geom = "polygon", aes(fill = ..level..)) + 
    scale_fill_gradient(low = "midnightblue", high = "goldenrod2") +
    #  geom_abline(intercept=0, slope=1,col="red") +
    geom_smooth(method = "lm", formula = y ~ x) + 
    labs(x='Observed Canopy Height', y='Predicted Canopy Height', title='Predicted vs. Observed') +
    theme_dark()+ 
    annotate("text", x = 70, y = 86,label = paste0("RMSE=", StatModel(model, data)[1,2],"m"))+
    annotate("text", x = 70, y = 80,label = paste0("rRMSE=",StatModel(model, data)[2,2]," %"))+ 
    annotate("text", x = 70, y = 74,label = paste0("Bias=",StatModel(model, data)[3,2],"m"))+ 
    annotate("text", x = 70, y = 68,label = paste0("adj.R2=",StatModel(model, data)[8,2]))
}  


#############################################################
#Train, plot and Save trained models
#############################################################
#train model
TrainedModel_glm <- TrainModel(method = 'glm') #Available methods: #glm  #rf #xgbTree #xgbDART
TrainedModel_rf <- TrainModel(method = 'rf')
TrainedModel_xgbTree <- TrainModel(method = 'xgbTree')
TrainedModel_xgbDART <- TrainModel(method = 'xgbDART')

#save model
saveRDS(TrainedModel, "___.rds") #enter name
saveRDS(TrainedModel_glm, "TrainedModel_glm.rds")
saveRDS(TrainedModel_rf, "Final_rf_newseed.rds")
saveRDS(TrainedModel_xgbTree, "Final_xgbTree.rds")
saveRDS(TrainedModel_xgbDART, "Final_xgbDART.rds")

#############################################################
#Shut down the parallel processing cluster
#############################################################

stopCluster(cluster)


#############################################################
#Plot training and validation performance / variable importance
#############################################################

#Choose model
TrainedModel <- TrainedModel_xgbDART
# or read saved one 
# TrainedModel<-readRDS("C:/Users/EOC/Desktop/ML_modelling_Final/TrainedModel_rf.rds")
summary(TrainedModel_xgbDART)



# Validation  # Look at TrainDD too!
# stat
StatModel(TrainedModel, ValidDD)
# scatter plot
PlotModel(TrainedModel, ValidDD)

# Variable importance
VarImp <- varImp(TrainedModel, scale = F)
ggplot(VarImp)


#############################################################
#Predict
#############################################################

#predict(TrainedModel, newdata = DD)


#############################################################
#
#############################################################
