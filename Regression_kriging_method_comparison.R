library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)


library(sf)
library(sp)
library(raster)
library(rgdal)
library(rasterVis)


library(caret)

library(randomForest)

setwd('C:/Users/lgorman/OneDrive/Desktop/PhD/Analysis/')


################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 

################################################################################ Preperation ################################################################################################################################## 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 




############################################# Functions to identify outliers  #############################################################################
TRUE_in_Vector<-function(vector){return (TRUE %in% vector)}
identify_outlier_rows_and_NAs<-function(column){
  
  column<-as.numeric(column)
  lower_quartile<-quantile(column,probs=0.25, na.rm=T)
  upper_quartile<-quantile(column,probs=0.75, na.rm=T)
  
  IQR<-upper_quartile-lower_quartile
  
  lower_outliers<-(column<(lower_quartile-1.5*IQR))
  upper_outliers<-(column>(upper_quartile+1.5*IQR))
  na_values<-is.na(column)
  
  indexing_data_frame<-data.frame(lower_outliers,upper_outliers,na_values)
  indexing_outliers<-apply(indexing_data_frame, MARGIN = 1,TRUE_in_Vector)
  
  return (indexing_outliers)
  
  
}
############################################# Preparing shapefile  #############################################################################
world_shapefile <- st_read("data/shapefiles/World/Countries_WGS84.shp")
crs<-st_crs(world_shapefile) # view the coordinate reference system
st_bbox(world_shapefile) #bounding box

Tanzania_Country_name<-grep("Tanzania", world_shapefile$CNTRY_NAME,value=T)
Tanzania_shapefile<-world_shapefile[world_shapefile$CNTRY_NAME==Tanzania_Country_name,]
world_shapefile<-NULL
############################################# Preparing raster dataset #############################################################################

raster_data_sets<-data.frame("raster_level_education"="data/raster/ClimAfr21_education_index/ClimAfr21_education index.tif",
                             "raster_level_infrastructure"="data/raster/ClimAfr23_infrastructure_index/ClimAfr23_infrastructure index.tif",
                             "raster_level_financial_capital"="data/raster/ClimAfr09_financial_capital_index/ClimAfr09_financial capital index.tif",
                             "raster_level_rurality"="data/raster/ClimAfr13_rurality_index/ClimAfr13_rurality index.tif",
                             "raster_level_technological_capital"="data/raster/ClimAfr08_technological_capital_index/ClimAfr08_technological capital index.tif",
                             "raster_level_household_technological_capital"="data/raster/ClimAfr22_household_technology_index/ClimAfr22_household technology index.tif",
                             "raster_level_richness"="data/raster/ClimAfr25_richness_index/ClimAfr25_richness index.tif",
                             #"raster_level_governance"="data/raster/ClimAfr26_governance_index/ClimAfr26_governance index.tif",
                             #"raster_level_gender_gap"="data/raster/ClimAfr15_gender_gap_index/ClimAfr15_gender gap index.tif",
                             #"raster_level_population_density"="data/raster/gridded_pop_world_population_density_2015/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_2pt5_min.tif",
                             "raster_level_time_to_market_20_k"="data/raster/Harvest_Choice_Market_Travel/TravelTimeToMarket_SSA_GeoTiff/traveltimetomarket_ssa_020k.tif",
                             "raster_level_time_to_market_50_k"="data/raster/Harvest_Choice_Market_Travel/TravelTimeToMarket_SSA_GeoTiff/traveltimetomarket_ssa_050k.tif",
                             "raster_level_time_to_market_100_k"="data/raster/Harvest_Choice_Market_Travel/TravelTimeToMarket_SSA_GeoTiff/traveltimetomarket_ssa_100k.tif",
                             "raster_level_time_to_market_250_k"="data/raster/Harvest_Choice_Market_Travel/TravelTimeToMarket_SSA_GeoTiff/traveltimetomarket_ssa_250k.tif",
                             "raster_level_time_to_market_500_k"="data/raster/Harvest_Choice_Market_Travel/TravelTimeToMarket_SSA_GeoTiff/traveltimetomarket_ssa_500k.tif",
                             #"raster_level_financial_development"="data/raster/ClimAfr24_financial_development_index/ClimAfr24_financial development index.tif",
                             "raster_level_institutional_capital"="data/raster/ClimAfr10_institutional_capital_index/ClimAfr10_institutional capital index.tif",
                             "raster_level_AEZ"="data/raster/Harvest_Choice_AEZ/AEZ16 r2.0 - TIF/AEZ16_CLAS--SSA.tif")
#consider a slop and a soil raster, possibly pest and disease map

if (exists("raster_stack")){raster_stack<-NULL}

counter<-1
for (raster_name in colnames(raster_data_sets))
{
  file_name_temp<-as.character(raster_data_sets[,raster_name])
  raster_temp<-raster(file_name_temp)
  if (counter==1)
  {
    raster_temp <- crop(raster_temp, Tanzania_shapefile)
    raster_temp <- mask(raster_temp, Tanzania_shapefile)
    
    raster_stack<-raster_temp
  }
  if (counter>1)
  {
    raster_temp <- projectRaster(raster_temp, raster_stack)
    raster_temp <- crop(raster_temp, Tanzania_shapefile)
    raster_temp <- mask(raster_temp, Tanzania_shapefile)
    raster_stack<-addLayer(raster_stack,raster_temp)
    
  }
  #assign(raster_name,raster_temp)
  counter<-counter+1
  
  raster_temp<-NULL
}

names(raster_stack)<-colnames(raster_data_sets)
plot(raster_stack) #rurality


for (raster_type in names(raster_stack))
{
  
  png(paste0("R_outputs/raster_pixel_distribution/",raster_type,".png"))
  par(mar=c(5,5,5,5))
  hist(subset(raster_stack,raster_type),
       main = paste0("Distribution of ",raster_type," raster values"),
       xlab = "raster value", ylab = "Frequency",
       col = "darkred")
  dev.off()
  
}

#' from the plots it is clear that the following variables show no variation over the country
#' financial development
#' gender gap
#' governance


############################################# Preparing point dataset #############################################################################


data<-read.csv("data/processed/point_data_processed.csv", na.strings = c("NaN", "n/a"))

columns_to_extract<-c("GPS_LON", "GPS_LAT", "TVA_USD_PPP_pmae_pday", "LandCultivated","HHsizeMAE", "score_HDDS_BadSeason" ,names(raster_stack))
#data$score_HDDS_BadSeason

X_Y<-data[,columns_to_extract]
nrow(X_Y)
str(X_Y)

X_Y<-data.frame(lapply(X_Y, function(x) as.numeric(as.character(x))))

X_Y<-data.table(X_Y)
X_Y$GPS_LON<-jitter(X_Y$GPS_LON,0.00000001)
X_Y$GPS_LAT<-jitter(X_Y$GPS_LAT,0.00000001)

X_Y<-st_as_sf(x=X_Y, coords = c("GPS_LON", "GPS_LAT"),crs = 4326, agr = "constant")  

X_Y_tanzania<-suppressWarnings(st_intersection(X_Y,Tanzania_shapefile)) #st_geometry can also be used to extract the exact geometry

X_Y_tanzania_df<-data.frame(X_Y_tanzania)

str(X_Y_tanzania_df)





############################################# Grid Construction #############################################################################

number_of_gridcells<-100

# choose either hrid orizontal or vertical, otherwize will end up with results which are offset.
horizontal_extent_Tanzania<-diff(st_bbox(Tanzania_shapefile)[c("xmin","xmax")])
vertical_extent_Tanzania<-diff(st_bbox(Tanzania_shapefile)[c("ymin","ymax")])
grid_size<-horizontal_extent_Tanzania/number_of_gridcells

cellsize<-c(horizontal_extent_Tanzania/number_of_gridcells,vertical_extent_Tanzania/number_of_gridcells)
offset<-st_bbox(Tanzania_shapefile)[c("xmin","ymin")] #This is the lower left coordinate of the bounding box 

#grid<-  st_bbox(Tanzania_shapefile) %>% st_as_stars(dx=grid_size, dy=grid_size, crs=st_crs(Tanzania_shapefile)) %>% st_crop(Tanzania_shapefile)
grid<-  st_bbox(raster_stack) %>% st_as_stars(dx=grid_size, dy=grid_size, crs=st_crs(Tanzania_shapefile)) %>% st_crop(Tanzania_shapefile)


plot(grid)
plot(Tanzania_shapefile, add=T, fill=rgb(0,0,0,0))



################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 

################################################################################ Comparing regression and kriging techniques ################################################################################################################################## 
################################################################################################################################################################################################################################# 
################################################################################################################################################################################################################################# 


############################################# Selecting target variable, removing outliers from TARGET VARIABLE ONLY and conducting train_test_split #############################################################################

predicted_variable<-"LandCultivated"

data_frame_for_testing<-X_Y_tanzania_df[,c(predicted_variable,names(raster_stack), "geometry"),]

outlier_rows<-identify_outlier_rows_and_NAs(data_frame_for_testing[,predicted_variable]) # identifying outliers in the target variable
data_frame_for_testing<-data_frame_for_testing[outlier_rows==F,]

predictors<-names(raster_stack)


row_index<-1:nrow(X_Y_tanzania_df_no_outliers)

set.seed(123)

train_set_index<-sample(1:nrow(X_Y_tanzania_df_no_outliers), floor(0.8*nrow(X_Y_tanzania_df_no_outliers)))
train_set_index_boolean<-row_index%in%train_set_index
test_set_index<-train_set_index_boolean==F


projection_string<-proj4string(raster_stack)

survey_points_spatial<- SpatialPointsDataFrame(coords = st_coordinates(data_frame_for_testing$geometry), # Alternatively could write data_frame_for_testing[,c("GPS_LON", "GPS_LAT")]
                                               data =   data_frame_for_testing,
                                               proj4string = CRS(projection_string))

par(mfrow=c(1,2))
plot(Tanzania_shapefile$geometry, main="Tanzania",
     xlab = "Longitude", ylab="Latitude")
plot(survey_points_spatial, add=TRUE)
hist(data_frame_for_testing[,colnames(data_frame_for_testing)==predicted_variable], xlab= predicted_variable, main=predicted_variable, ylab="Frequency")
dev.off()







ordinary_kriging_error_list<-c()
random_forest_error_list<-c()
random_forest_ordinary_kriging_error_list<-c()
GLM_error_list<-c()
GLM_ordinary_kriging_error_list<-c()
GAM_error_list<-c()
GAM_ordinary_kriging_error_list<-c()


set.seed(456)
 


rows<-1:nrow(data_frame_for_testing)

number_of_folds<-10
fold_indexes<-createFolds(rows, k=number_of_folds, list=T, returnTrain = T) # caret function for generating kfolds


for (kfold in 1:number_of_folds)##############################################################################################################################################
{
  print(paste0(kfold, " fold"))
  if (kfold<10)
  {
    fold_subset<-paste0("Fold0",kfold)
  }
  if (kfold==10)
  {
    fold_subset<-paste0("Fold",kfold)
  }
  train_set_index<-fold_indexes[[fold_subset]]
  
  
  training_set_data_frame<-data_frame_for_testing[train_set_index,]
  training_set_spatial<-survey_points_spatial[train_set_index,]
  
  test_set_data_frame<-data_frame_for_testing[!test_set_index,]
  test_set_spatial<-survey_points_spatial[!test_set_index,]
  
################################################################################ Ordinary Kriging ################################################################################################################################## 


# Form of model indicates that the variance will be only dependent on spatial distance for the variogram, not on any potential covariates.
  form_of_model<- as.formula(
    paste(predicted_variable, 
          1, 
          sep = " ~ "))
  
  # setting parameters for the variogram
  # For explanation of variogram parameters please see: https://vsp.pnnl.gov/help/Vsample/Kriging_Variogram_Model.htm
  model<-"Exp" #could use gaussian or spherical, these are the most common
  psill<-3 #the height at which the variogram is expected to flatten out
  nugget<-0.5 # the inital variance expected at close quarters (could be a measure of measurement error or intrinsic local heterogeneity)
  range<-50 #the distance (along the x-axis) where the variogram will flatten out
  
  variogram_model <- vgm( model=model,psill=psill, nugget=nugget, range=range) # create model
  variagram_points<- variogram(form_of_model,training_set_spatial) # calculates sample variogram values 
  variogram_fit_OLS<-fit.variogram(variagram_points, model = variogram_model,  fit.method = 6)
  
  png(paste("R_outputs/Regression_Kriging_Results/variogram",predicted_variable,model,".png", sep="_"))
  plot(variagram_points,variogram_fit_OLS, main=paste0("Variogram for ", predicted_variable,"\nmodel: '",model,"'\npsill: '",psill, "'\nnugget: '", nugget,"'\nrange: '", range,"'"))
  dev.off()
  
  
  ordinary_kriging <- krige(formula = form_of_model ,
              locations = training_set_spatial, 
              model = variogram_fit_OLS,
              newdata = test_set_spatial,
              debug.level = 0)
  
  ordinary_kriging_predictions <- ordinary_kriging@data$var1.pred
  ordinary_kriging_error <- sqrt(mean((ordinary_kriging_predictions - test_set_data_frame[,predicted_variable])^2))
  ordinary_kriging_error_list<-c(ordinary_kriging_error_list,ordinary_kriging_error)
################################################################################  Random Forest Calibration ################################################################################################################################## 
  
  
  
  random_forest_model <- randomForest(y = training_set_data_frame[, predicted_variable], 
                     x = training_set_data_frame[, predictors],
                     ntree = 500,
                     mtry = 2)
  
  random_forest_predictions <- predict(random_forest_model, newdata = test_set_data_frame, type="response")
  random_forest_error<-sqrt(mean((random_forest_predictions - test_set_data_frame[,predicted_variable])^2))
  random_forest_error_list<-c(random_forest_error_list,random_forest_error)
  
  
  ################################################################################ Ordinary Kriging with Random Forest ################################################################################################################################## 
  
  
  resid.RF <- function(x) return(x$y - x$predicted)
  random_forest_residuals<-resid.RF(random_forest_model)
  
  
  temporary_spatial_training_set<-training_set_spatial
  temporary_spatial_training_set@data<-cbind(temporary_spatial_training_set@data, random_forest_residual=random_forest_residuals)
  
  # This time presuming that the residuals (from the random forest) are dependent on spatial autocorrelation
  form_of_model<- as.formula(
    paste("random_forest_residuals", 
          1, 
          sep = " ~ "))
  
  
  model<-"Exp" #could use gaussian or spherical, these are the most common
  psill<-3 #the height at which the variogram is expected to flatten out
  nugget<-0.5 # the inital variance expected at close quarters (could be a measure of measurement error or intrinsic local heterogeneity)
  range<-50 #the distance (along the x-axis) where the variogram will flatten out
  
  variogram_model <- vgm( model=model,psill=psill, nugget=nugget, range=range) # create model
  variagram_points<- variogram(form_of_model,temporary_spatial_training_set) # calculates sample variogram values 
  variogram_fit_OLS<-fit.variogram(variagram_points, model = variogram_model,  fit.method = 6)
  
  png(paste("R_outputs/Regression_Kriging_Results/variogram_RF_residuals",predicted_variable,model,".png", sep="_"))
  plot(variagram_points,variogram_fit_OLS, main=paste0("Variogram for ", predicted_variable,"\nmodel: '",model,"'\npsill: '",psill, "'\nnugget: '", nugget,"'\nrange: '", range,"'"))
  dev.off()
  
  
  
  Random_Forest_Kriging <- krige(formula = form_of_model ,
                 locations = temporary_spatial_training_set, 
                 model = variogram_fit_OLS,
                 newdata = test_set_spatial,
                 debug.level = 0)
  
  random_forest_ordinary_kriging_predictions <- random_forest_predictions + Random_Forest_Kriging@data$var1.pred #LOOK INTO MORE Adding the random-forest predictions with the Random-Forest Ordinary Kriging predictions.  random_forest_error_list<-c(random_forest_error_list,random_forest_error)
  random_forest_ordinary_kriging_error <- sqrt(mean((random_forest_ordinary_kriging_predictions - test_set_data_frame[,predicted_variable])^2))
  random_forest_ordinary_kriging_error_list<-c(random_forest_ordinary_kriging_error_list,random_forest_ordinary_kriging_error)
  
  
  
  ################################################################################ GLM calibration ################################################################################################################################## 
  form_of_model<- as.formula(
    paste(predicted_variable,
          paste(predictors, collapse = " + "), 
          sep = " ~ "))
  #print(form_of_model)
  
  
  
  GLM<- glm(formula=form_of_model, data = training_set_data_frame)
  
  GLM_predictions<-predict(GLM, newdata=test_set_data_frame, type="response")
  GLM_error<-sqrt(mean((GLM_predictions - test_set_data_frame[,predicted_variable])^2))
  GLM_error_list<-c(GLM_error_list,GLM_error)
  
  ################################################################################ Ordinary kriging with GLM ################################################################################################################################## 
  
  
  temporary_spatial_training_set<-training_set_spatial
  temporary_spatial_training_set@data<-cbind(temporary_spatial_training_set@data, GLM_residual=resid(GLM))
  
  
  form_of_model<- as.formula(
    paste("GLM_residual", 
          1, 
          sep = " ~ "))
  
  model<-"Exp" #could use gaussian or spherical, these are the most common
  psill<-3 #the height at which the variogram is expected to flatten out
  nugget<-0.5 # the inital variance expected at close quarters (could be a measure of measurement error or intrinsic local heterogeneity)
  range<-50 #the distance (along the x-axis) where the variogram will flatten out
  
  variogram_model <- vgm( model=model,psill=psill, nugget=nugget, range=range) # create model
  variagram_points<- variogram(form_of_model,temporary_spatial_training_set) # calculates sample variogram values 
  variogram_fit_OLS<-fit.variogram(variagram_points, model = variogram_model,  fit.method = 6)
  
  png(paste("R_outputs/Regression_Kriging_Results/variogram_GLM_residuals",predicted_variable,model,".png", sep="_"))
  plot(variagram_points,variogram_fit_OLS, main=paste0("Variogram for ", predicted_variable,"\nmodel: '",model,"'\npsill: '",psill, "'\nnugget: '", nugget,"'\nrange: '", range,"'"))
  dev.off()
  
  
  
  
  GLM_ordinary_kriging <- krige(formula = form_of_model ,
                  locations = temporary_spatial_training_set, 
                  model = variogram_fit_OLS,
                  newdata = test_set_spatial,
                  debug.level = 0)
  
  GLM_ordinary_kriging_predictions <- GLM_predictions + GLM_ordinary_kriging@data$var1.pred
  GLM_ordinary_kriging_error <- sqrt(mean((GLM_ordinary_kriging_predictions - test_set_data_frame[,predicted_variable])^2))
  GLM_ordinary_kriging_error_list<-c(GLM_ordinary_kriging_error_list,GLM_ordinary_kriging_error)
  
  ################################################################################ GAM Calibration ################################################################################################################################## 
  form_of_model<- as.formula(
    paste(predicted_variable,
          paste(predictors, collapse = " + "), 
          sep = " ~ "))
  #print(form_of_model)
  
  GAM <- gam(formula = form_of_model, data = training_set_data_frame)
  
  GAM_predictions<-predict(GAM, newdata=test_set_data_frame, type="response")
  GAM_error<-sqrt(mean((sqrt(mean((GAM_predictions - test_set_data_frame[,predicted_variable])^2)))^2))
  GAM_error_list<-c(GAM_error_list,GAM_error)
  
  
  ################################################################################ Ordinary kriging with GAM ################################################################################################################################## 
  
  
  temporary_spatial_training_set<-training_set_spatial
  temporary_spatial_training_set@data<-cbind(temporary_spatial_training_set@data, GAM_residual=resid(GAM))
  
  
  form_of_model<- as.formula(
    paste("GAM_residual", 
          1, 
          sep = " ~ "))
  
  model<-"Exp" #could use gaussian or spherical, these are the most common
  psill<-3 #the height at which the variogram is expected to flatten out
  nugget<-0.5 # the inital variance expected at close quarters (could be a measure of measurement error or intrinsic local heterogeneity)
  range<-50 #the distance (along the x-axis) where the variogram will flatten out
  
  variogram_model <- vgm( model=model,psill=psill, nugget=nugget, range=range) # create model
  variagram_points<- variogram(form_of_model,temporary_spatial_training_set) # calculates sample variogram values 
  variogram_fit_OLS<-fit.variogram(variagram_points, model = variogram_model,  fit.method = 6)
  
  png(paste("R_outputs/Regression_Kriging_Results/variogram_GAM_residuals",predicted_variable,model,".png", sep="_"))
  plot(variagram_points,variogram_fit_OLS, main=paste0("Variogram for ", predicted_variable,"\nmodel: '",model,"'\npsill: '",psill, "'\nnugget: '", nugget,"'\nrange: '", range,"'"))
  dev.off()
  
  
  GAM_ordinary_kriging <- krige(formula = form_of_model ,
                  locations = temporary_spatial_training_set, 
                  model = variogram_fit_OLS,
                  newdata = test_set_spatial,
                  debug.level = 0)
  
  
  GAM_ordinary_kriging_predictions <- GAM_predictions + GAM_ordinary_kriging@data$var1.pred
  GAM_ordinary_kriging_error <- sqrt(mean((GAM_ordinary_kriging_predictions - test_set_data_frame[,predicted_variable])^2))
  GAM_ordinary_kriging_error_list<-c(GAM_ordinary_kriging_error_list,GAM_ordinary_kriging_error)

} ##############################################################################################################################################
error_measures<-data.frame("Ordinary_Kriging"=ordinary_kriging_error_list,
           "Random_Forest"=random_forest_error_list,
           "Random_Forest_Kriging"=random_forest_ordinary_kriging_error_list,
           "GLM"=GLM_error_list,
           "GLM_Kriging"=GLM_ordinary_kriging_error_list,
           "GAM"=GAM_error_list,
           "GAM_Kriging"=GAM_ordinary_kriging_error_list)

summary_of_kfold<-data.frame(lapply(error_measures, function(x) mean(x)))
summary_of_kfold<-rbind(summary_of_kfold,data.frame(lapply(error_measures, function(x) sd(x))))
row.names(summary_of_kfold)<-c("mean","standard_deviation")                   





################################################################################################################################################################################################################################################ 
################################################################################################################################################################################################################################################ 
################################################################################################################################################################################################################################################ 
################################################ FINAL PREDICTIONS based on GAM_Kriging ############################################################

form_of_model<- as.formula(
  paste(predicted_variable,
        paste(predictors, collapse = " + "), 
        sep = " ~ "))


GAM <- gam(formula = form_of_model, data = data_frame_for_testing)
GAM_predictions_from_raster <- predict(raster_stack, GAM, type="response")


raster_as_spatial_pixels <- as(raster_stack[[1]], "SpatialPixelsDataFrame") # this step is needed for kriging over the raster

# Now need to interpolate the regression residuals and then add them back in to the regression results

# Create a temporary SpatialPointsDF object to store GAM residuals
temp_spatial_data <- survey_points_spatial
crs(temp_spatial_data) <- crs(raster_as_spatial_pixels)
temp_spatial_data@data <- cbind(temp_spatial_data@data, GAM_residual = resid(GAM))


form_of_model<- as.formula(
  paste("GAM_residual", 
        1, 
        sep = " ~ "))


variogram_model <- vgm(model  = "Exp", psill  = 0.15, range  = 10, nugget = 0.01)
variogram_points <- variogram(form_of_model, temp_spatial_data)
variogram_fit_OLS <- fit.variogram(variogram_points, model = variogram_model,  fit.method = 6)

plot(variogram_points, variogram_fit_OLS, main="Semi-variogram of GAM residuals")


GAM_ordinary_kriging_all_data<-krige(formula = form_of_model ,
                                     locations = temp_spatial_data, 
                                     model = variogram_fit_OLS,
                                     newdata = raster_as_spatial_pixels)





