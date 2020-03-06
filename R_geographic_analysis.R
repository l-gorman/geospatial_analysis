library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gptk) # Gaussian processing
library(rstan) # bayesian modelling package
library(data.table)
# Geographic packages
library(sf)
library(sp)
library(raster)
library(rgdal)
library(rasterVis)


library(dplyr)
library(spData)
#library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications


############################################################################################################
setwd('C:/Users/lgorman/OneDrive/Desktop/PhD/Analysis/')

### Loading RSTAN options ####
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


############################################################################################################


#### Identify outliers ####
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



############################################################################################################
#Read in and plot with shapefile
############################################################################################################
world_shapefile <- st_read("data/shapefiles/World/Countries_WGS84.shp")
crs<-st_crs(world_shapefile) # view the coordinate reference system
st_bbox(world_shapefile) #bounding box

Tanzania_Country_name<-grep("Tanzania", world_shapefile$CNTRY_NAME,value=T)
Tanzania_shapefile<-world_shapefile[world_shapefile$CNTRY_NAME==Tanzania_Country_name,]
world_shapefile<-NULL

ggplot() + 
  geom_sf(data = Tanzania_shapefile, size = 1, color = "black", fill = rgb(0, 0, 0,0)) + 
  ggtitle("World Shapefile Boundary Plot") + 
  coord_sf() 
############################################################################################################
#Geographical plotting in R
############################################################################################################


# Load and explore raster
AEZ_rater_file_name<-"data/raster/Harvest_Choice_AEZ/AEZ16 r2.0 - TIF/AEZ16_CLAS--SSA.tif"
GDALinfo(AEZ_rater_file_name) # Get raster info before reading in
AEZ_raster<-raster(AEZ_rater_file_name)

plot(AEZ_raster)
Tanzania_AEZ <- crop(AEZ_raster, Tanzania_shapefile)
Tanzania_AEZ <- mask(Tanzania_AEZ, Tanzania_shapefile)
plot(Tanzania_AEZ)
factor_raster<-as.factor(Tanzania_AEZ)
raster_levels<-levels(factor_raster)
AEZ_raster<-NULL

levels(Tanzania_AEZ) # accesses the attributes of the raster (useful for converting from numeric to categorical)
minValue(Tanzania_AEZ)
maxValue(Tanzania_AEZ)
ncell(Tanzania_AEZ)

raster_levels<-levels(Tanzania_AEZ)[[1]]

par(mar=c(5,5,5,5))
hist(Tanzania_AEZ,
     maxpixels=ncell(DSM_HARV), #Ensures all pixels used in distribution rather than default of 100,000
     main="Distribution of AEZ categories in Tanzania raster",
     xlab="numerical category",
     ylab="Frequency",
     col="wheat")
dev.off()

#plot(Tanzania_AEZ)
#plot(Tanzania_shapefile,add=T, fill = rgb(0, 0, 0,0))
#Tanzania_AEZ_df<-as.data.frame(Tanzania_AEZ,xy=T) #convert raster to dataframe

ggplot() + 
  geom_sf(data = Tanzania_shapefile, size = 1, color = "black", fill = rgb(0, 0, 0,0)) + 
  ggtitle("World Shapefile Boundary Plot") + 
  coord_sf() 
#levelplot(Tanzania_AEZ) # ordinary rastervis plot

############################################################################################################


### Preparing raster data for ggplot2, converting it into a geom_tile.

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot<-gplot(Tanzania_AEZ)
plot_with_value<-merge(x=data.frame(plot$data),y=raster_levels, by.x="value", by.y="ID")
plot$data<-plot_with_value


plot_raster_and_shapefile<-ggplot()+geom_tile(data=plot$data,aes(x = x, y = y, fill = category))+scale_fill_manual(values = cbp1)
plot_raster_and_shapefile<-plot_raster_and_shapefile+geom_sf(data = Tanzania_shapefile, size = 1, color = "black", fill = rgb(0, 0, 0,0))
plot_raster_and_shapefile

############################################################################################################

# Adding point data
data<-read.csv("data/processed/point_data_processed.csv", na.strings = c("NaN", "n/a"))
AEZ_categories<-read.csv("data/AEZ_Categories/AEZ_categories.csv", na.strings = c("NaN", "n/a"))
AEZ_categories$X<-NULL

# Converting Numerical AEZ categories to Names
data<-merge(data, AEZ_categories, by.x="raster_level_AEZ", by.y="AEZ_Numbers")
most_important_variables<-read.csv("exploratory_outputs/LandCultivated_50_Most_important_RFE.csv")
colnames(data)<-gsub("AEZ_Names","raster_level_AEZ_names", colnames(data))

X_Y<-data.frame("Country"=data$Three_Letter_Country_Code_x,"AEZ"=data$raster_level_AEZ_names,"longitude"=data$GPS_LON, "latitude"=data$GPS_LAT, "X1"=data$TVA_USD_PPP_pmae_pday, "X2"=data$LandCultivated,"X3"=data$HHsizeMAE,"Y"=data$Livestock_Orientation)
X_Y<-data.table(X_Y)
X_Y$longitude<-jitter(X_Y$longitude,0.00000001)
X_Y$latitude<-jitter(X_Y$latitude,0.00000001)

X_Y<-st_as_sf(x=X_Y, coords = c("longitude", "latitude"),crs = 4326, agr = "constant")
plot(X_Y$geometry)

data<-NULL

X_Y_tanzania<-suppressWarnings(st_intersection(X_Y,Tanzania_shapefile)) #st_geometry can also be used to extract the exact geometry
unique(X_Y_tanzania$Country)  
table(X_Y_tanzania$Country) #There are three located in Rwanda could this be a GPS Error
X_Y<-NULL

# This also yields the same results #
#X_Y_Tanzania_test<-crop(as(X_Y, "Spatial"), as(Tanzania_shapefile,"Spatial"))
#X_Y_Tanzania_test<-st_as_sf(X_Y_Tanzania_test)



#plot(Tanzania_shapefile$geometry)
#plot(X_Y_tanzania, add=T)




library(RColorBrewer)
plot_points_raster_and_shapefile<-ggplot()+geom_tile(data=plot$data,aes(x = x, y = y, fill = category))+scale_fill_brewer(palette = "YlOrBr")
plot_points_raster_and_shapefile<-plot_points_raster_and_shapefile+geom_sf(data = Tanzania_shapefile, size = 1, color = "black", fill = rgb(0, 0, 0,0))
plot_points_raster_and_shapefile<-plot_points_raster_and_shapefile+geom_sf(X_Y_tanzania,mapping=aes(col=as.numeric(X2)))#+scale_color_gradient(palette = "YlOrBr")
plot_points_raster_and_shapefile
print("final line executed")




#########################################################################################################################################################################################################
#Kriging
#https://keen-swartz-3146c4.netlify.com/interpolation.html
library(gstat)
library(stars)



# Cleaning data #
X_Y_tanzania$X2<-as.numeric(as.character(X_Y_tanzania$X2))
X_Y_tanzania$Y<-as.numeric(as.character(X_Y_tanzania$Y))

outliers<-identify_outlier_rows_and_NAs(X_Y_tanzania$X2)
X_Y_FINAL<-X_Y_tanzania[outliers==F,]
X_Y_FINAL<-X_Y_FINAL[!is.na(X_Y_FINAL$X2),]






# Fitting Variogram #

# variogram models are commonly spherical, exponential and Gaussian models.  the variogram increases with distance at small distances and then levels off.
# This general shape is suggestive of a spatial correlation that is positive and strong at small distances and becomes less so as distances increase until reaching a certain distance d.  Pairs of points separated by a distance greater than d appear uncorrelated.




variogram_TZ<- variogram(X2~1, X_Y_FINAL, cressie=T) # calculates sample variogram values 
variogram_TZ
plot(variogram_TZ)

TheVariogramModel <- vgm( model="Gau",psill=3, nugget=0.5, range=50)#, range=60)
#plot(variogram_TZ,TheVariogramModel)

#SHOULD LOOK INTO CROSS VARIOGRAMS, APPARENTLY THESE WILL HELP DETERMINE BEST FITTING.
# KRIGEST CAN BE USED FOR SPATIOTEMPORAL PREDICTIONS, SIMILAR IDEA BUT NEED TO PRODUCE A SPATIOTEMPORAL VARIOGRAM TO PRODUCE IT

TZ_variogram_fit <- fit.variogram(variogram_TZ, model=TheVariogramModel) # fit model
plot(variogram_TZ,TZ_variogram_fit)







# Creating gridcells to fill for GP predictions

number_of_gridcells<-100

# choose either horizontal or vertical, otherwize will end up with results which are offset.
horizontal_extent_Tanzania<-diff(st_bbox(Tanzania_shapefile)[c("xmin","xmax")])
vertical_extent_Tanzania<-diff(st_bbox(Tanzania_shapefile)[c("ymin","ymax")])
grid_size<-horizontal_extent_Tanzania/number_of_gridcells

cellsize<-c(horizontal_extent_Tanzania/number_of_gridcells,vertical_extent_Tanzania/number_of_gridcells)
offset<-st_bbox(Tanzania_shapefile)[c("xmin","ymin")] #This is the lower left coordinate of the bounding box 

grid<-  st_bbox(Tanzania_shapefile) %>% st_as_stars(dx=grid_size, dy=grid_size, crs=st_crs(Tanzania_shapefile)) %>% st_crop(Tanzania_shapefile)

plot(grid)
plot(Tanzania_shapefile, add=T, fill=rgb(0,0,0,0))







# Kriging #


krige_model<-krige(X2 ~ 1,X_Y_FINAL, newdata=grid,model=TZ_variogram_fit) # don't specify data=X_Y_Final


krige_model_sf<-st_as_sf(krige_model)
ggplot()+ geom_sf(data = krige_model_sf, aes(fill = var1.pred))+
geom_sf(X_Y_tanzania,mapping=aes())+geom_sf(Tanzania_shapefile, size=2,mapping=aes(),colour="black", fill=rgb(0,0,0,0))

ggplot()+geom_sf(krige_model_sf, mapping=aes(), fill=rgb(0,0,0,0))+geom_sf(st_as_sf(grid, mapping),mapping =aes(), fill=rgb(0,0,0,0))

krige_model_sf$cov = 100 * sqrt( krige_model_sf$var1.var) / krige_model_sf$var1.pred #potential measure of uncertainty
krige_model_sf$sd = sqrt( krige_model_sf$var1.var) #standard deviation
 
ggplot()+ geom_sf(data = krige_model_sf, aes(fill = var1.var))+
  geom_sf(X_Y_tanzania,mapping=aes())+geom_sf(Tanzania_shapefile, size=2,mapping=aes(),colour="black", fill=rgb(0,0,0,0))


krige_model_df<-as.data.frame(krige_model_sf)
points_krige_model<-st_as_sf(krige_model, as_points=T)
points_krige_model<-cbind(data.frame(points_krige_model),st_coordinates(points_krige_model))
points_krige_model$sd<-sqrt(points_krige_model$var1.var)

kriging_plot<-ggplot()+geom_sf(data = krige_model_sf, aes(fill=var1.pred))+
  geom_contour(data = points_krige_model, aes(x=X,y=Y,z = sd), col="white")+geom_sf(X_Y_tanzania,mapping=aes())+geom_sf(Tanzania_shapefile, size=2,mapping=aes(),colour="black", fill=rgb(0,0,0,0))
ggsave("R_outputs/simple_kriging_TZ.png",kriging_plot )


#########################################################################################################################################################################################################

# Regression Kriging #

#' Regression kriging follows the same idea as above (simple kriging) but goes beyond making predictions only based on spatial autocorrelation
#' 
library(reshape2)
data<-read.csv("data/processed/point_data_processed.csv", colClasses = "character",na.strings = c("NaN", "n/a", "NA", "<NA>"))

# read and mask shapefile #

world_shapefile <- st_read("data/shapefiles/World/Countries_WGS84.shp")
crs<-st_crs(world_shapefile) # view the coordinate reference system
st_bbox(world_shapefile) #bounding box

Tanzania_Country_name<-grep("Tanzania", world_shapefile$CNTRY_NAME,value=T)
Tanzania_shapefile<-world_shapefile[world_shapefile$CNTRY_NAME==Tanzania_Country_name,]
world_shapefile<-NULL



#find all numeric data and remove variables with >50% NAs.
NA_faction_per_row<-reshape::melt(data.frame(lapply(data,function(x) sum(is.na(x)))))

variances_of_all_data<-reshape::melt(data.frame(lapply(data, function (x) var(as.numeric(as.character(x)), na.rm = T))))


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



#proj4string<-"+proj=longlat +datum=WGS84 +no_defs"

#AEZ_raster<-raster("data/raster/AEZ_2009/AEZ_FINAL.asc")

if (exists("raster_stack")){raster_stack<-NULL}
if (exists("counter")){raster_stack<-NULL}

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

columns_to_extract<-c("GPS_LON", "GPS_LAT", "TVA_USD_PPP_pmae_pday", "LandCultivated","HHsizeMAE", "score_HDDS_BadSeason" ,names(raster_stack))
#data$score_HDDS_BadSeason

X_Y<-data[,columns_to_extract]
nrow(X_Y)
str(X_Y)

X_Y<-data.frame(lapply(X_Y, function(x) as.numeric(as.character(x))))



#columns_to_extract[columns_to_extract %in% colnames(data)==F]
#X_Y<-data.frame("Country"=data$Three_Letter_Country_Code_x,"AEZ"=data$raster_level_AEZ_names,"longitude"=data$GPS_LON, "latitude"=data$GPS_LAT, "X1"=data$TVA_USD_PPP_pmae_pday, "X2"=data$LandCultivated,"X3"=data$HHsizeMAE,"Y"=data$Livestock_Orientation)

X_Y<-data.table(X_Y)
X_Y$GPS_LON<-jitter(X_Y$GPS_LON,0.00000001)
X_Y$GPS_LAT<-jitter(X_Y$GPS_LAT,0.00000001)

X_Y<-st_as_sf(x=X_Y, coords = c("GPS_LON", "GPS_LAT"),crs = 4326, agr = "constant")  
           
X_Y_tanzania<-suppressWarnings(st_intersection(X_Y,Tanzania_shapefile)) #st_geometry can also be used to extract the exact geometry

X_Y_tanzania_df<-data.frame(X_Y_tanzania)

str(X_Y_tanzania_df)

outlier_data.frame<-data.frame(lapply(X_Y_tanzania_df[,-which(colnames(X_Y_tanzania_df)=="geometry")], function(x) identify_outlier_rows_and_NAs(x)))
colSums(outlier_data.frame,na.rm=T)

outlier_rows<-rowSums(outlier_data.frame,na.rm = T)>0
X_Y_tanzania_df_no_outliers<-X_Y_tanzania_df[outlier_rows==F,]

correlations_within_data<- cor(X_Y_tanzania_df_no_outliers[,-which(colnames(X_Y_tanzania_df)%in%c("geometry","CNTRY_NAME"))], use="complete.obs")
#from the correlation matrix, it seems as though trying to predict dietary diversity might be the most useful exercise?




corrplot(correlations_within_data, method = "color", type="upper")



predicted_variable<-"LandCultivated"

X_Y<-st_as_sf(x=X_Y, coords = c("GPS_LON", "GPS_LAT"),crs = 4326, agr = "constant")  

X_Y_tanzania<-suppressWarnings(st_intersection(X_Y,Tanzania_shapefile)) #st_geometry can also be used to extract the exact geometry

X_Y_tanzania_df<-data.frame(X_Y_tanzania)


data_frame_for_testing<-X_Y_tanzania_df[,c(predicted_variable,names(raster_stack), "geometry"),]

#outlier_data_frame<-data.frame(lapply(data_frame_for_testing[,-which(colnames(data_frame_for_testing)=="geometry")], function(x) identify_outlier_rows_and_NAs(x)))
#colSums(outlier_data_frame,na.rm=T)

#outlier_rows<-rowSums(outlier_data_frame,na.rm = T)>0
outlier_rows<-identify_outlier_rows_and_NAs(data_frame_for_testing[,predicted_variable])
X_Y_tanzania_df_no_outliers<-data_frame_for_testing[outlier_rows==F,]

correlations_within_data<- cor(X_Y_tanzania_df_no_outliers[,-which(colnames(data_frame_for_testing)%in%c("geometry","CNTRY_NAME"))], use="complete.obs")
corrplot(correlations_within_data, method = "color", type="upper")


#### Creating variogram ######



variogram_TZ<- variogram(X2~1, X_Y_FINAL, cressie=T) # calculates sample variogram values 
variogram_TZ
plot(variogram_TZ)

TheVariogramModel <- vgm( model="Gau",psill=3, nugget=0.5, range=50)#, range=60)
#plot(variogram_TZ,TheVariogramModel)

#SHOULD LOOK INTO CROSS VARIOGRAMS, APPARENTLY THESE WILL HELP DETERMINE BEST FITTING.
# KRIGEST CAN BE USED FOR SPATIOTEMPORAL PREDICTIONS, SIMILAR IDEA BUT NEED TO PRODUCE A SPATIOTEMPORAL VARIOGRAM TO PRODUCE IT

TZ_variogram_fit <- fit.variogram(variogram_TZ, model=TheVariogramModel) # fit model
plot(variogram_TZ,TZ_variogram_fit)


