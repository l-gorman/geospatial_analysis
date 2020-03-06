library(corrplot)





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

X_Y<-data.table(X_Y)
X_Y$GPS_LON<-jitter(X_Y$GPS_LON,0.00000001)
X_Y$GPS_LAT<-jitter(X_Y$GPS_LAT,0.00000001)

X_Y<-st_as_sf(x=X_Y, coords = c("GPS_LON", "GPS_LAT"),crs = 4326, agr = "constant")  

X_Y_tanzania<-suppressWarnings(st_intersection(X_Y,Tanzania_shapefile)) #st_geometry can also be used to extract the exact geometry

X_Y_tanzania_df<-data.frame(X_Y_tanzania)

str(X_Y_tanzania_df)


predicted_variable<-"LandCultivated"



data_frame_for_testing<-X_Y_tanzania_df[,c(predicted_variable,names(raster_stack), "geometry"),]

#outlier_data_frame<-data.frame(lapply(data_frame_for_testing[,-which(colnames(data_frame_for_testing)=="geometry")], function(x) identify_outlier_rows_and_NAs(x))) #line to remove outliers from all variables
#colSums(outlier_data_frame,na.rm=T) #summing the number of outlier rows
#outlier_rows<-rowSums(outlier_data_frame,na.rm = T)>0


############################################# Removing outliers from TARGET VARIABLE ONLY #############################################################################


outlier_rows<-identify_outlier_rows_and_NAs(data_frame_for_testing[,predicted_variable]) # identifying outliers in the target variable
X_Y_tanzania_df_no_outliers<-data_frame_for_testing[outlier_rows==F,]

############################################# Investigating correlations #############################################################################

correlations_within_data<- cor(X_Y_tanzania_df_no_outliers[,-which(colnames(data_frame_for_testing)%in%c("geometry","CNTRY_NAME"))], use="complete.obs")
corrplot(correlations_within_data, method = "color", type="upper")






############################################# Creating grid #############################################################################

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









#SpatialPointsDataFrame(coords      = climDataPT[,c("Lon","Lat")], 
#                       data        = grid,
#                       proj4string = CRS(proj4Str))


############################################# Ordinary kriging #############################################################################
predictors<-grep("raster_level_", colnames(X_Y_tanzania_df_no_outliers), value=T)

form_of_model<- as.formula(
  paste(predicted_variable, 
        1, 
        sep = " ~ "))
print(form_of_model)

X_Y_tanzania_df_no_outliers_df<-X_Y_tanzania_df_no_outliers


X_Y_tanzania_no_outliers<-st_as_sf(X_Y_tanzania_df_no_outliers)

variagram_for_kriging<- variogram(form_of_model,X_Y_tanzania_no_outliers)#  model=TheVariogramModel) # calculates sample variogram values 


# For explanation of variogram parameters please see: https://vsp.pnnl.gov/help/Vsample/Kriging_Variogram_Model.htm
model<-"Exp" #could use gaussian or spherical, these are the most common
psill<-3 #the height at which the variogram is expected to flatten out
nugget<-0.5 # the inital variance expected at close quarters (could be a measure of measurement error or intrinsic local heterogeneity)
range<-50 #the distance (along the x-axis) where the variogram will flatten out


TheVariogramModel <- vgm( model=model,psill=psill, nugget=nugget, range=range)
#TheVariogramModel <- vgm( model="Gau",psill=3, nugget=0.5, range=50)



variagram_for_kriging
plot(variagram_for_kriging)

TZ_variogram_fit <- fit.variogram(variagram_for_kriging, model=TheVariogramModel) # fit model

png(paste("R_outputs/Regression_Kriging_Results/variogram",predicted_variable,model,".png", sep="_"))
plot(variagram_for_kriging,TZ_variogram_fit, main=paste0("Variogram for ", predicted_variable,"\nmodel: '",model,"'\npsill: '",psill, "'\nnugget: '", nugget,"'\nrange: '", range,"'"))
dev.off()



############################################# Plotting ordinary kriging #############################################################################


krige_model<-krige(form_of_model,X_Y_tanzania_no_outliers, newdata=grid,model=TZ_variogram_fit) # don't specify data=X_Y_Final


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











residKrigMap <- krige(formula = formMod ,
                      locations = statPointsTMP, 
                      model = variogFitOLS,
                      newdata = rstPixDF)



#plot(variogram_TZ,TheVariogramModel)

#SHOULD LOOK INTO CROSS VARIOGRAMS, APPARENTLY THESE WILL HELP DETERMINE BEST FITTING.
# KRIGEST CAN BE USED FOR SPATIOTEMPORAL PREDICTIONS, SIMILAR IDEA BUT NEED TO PRODUCE A SPATIOTEMPORAL VARIOGRAM TO PRODUCE IT


############################################# Regression kriging (Also known as co-kriging) #############################################################################
#http://www.css.cornell.edu/faculty/dgr2/teach/R/CoKrigeR.pdf
#https://www.r-exercises.com/2018/03/31/advanced-techniques-with-raster-data-part-3-regression-kriging/?__cf_chl_jschl_tk__=9e5183be841387d7eeda88fd1d35ab94d580dc5f-1583423451-0-Ad9axwbJqE8eScmph1REH4VSFXcWclOrydg67xWNsGaW3Bk-h-YK_twMmQHRiptw_ftwgQ6kigD3cVmqxz-pAv9roIluJXgVkLia0ukISA3aBO1s-FbToT-Tb0cYpehVPMlijgLaW4dK-ELYCGoHs4YwKuZKQlVcr6MWC42u-8xcaGoXlet0Lq01CgtZuyHthqQw-Ur_fsPTIqUbULHD3eE-oTx4CgIJsOxUNOu1Byz31oep9sh-4HX18SYGY_z3lTlrwURwHCwrggL7YI-qlj9Gj9Xe_j1cMlvFPwwngOoVGNLKkpKumv93T0oADFyG_rlHJj91Q1aVxBzYG84pqyyaHy36xNUOs_r6CMPUWFiqFJo-Ka3pmmIIhqWAr2UfHg

# model this time takes all of the predictors into account

form_of_model<- as.formula(
  paste(predicted_variable,
        paste0(1," + ",
               paste(predictors, collapse = " + ")), 
        sep = " ~ "))
print(form_of_model)


variagram_for_kriging<- variogram(form_of_model,X_Y_tanzania_no_outliers)#  model=TheVariogramModel) # calculates sample variogram values 

