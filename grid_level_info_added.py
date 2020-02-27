import os
import pandas as pd
import math
import numpy as np
import matplotlib.pyplot as plt
import geopandas as gpd
#import gdal
import geopandas as gpd
from shapely.geometry import Point, Polygon, mapping
import rasterio
import rasterio.plot
import rasterio.mask # an important package which allows you to only extract points within your shapefile
import rasterstats # gives us raster summaries within a shapefile
from rasterstats import zonal_stats # gives us raster summaries within a shapefile


os.chdir('../')

# Defining filenames and resolution
resolution="100"
file_name="world_grid_"+resolution+".0km"
continent="Africa"
continent_file="africa_grid_"+resolution+".0km"

# Outlining which rasters to attach
rasters_to_attach={"education":os.path.join("data", "raster","ClimAfr21_education_index","ClimAfr21_education index.tif"),
                  "technological_capital":os.path.join("data", "raster","ClimAfr08_technological_capital_index","ClimAfr08_technological capital index.tif"),
                  "infrastructure":os.path.join("data", "raster","ClimAfr23_infrastructure_index","ClimAfr23_infrastructure index.tif"),
                  "conflictuality":os.path.join("data", "raster","ClimAfr27_conflictuality_index","ClimAfr27_conflictuality index.tif"),
                  #"soil":os.path.join("data", "raster","HWSD_RASTER","hwsd.bil"),
                  "cattle_density":os.path.join("data", "raster","cattle_density_map","Cattle10km_AD_2010_v2_1.tif"),
                  #"AEZ":os.path.join("data", "raster","ClimAfr_AEZ","ClimAf_1_1981_2050_lpjm_mir5_B1_AEZAI_ann_avgCUR05_10km.tif"),
                  "financial_capital":os.path.join("data", "raster","ClimAfr09_financial_capital_index","ClimAfr09_financial capital index.tif"),
                  "rurality":os.path.join("data", "raster","ClimAfr13_rurality_index","ClimAfr13_rurality index.tif"),
                  "gender_gap":os.path.join("data", "raster","ClimAfr15_gender_gap_index","ClimAfr15_gender gap index.tif"),
                  "household_technological_capital":os.path.join("data", "raster","ClimAfr22_household_technology_index","ClimAfr22_household technology index.tif"),
                  "financial_development":os.path.join("data", "raster","ClimAfr24_financial_development_index","ClimAfr24_financial development index.tif"),
                  "richness":os.path.join("data", "raster","ClimAfr25_richness_index","ClimAfr25_richness index.tif"),
                  "governance":os.path.join("data", "raster","ClimAfr26_governance_index","ClimAfr26_governance index.tif"),
                  #"slope":os.path.join("data", "raster","global_slope_median","plate09.bil"),
                  "population_density":os.path.join("data", "raster","gridded_pop_world_population_density_2015","gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_2pt5_min.tif"),
                  "institutional_capital":os.path.join("data", "raster","ClimAfr10_institutional_capital_index","ClimAfr10_institutional capital index.tif"),
                  "AEZ":os.path.join("data", "raster","AEZ_2009","AEZ_FINAL.asc"),
                  "time_to_market_20_k":os.path.join("data", "raster","Harvest_Choice_Market_Travel","TravelTimeToMarket_SSA_GeoTiff","traveltimetomarket_ssa_020k.tif"),
                  "time_to_market_50_k":os.path.join("data", "raster","Harvest_Choice_Market_Travel","TravelTimeToMarket_SSA_GeoTiff","traveltimetomarket_ssa_050k.tif"),
                  "time_to_market_100_k":os.path.join("data", "raster","Harvest_Choice_Market_Travel","TravelTimeToMarket_SSA_GeoTiff","traveltimetomarket_ssa_100k.tif"),
                  "time_to_market_250_k":os.path.join("data", "raster","Harvest_Choice_Market_Travel","TravelTimeToMarket_SSA_GeoTiff","traveltimetomarket_ssa_250k.tif"),
                  "time_to_market_500_k":os.path.join("data", "raster","Harvest_Choice_Market_Travel","TravelTimeToMarket_SSA_GeoTiff","traveltimetomarket_ssa_500k.tif")}
                  #"pests_and_disease":os.path.join("data", "raster","Harvest_Choice_Pests_and_Disease","Pests_and_Disease.tiff")}

raster_keys=[value for value, key in  rasters_to_attach.items()]
raster_labels=[key for value, key in  rasters_to_attach.items()]


# Function to merge a raster with shapefile
def merge_shape_file_with_raster(filepath,datatype, shapefile,raster_keys):
    with rasterio.open(filepath) as src:
        # can check the profile and type of src, but effectively is an open file command
        affine = src.transform # transforms the raster dataset into the coordinate reference system
        
        raster_array=rasterio.open(filepath).read(1) # why is this "1" here?
        if raster_keys!="AEZ":
            raster_array=raster_array.astype(float)
        #raster_array[raster_array<-3.4e+38]=np.nan
            raster_array[raster_array<-2e+9]=np.nan
        if raster_keys=="AEZ":
            raster_array=raster_array.astype(int)
        raster_statistics_for_grid = pd.DataFrame(zonal_stats(shapefile,raster_array ,affine=affine,stats=[ 'mean', 'median', 'std', "majority"])) 
        
        
        
# calculates raster statistics within boundary of shapefile
        raster_array=None
    raster_statistics_for_grid=pd.concat([shapefile,raster_statistics_for_grid],axis=1)
    grid_raster_merge=raster_statistics_for_grid
    raster_statistics_for_grid=None


    grid_raster_merge.columns=[column_name.replace("mean","mean_"+datatype) if column_name=="mean" else column_name  for column_name in grid_raster_merge.columns]
    grid_raster_merge.columns=[column_name.replace("median","median_"+datatype) if column_name=="median" else column_name for column_name in grid_raster_merge.columns]
    grid_raster_merge.columns=[column_name.replace("std","std_"+datatype) if column_name=="std" else column_name for column_name in grid_raster_merge.columns]
    grid_raster_merge.columns=[column_name.replace("majority","majority_"+datatype) if column_name=="majority" else column_name for column_name in grid_raster_merge.columns]

    return grid_raster_merge;

# looping through all rasters and adding to file
world_grid_old=gpd.read_file(os.path.join('data', 'shapefiles', continent_file))
world_grid=None
for loop_index in range(len(rasters_to_attach)):
    print(raster_keys[loop_index])
    world_grid_new=merge_shape_file_with_raster(filepath=raster_labels[loop_index], datatype=raster_keys[loop_index], shapefile=world_grid_old, raster_keys=raster_keys[loop_index])
    columns_to_add=["FID"]
    columns_to_add.extend(list(world_grid_new.columns.difference(world_grid_old.columns)))
    world_grid_old=pd.merge(world_grid_old,world_grid_new[world_grid_new.columns.intersection(columns_to_add)])  
    world_grid_new=None
processed_file_name="africa_grid_"+resolution+"processed.csv"
world_grid_old.to_csv(os.path.join("data","processed",processed_file_name))