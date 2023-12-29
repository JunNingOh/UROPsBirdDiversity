################
#Raster Data#

bot_countries <- st_read("Data/lvl3/level3.shp")

#libraries
library(sf)           
library(terra)          
library(ggplot2)
library(geodata)
library(tidyterra)
#####################
#Bioclimatic variables 1-19 https://www.worldclim.org/data/bioclim.html

#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#BIO3 = Isothermality (BIO2/BIO7) (×100)
#BIO4 = Temperature Seasonality (standard deviation ×100)
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO12 = Annual Precipitation
#BIO15 = Precipitation Seasonality (Coefficient of Variation)

###########################################################################
################# Data #############
Data_WorldClim <- geodata::worldclim_global(var = "bio", res = 10, path = "Data/WorldClimData")
names(Data_WorldClim) <- paste0("bio", 1:19)
Extracted_WorldClim <- Data_WorldClim[[c("bio1", "bio4", "bio12", "bio15")]]

clim_mean <- terra::extract(x = Extracted_WorldClim, y = bot_countries, fun = mean, na.rm = TRUE)
clim_mean <- clim_mean[,-1]

clim_mean_df <- cbind(bot_countries$LEVEL3_COD,clim_mean,bot_countries$geometry)
clim_mean_df <- st_as_sf(clim_mean_df)

###########################################################################
ggplot(data = clim_mean_df) +
  geom_sf(aes(fill = bio15)) +
  scale_fill_gradient(low = 'blue',high = 'red')
