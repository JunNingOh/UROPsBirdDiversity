################
#Raster Data#

#libraries
library(sf)           
library(terra)          
library(ggplot2)
#####################
#Bioclimatic variables 1-19 https://www.worldclim.org/data/bioclim.html



Annual_mean_temp <- rast('wc2.1_10m_bio/wc2.1_10m_bio_1.tif')
names(Annual_mean_temp) <- c('Annual Mean Temp')
names(Annual_mean_temp)
summary(Annual_mean_temp)

rasterdf <- function(x, aggregate = 1) {
  resampleFactor <- aggregate        
  inputRaster <- x    
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the resampled raster
  resampledRaster <- rast(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor),
                          crs = crs(inputRaster))
  # Match to the extent of the original raster
  ext(resampledRaster) <- ext(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='near')
  # Extract cell coordinates into a data frame
  coords <- xyFromCell(y, seq_len(ncell(y)))
  # Extract layer names
  dat <- stack(values(y, dataframe = TRUE))
  # Add names - 'value' for data, 'variable' for different
  # layer names in a multilayer raster
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  dat
}

Annual_mean_temp_df <- rasterdf(Annual_mean_temp)
summary(Annual_mean_temp_df)

ggplot(data = Annual_mean_temp_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "Annual Mean Temp",
       x = "longitude", 
       y = "latitude") +
  theme(legend.position = "bottom")

