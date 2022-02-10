library(knitr)
library(sf)
library(tidyverse)
library(raster)
library(exactextractr)
library(rgeos)
library(parallel)

select <- dplyr::select
extract <- raster::extract

# data downloaded from protected planet https://www.protectedplanet.net/en
# 3 large shapefiles with polygons for all protected areas


# this reads the data
setwd("/home/chris/Documents/data/deforecast/")
wdpa_0 <- read_sf("./raw/wdpa/wdpa_0/WDPA_Jan2022_Public_shp-polygons.shp")
wdpa_1 <- read_sf("./raw/wdpa/wdpa_1/WDPA_Jan2022_Public_shp-polygons.shp")
wdpa_2 <- read_sf("./raw/wdpa/wdpa_2/WDPA_Jan2022_Public_shp-polygons.shp")

# combines the three shps and filters out marine areas

wdpa <- rbind(wdpa_0, wdpa_1, wdpa_2)
wdpa <- wdpa[wdpa$MARINE == 0,]

# reads in the binary modis lcr (see modispreprocessing.r)

lc <- raster("./processed/fc2020.tif")

# this extracts forest cover for each protected area.
# WARNING, takes a long time
fc <- exact_extract(x = lc, 
                    y = wdpa, 
                    fun = "mean",
                    default_value = 0)
fwdpa <- cbind(wdpa, fc)
# And filters protected areas by those which have more than 30% forest cover
fwdpa <- fwdpa[fwdpa$fc >= 0.3,]

#removes invalid geometries
valid <- st_geometry(fwdpa) %>% st_is_valid()

# calculates areas
areas <- cbind(fwdpa, valid) %>% dplyr::filter(valid == T) %>% st_area()

# converts from m2 to km2
fwdpa <- cbind(dplyr::filter(fwdpa, valid == T), areas) %>% mutate(areas = as.numeric(areas/1000000))

# filters for above 10km2
bfwdpa <- fwdpa %>% arrange(areas) %>%
  dplyr::filter(areas >= 10) %>%
  dplyr::select(WDPAID,NAME,ISO3,fc,areas)

#removes two problematic protected areas with dodgy geometries
bfwdpa <- bfwdpa[c(1:7193, 7195:15894,15896:16796),]
# gets the centres
bfwdp_points <- bfwdpa %>% st_centroid()

# writes to disk
write_sf(bfwdpa, "./processed/pa_polygon.shp", overwrite = T)
write_sf(bfwdp_points, "./processed/pa_centroid.shp", overwrite = T)
