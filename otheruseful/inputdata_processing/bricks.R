# sets up necessary packages
library(sf)
library(tidyverse)
library(raster)
library(plyr)
library(fasterize)
library(parallel)
select <- dplyr::select

setwd("/home/chris/Documents/data/deforecast")
#loads 2001 LC
lc <- raster("./processed//lc/fc2020.tif") # loads lc raster
#loads population
pop <- raster("./raw/ls2001/lspop2001/w001001.adf") # loads pop raster

# function for building a brick
buildingbricks <- function(x) {
  x <- st_simplify(x) %>% select(WDPAID); # simplifies the geometry and strips out all info apart from the id
  id <- as.character(x$WDPAID); # saves ids for later 
  zone <- floor((st_coordinates(x)[[1,1]] + 180) / 6) + 1 # gets the appropriate utm zone for each forest 
  crs <- paste0("+proj=utm +zone=",zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"); # creates a proj4s for the zone
  st_crs(x) <- "+proj=longlat +datum=WGS84 +no_defs"; # affirms the original crs
  xpa <- st_transform(x,crs) # reprojects to utm
  x5 <- st_buffer(xpa, dist = 5000) # creates a 5km buffer around the pa
  x10 <- st_buffer(x5, dist = 5000) # creates a further 5km buffer around 
  x10 <- st_difference(x10,x5) %>% select(WDPAID)
  x15 <- st_buffer(x10, dist = 5000)
  x15 <- st_difference(st_difference(x15,x10), x5) %>% select(WDPAID)
  x20 <- st_buffer(x15, dist = 5000)
  x20 <- st_difference(st_difference(st_difference(x20,x15),x10),x5) %>% select(WDPAID)
  x20_bb <- st_as_sfc(st_bbox(x20))
  x21 <- st_buffer(x20, dist = 1000)
  x21_bb <- st_as_sfc(st_bbox(x21))
  x21_bb_4326 <- st_transform(x21_bb, crs = "+proj=longlat +datum=WGS84 +no_defs")
  template_raster <- raster(
    xmn = round_any(min(st_coordinates(x20_bb)[,1]), accuracy = 500, f = floor),
    ymn = round_any(min(st_coordinates(x20_bb)[,2]), accuracy = 500, f = floor),
    xmx = round_any(max(st_coordinates(x20_bb)[,1]), accuracy = 500, f = ceiling),
    ymx = round_any(max(st_coordinates(x20_bb)[,2]), accuracy = 500, f = ceiling),  
    crs = crs,
    res = 500)
  lcs <- crop(lc, as(x21_bb_4326,"Spatial"), snap = "out")
  lcs <- projectRaster(lcs, crs = crs)
  lcs <- resample(lcs, template_raster, method = "ngb")
  pops <- crop(pop, as(x21_bb_4326, "Spatial"), snap = "out")
  raster::crs(pops) <- "+init=epsg:4326"
  pops <- projectRaster(pops, crs = crs)
  pops <- resample(pops, template_raster, method = "ngb")
  parea <- fasterize(xpa,pops)
  b <- brick(parea,lcs,pops)
  writeRaster(x = b, filename = paste0("./processed/brick2020/",id,".tif"), overwrite = T)
  return(b)
}

calibration_forests <- list.files("/home/chris/Documents/data/deforecast/results/calibration/1/")
calibration_forests <- gsub(".csv", "", calibration_forests)
pa <- read_sf("./processed/pa/pa_polygon.shp") # loads protected shape files
pa_calibration <- pa[pa$WDPAID %in% calibration_forests,]
pa_calibration <- split(pa_calibration, seq(nrow(pa_calibration)))

# loads protected area
pa <- split(pa, seq(nrow(pa))) # splits to list

# splits pas into manageable chunks for computing


pa1 <- pa[1:999]
pa2 <- pa[1000:1999]
pa3 <- pa[2000:2999]
pa4 <- pa[3000:3999]
pa5 <- pa[4000:4999]
pa6 <- pa[5000:5999]
pa7 <- pa[6000:6999]
pa8 <- pa[7000:7999]
pa9 <- pa[8000:8999]
pa10 <- pa[9000:9999]
pa11 <- pa[10000:10999]
pa12 <- pa[11000:11999]
pa13 <- pa[12000:12999]
pa14 <- pa[13000:13999]
pa15 <- pa[14000:14999]
pa16 <- pa[15000:15999]
pa17 <- pa[16000:16794]

# lays the bricks. 
# Mclapply doesn't work on windows but can be replaced with lapply or parLapply

mclapply(pa1, FUN = buildingbricks, mc.cores = 8)
mclapply(pa2, FUN = buildingbricks, mc.cores = 8)
mclapply(pa3, FUN = buildingbricks, mc.cores = 8)
mclapply(pa4, FUN = buildingbricks, mc.cores = 8)
mclapply(pa5, FUN = buildingbricks, mc.cores = 8)
mclapply(pa6, FUN = buildingbricks, mc.cores = 8)
mclapply(pa7, FUN = buildingbricks, mc.cores = 8)
mclapply(pa8, FUN = buildingbricks, mc.cores = 8)
mclapply(pa9, FUN = buildingbricks, mc.cores = 8)
mclapply(pa10, FUN = buildingbricks, mc.cores = 8)
mclapply(pa11, FUN = buildingbricks, mc.cores = 8)
mclapply(pa12, FUN = buildingbricks, mc.cores = 8)
mclapply(pa13, FUN = buildingbricks, mc.cores = 8)
mclapply(pa14, FUN = buildingbricks, mc.cores = 8)
mclapply(pa15, FUN = buildingbricks, mc.cores = 8)
mclapply(pa16, FUN = buildingbricks, mc.cores = 8)
mclapply(pa17, FUN = buildingbricks, mc.cores = 8)
mclapply(pa_calibration, FUN = buildingbricks, mc.cores = 8)
mclapply(pa_again, FUN = buildingbricks, mc.cores = 6)
# This takes ages (overnight) and sometimes crashes (esp for forests with strange geometries). 
# The following code finds the unprocessed forests and reruns.
# I had to rerun many times after the first 15000ish forests completed in the first go

ids <- read_sf("./processed/pa/pa_centroid.shp") %>% st_drop_geometry() %>% 
  select(WDPAID) %>% mutate(index = 1:16794, id = as.character(WDPAID)) 
done <- list.files("./processed/brick2020/")
done <- gsub(".tif", "", done)
'%!in%' <- function(x,y)!('%in%'(x,y))
notdone <- filter(ids, id %!in% done)
pa_again <- filter(pa, WDPAID %in% notdone$WDPAID)
pa_again <- split(pa_again, seq(nrow(pa_again))) # splits to list
buildingbricks(pa_again[[2]])
