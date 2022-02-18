library(MODIStsp)
library(raster)

# script for downloading, pre-processing and processing MODIS land cover data.
# WARNING - the download

# downloads MCD12Q1 LCR for 2001

MODIStsp(
  gui = FALSE,
  out_folder = "/home/chris/Documents/data/deforecast/processed",
  selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
  bandsel = "LC1" ,
  user = "", # secret!
  password = "", # secret!
  download_range = "FULL",
  start_date = "2001.01.01",
  end_date = "2001.12.31",
  spatmeth = "tiles",
  start_x = 0,
  end_x = 35,
  start_y = 0,
  end_y = 17,
  output_proj = 4326,
  delete_hdf = TRUE,
  out_format = "GTiff",
  n_retries = 5,
  verbose = TRUE,
  parallel = TRUE
)

# and for 2020
MODIStsp(
  gui = FALSE,
  out_folder = "/home/chris/Documents/data/deforecast/processed",
  selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
  bandsel = "LC1" ,
  user = "",
  password = "",
  download_range = "FULL",
  start_date = "2020.01.01",
  end_date = "2020.12.31",
  spatmeth = "tiles",
  start_x = 0,
  end_x = 35,
  start_y = 0,
  end_y = 17,
  output_proj = 4326,
  delete_hdf = TRUE,
  out_format = "GTiff",
  n_retries = 5,
  verbose = TRUE,
  parallel = TRUE
)

# creates a matrix to reclassify forest (IGBP category 1-5) and not forest
reclass <- matrix(c(1:17, NA,rep(1,5), rep(NA,13)), nrow = 18,ncol =2)

setwd("/home/chris/Documents/data/deforecast/raw/LC2001/LC1/")
lc_2001 <- raster("MCD12Q1_LC1_2001_001.tif")
# does this for 2001
lc_2001_binary <- reclassify(lc_2001, reclass)
# and writes to disk
writeRaster(lc_2001_binary, "/home/chris/Documents/data/deforecast/processed/fc2001.tif")

setwd("/home/chris/Documents/data/deforecast/raw/LC2020/LC1/")
lc_2020 <- raster("MCD12Q1_LC1_2020_001.tif")
lc_2020_binary <- reclassify(lc_2020, reclass)
writeRaster(lc_2020_binary, "/home/chris/Documents/data/deforecast/processed/fc2020.tif")
