# bricks2matrix
rm(list = ls())
library(raster)
library(ggplot2)
library(parallel)

brickstomatrix <- function(wdpa){
# reads brick from filename
id <- gsub(".tif", "", wdpa)
setwd("/home/chris/Documents/data/deforecast/processed/brick")
brick_wdpa <- brick(wdpa)
n_row <- nrow(brick_wdpa)
n_col = ncol(brick_wdpa)
# coerces raster stack to a list of rasters
bricklist <- list(brick_wdpa[[1]], brick_wdpa[[2]], brick_wdpa[[3]])
# applies rastertomatrix function (see below) to each layer of the stack
# function to turn a raster to an x/y/value matrix
rastertomatrix <- function(ras) {
  # gets utm coordinates
  ras <- as.data.frame(ras, xy = T)
  #if lon is negative
  neglon <- ifelse(mean(ras$x) < 0, TRUE,FALSE) 
  # if lat is negative
  neglat <- ifelse(mean(ras$y) < 0, TRUE,FALSE) 
  # mirrors if negative to ensure correct shape
  ras$x <- if(neglon == FALSE){ 
    rep(1:n_col, times = n_row)
  } else {  
    -rep(1:n_col, times = n_row) + n_col
  }
  ras$y <- if(neglat == FALSE) {
    rep(1:n_row, each = n_col)
  } else {
    -rep(1:n_row, each = n_col) + n_row
  }
  # fixes column names
  colnames(ras) <- c("x", "y", "value")
  return(ras)
}
matrices <- lapply(bricklist, rastertomatrix)
matrix <- cbind(matrices[[1]], matrices[[2]], matrices[[3]])
matrix[,9] <- ceiling(matrix[,9]/1000)
setwd("/home/chris/Documents/data/deforecast/processed/matrices")
write.csv(x = matrix, 
          file = paste0(id, ".csv")
          )
}

setwd("/home/chris/Documents/data/deforecast/processed")
bricks <- list.files("./brick")
bricks <- split(bricks, seq(length(bricks))) # splits to list
mclapply(bricks, brickstomatrix)
