# cluster analysis

library(tidyverse)
library(raster)
library(exactextractr)
library(sf)

setwd("/home/chris/Documents/data/deforecast/")
#loads protected areas
pa <- read_sf("./processed/pa/pa_polygon.shp") %>% st_geometry()

#gets criteria for deciding forest groupings
setwd("./raw/forestcriteria/")
rasters <- list.files()
# a list of files including:
# from bioclim... 
# Mean temperature, temp seasonality, mean precip, precip seasonality
# And from 

# writes function to extract mean raster values for the above 
# from protected forest shp files
get_values <- function(x) {
x <- raster(x)
v <- exact_extract(x, pa, "mean")
return(v)
}

res <- lapply(rasters, FUN = get_values)
# stores the results
results <- cbind(res[[1]], res[[2]], res[[3]], res[[4]], res[[5]])

# performs the kmeans analysis (10 clusters, normalised results)
kmean_f <- kmeans(scale(results), 10)
results <- data.frame(cbind(results, as.factor(kmean_f$cluster)))
colnames(results) <- c("soil", "t_m", "p_m", "t_s", "p_s", "cluster")
write.csv(results, file = "/home/chris/Documents/data/deforecast/results/cluster/10kmeans")
# creates a summary table for those interested...

summaries <- results %>% group_by(cluster) %>% add_tally() %>%
  summarise(soil_depth = mean(soil), 
            mean_temperature = mean(t_m),
            mean_precipitation = mean(p_m),
            temperature_variation = mean(t_s),
            precipitation_variation = mean(p_s),
            n = mean(n))

# writes to disk
setwd("/home/chris/Documents/data/deforecast/processed/")
write.csv(kmean_f$cluster, "kmean_clusters.csv")
