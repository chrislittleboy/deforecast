# forest location map

library(tidyverse)
library(fasterize)
library(sf)
library(raster)

setwd("/home/chris/Documents/data/deforecast/processed/pa/") # protected area shapefiles
pas <- read_sf("pa_polygon.shp") 
world <- map_data("world") %>% filter(region != "Antarctica") # gets standard world map data, without antarctica
sf_world <- st_as_sf(world, coords = c("long", "lat"), crs = 4326)  # converts to sf
r_world <- raster(sf_world, ncols = 2000, nrows = 2000) # rasterizes for template
r_pas <- fasterize(pas, r_world) # rasterizes pas shapefile for easier plotting

# converts to df for plotting with ggplot

xy_tile_pas <- xyFromCell(r_pas, 1:ncell(r_pas))
value_tile_pas <- values(r_pas)
tile_pas <- data.frame(cbind(xy_tile_pas, value_tile_pas))
colnames(tile_pas) <- c("x", "y", "value")
tile_pas$value <- as.factor(tile_pas$value)

clusters <- read_csv("/home/chris/Documents/data/deforecast/results/cluster/10kmeans") %>% dplyr::select(id,cluster) # gets cluster information and same process
pas_cluster <- merge(pas, clusters, by.x = "WDPAID", by.y = "id")
r_pas_cluster <- fasterize(pas_cluster, r_world, field = "cluster")
xy_tile_pas_cluster <- xyFromCell(r_pas_cluster, 1:ncell(r_pas_cluster))
value_tile_pas_cluster <- values(r_pas_cluster)
tile_pas_cluster <- data.frame(cbind(xy_tile_pas_cluster, value_tile_pas_cluster))
colnames(tile_pas_cluster) <- c("x", "y", "value")
tile_pas_cluster$value <- as.factor(tile_pas_cluster$value)

pfa <- ggplot()+
  geom_map(data = world, map = world,aes(map_id = region), colour = "white", size = 0.1) +  
  geom_tile(data = tile_pas, aes(x = x, y = y, fill = value)) +
  scale_fill_discrete(type = "#228B22", na.value=NA, na.translate = F) + 
  theme_void() +
  theme(legend.position = "none")

pfa_cluster <- ggplot()+
  geom_map(data = world, map = world,aes(map_id = region), colour = "white", size = 0.1) +  
  geom_tile(data = tile_pas_cluster, aes(x = x, y = y, fill = value)) +
  scale_fill_brewer(na.value = NA, palette = "Set3", na.translate = F) +
  theme_void() +
  labs(fill = "Cluster")

ggsave(plot = pfa, filename = "/home/chris/Documents/GitHub/deforecast/otheruseful/manuscript/figures/pfa_worldmap.png", width = 297, height =210, units = "mm")
ggsave(plot = pfa_cluster, filename = "/home/chris/Documents/GitHub/deforecast/otheruseful/manuscript/figures/pfa_cluster_worldmap.png", width = 297, height =210, units = "mm")

# potential improvements - add bathymetry, add elevation/shadows, think about basemap
