# virtual forests
library(basemaps)
library(tidyverse)
library(raster)
library(fasterize)
library(sf)
library(patchwork)
install.packages("patchwork")
select <- dplyr::select

fc <- raster("/home/chris/Documents/data/deforecast/processed/lc/fc2020.tif")
pa <- read_sf("/home/chris/Documents/data/deforecast/processed/pa/pa_polygon.shp") %>% filter()
pa_1496 <- pa %>% filter(WDPAID == 1496)

pa_1496 <- st_simplify(pa_1496) %>% select(WDPAID); # simplifies the geometry and strips out all info apart from the id
id <- as.character(pa_1496$WDPAID); # saves ids for later 
zone <- floor((st_coordinates(pa_1496)[[1,1]] + 180) / 6) + 1 # gets the appropriate utm zone for each forest 
crs <- paste0("+proj=utm +zone=",zone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"); # creates a proj4s for the zone
st_crs(pa_1496) <- "+proj=longlat +datum=WGS84 +no_defs"; # affirms the original crs
pa_1496pa <- st_transform(pa_1496,crs) # reprojects to utm
pa_14965 <- st_buffer(pa_1496pa, dist = 5000) # creates a 5km buffer around the pa
pa_149610 <- st_buffer(pa_14965, dist = 5000) # creates a further 5km buffer around 
pa_149610 <- st_difference(pa_149610,pa_14965) %>% select(WDPAID)
pa_149615 <- st_buffer(pa_149610, dist = 5000)
pa_149615 <- st_difference(st_difference(pa_149615,pa_149610), pa_14965) %>% select(WDPAID)
pa_149620 <- st_buffer(pa_149615, dist = 5000)
pa_149620 <- st_difference(st_difference(st_difference(pa_149620,pa_149615),pa_149610),pa_14965) %>% select(WDPAID)
pa_149620_bb <- st_as_sfc(st_bbox(pa_149620))
pa_149621 <- st_buffer(pa_149620, dist = 1000)
pa_149621_bb <- st_as_sfc(st_bbox(pa_149621))
pa_149621_bb_4326 <- st_transform(pa_149621_bb, crs = "+proj=longlat +datum=WGS84 +no_defs")

box <- pa_149621_bb_4326
st_crs(box) <- 4326

fc_1496 <- crop(fc, as(box, "Spatial"), snap = "out")
xy_tile_fc <- xyFromCell(fc_1496, 1:ncell(fc_1496))
value_tile_fc <- values(fc_1496)
tile_fc <- data.frame(cbind(xy_tile_fc, value_tile_fc))
colnames(tile_fc) <- c("x", "y", "value")
tile_fc$value <- as.factor(tile_fc$value)

ls <- raster("/home/chris/Documents/data/deforecast/raw/ls2019/lspop2019/w001001.adf")
ls_1496 <- crop(ls, as(box, "Spatial"), snap = "out")
xy_tile_pop <- xyFromCell(ls_1496, 1:ncell(ls_1496))
value_tile_pop <- values(ls_1496)
tile_pop <- data.frame(cbind(xy_tile_pop, value_tile_pop))
colnames(tile_pop) <- c("x", "y", "value")

set_defaults(map_service = "esri", map_type = "world_imagery")
ext <- st_bbox(box)


satellite <- ggplot() +
  basemap_gglayer(ext = ext) +  
  coord_sf() +
  scale_fill_identity() +
  theme_void()
satellite
fc <- ggplot() +
  geom_tile(data = tile_fc, aes(x = x, y = y, fill = value)) +
  scale_fill_discrete(type = "#228B22", na.value = NA, na.translate = F) +
  geom_sf(data = box, fill = NA) +
  geom_sf(data = pa_1496, colour = "black", fill = NA) +
  theme_void() +
  theme(legend.position = "none")
pop <- ggplot() +
  geom_tile(data = tile_pop, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis(option = "inferno", direction = -1, breaks = c(0,20000, 40000, 60000), limits = c(0,60000)) +
  geom_sf(data = pa_1496, colour = "black", fill = NA) +
  theme_void() +
  guides(alpha = "none") +
  labs(fill = "Population count") +
  theme(legend.position = "bottom")

pop  
modelforest <- satellite / fc / pop
modelforest
ggsave(plot = modelforest, filename = "/home/chris/Documents/GitHub/deforecast/otheruseful/manuscript/figures/modelforest.png", height = 297, width = 210, units = "mm")


# layer cleverly