library(tidyverse)
library(kableExtra)
setwd("/home/chris/Documents/GitHub/deforecast/otheruseful/manuscript/figures")
read.csv("/home/chris/Documents/data/deforecast/results/cluster/10kmeans") %>%
group_by(cluster) %>%
summarise(mean_temperature = mean(t_m),
          temperature_variation = mean(t_s),
          mean_precipitation = mean(p_m),
          precipitation_variation = mean(p_s),
          soil_depth = mean(soil)) %>%
  arrange() %>%
  kable(digits = 1) %>%
  kable_classic(full_width = F) %>% as_image(file = "cluster_properties.png")
view(cluster_table)
webshot::install_phantomjs()
getwd()
