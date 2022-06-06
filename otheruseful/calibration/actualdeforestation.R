# actual deforestation
library(tidyverse)
library(parallel)
library(raster)

bricks_2001 <- list.files("/home/chris/Documents/data/deforecast/processed/brick2001/")
bricks_2001 <- gsub(".tif", "", bricks_2001)

bricks_2020 <- list.files("/home/chris/Documents/data/deforecast/processed/brick2020/")
bricks_2020 <- gsub(".tif", "", bricks_2020)

bricklist <- split(bricks_2020, seq(length(bricks_2020)))

getdeforestation <- function(x){
    fc_2001 <- brick(paste0("/home/chris/Documents/data/deforecast/processed/brick2001/",x,".tif"))[[1:2]]
    fc_2001[is.na(fc_2001)] <- 0
    fc_2001[[1]] <- fc_2001[[1]] * fc_2001[[2]]
    
    fc_2020 <- brick(paste0("/home/chris/Documents/data/deforecast/processed/brick2020/",x,".tif"))[[1:2]]
    fc_2020[is.na(fc_2020)] <- 0
    fc_2020[[1]] <- fc_2020[[1]] * fc_2020[[2]]
    count_2001 <- cellStats(fc_2001, "sum")
    count_2020 <- cellStats(fc_2020, "sum")
return(c(count_2001, count_2020))
    }

def <- mclapply(bricklist,getdeforestation, mc.cores = 8)
results <- matrix(ncol = 5, nrow = length(bricklist))
results[,1] <- as.numeric(unlist(bricklist))
i <- 1
while(i <= length(bricklist)) {
  results[i,2] <- def[[i]][1]
  results[i,3] <- def[[i]][3]
  results[i,4] <- def[[i]][2]
  results[i,5] <- def[[i]][[4]]
  i <- i + 1
} 
results <- data.frame(results)
colnames(results) <- c("id", "protected2001","protected2020","unprotected2001","unprotected2020")

results <- results %>% 
  mutate(direction = ifelse(
    protected2001 + unprotected2001 - protected2020 - unprotected2020 < 0,
    1,-1)) %>%
  mutate(p = (protected2020-protected2001)/protected2001,
         np = (unprotected2020 - unprotected2001)/unprotected2001,
         t = (protected2020+unprotected2020-protected2001-unprotected2001)/
           (protected2001+unprotected2001))
write.csv(results, "/home/chris/Documents/data/deforecast/calibration/actual.csv")
