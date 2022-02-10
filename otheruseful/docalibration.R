library(tidyverse)
library(deforecasteR)
library(parallel)

rm (list = ls())
chop <- deforecasteR::chop
clusters <- read.csv("/home/chris/Documents/data/deforecast/results/cluster/10kmeans")[,c(1,7)]
# sample_clusters <- clusters %>% group_by(cluster) %>% slice_sample(n=10)
# write.csv(sample_clusters,"/home/chris/Documents/data/deforecast/results/cluster/sampleclusters.csv")
sample_clusters <- read.csv("/home/chris/Documents/data/deforecast/results/cluster/sampleclusters.csv")
calib_list <- read.csv("/home/chris/Documents/data/deforecast/results/cluster/sampleclusters.csv")
calib_list <- list.files("/home/chris/Documents/data/deforecast/processed/inputlists")
calib_list <- gsub(pattern = ".RData", "", calib_list)
calib_list <- calib_list[c(sample_clusters$X)]
calib_list <- calib_list[calib_list %!in% toolarge]

calib_list <- split(calib_list, seq(length(calib_list)))
params <- read.csv("/home/chris/Documents/data/deforecast/processed/params/params1")[,2:9]
new_calib_list[[1]]
getsize <- function(x){
i <- 1;
sizes <- matrix(nrow = 97, ncol = 4)
while(i <= length(x)) {  
x <- new_calib_list[[i]]
x <- readRDS(file = paste0("/home/chris/Documents/data/deforecast/processed/inputlists/",x,".RData"));
id <- x[[1]]
ppl_loc <- x[[2]]
ppl_n <- length(ppl_loc[,1])
p_loc <- x[[3]]
p_n <- length(p_loc[,1])
np_loc <- x[[4]]
np_n <- length(np_loc[,1])
sizes[i,1] <- id
sizes[i,2] <- ppl_n
sizes[i,3] <- p_n
sizes[i,4] <- np_n
i <- i + 1
}
return(data.frame(sizes))
}

test <- lapply(calib_list, getsize)
calib_list <- calib_list[c(1:36,38:95,97:100)]
test <- getsize(new_calib_list)
test <- data.frame(test)
test[,2:4] <- as.numeric(unlist(test[,2:4]))
test$index <- 1:100
toolarge <- c("20324", "352140", "29083")

test <- test %>% filter(X1 %in% toolarge)
new_calib_list <- calib_list[c(1:4,6:7,9:38,40:100)]
new_calib_list[[8]]
test
summary(test)

calib_GO <- 
         mclapply(calib_list, 
         calibration,
         mc.cores = 8,
         params = params,
         years = 19)

x <- calib_list[[17]]
x <- readRDS(file = paste0("/home/chris/Documents/data/deforecast/processed/inputlists/",x,".RData"));
x <- x[[2]]
colnames(x) <- c("x", "y", "value")
ggplot(x, aes(x = x, y = y, col = value)) +
geom_point()

x[[2]]
rm(list = ls())
params

done <- list.files("/home/chris/Documents/data/deforecast/results/calibration/1")
done <- gsub(pattern = ".csv", "", done)

