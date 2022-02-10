# matricestoinputlists
library(tidyverse)
library(parallel)

setwd("/home/chris/Documents/data/deforecast/processed/")

matlist <- list.files("./matrices")
matlist <- split(matlist, seq(length(matlist))) # splits to list

matrixtolist <- function(x){
id <- gsub(".csv","",x)
mat <- read.csv(paste0("./matrices/",x))
p_loc <- mat[,2:4] %>% drop_na()
np_loc <- mat[,5:7] %>% drop_na()
ppl_loc <- mat[,8:10] %>% drop_na() %>% filter(value.2 != 0)
inputlists <- list(id,ppl_loc,p_loc,np_loc)
saveRDS(inputlists, file =paste0("./inputlists/",id,".RData"))
}

mclapply(matlist, matrixtolist)
