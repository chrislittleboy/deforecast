
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