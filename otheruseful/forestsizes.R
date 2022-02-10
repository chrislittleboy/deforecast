library(raster)
library(sf)
library(utils)
library(rgdal)
setwd("/home/chris/Documents/data/deforecast/bricks")
listbricks <- list.files()
listbricks <- sample(listbricks, 30)
s <- cbind(1:30,1:30,1:30)
i <- 1
indication <- function(){
  s <- s;
  while (i <= 30){
    bricks <- brick(listbricks[i])
    
    vec_p <- bricks[[1]][]
    vec_np <- bricks[[2]][]
    vec_ppl <- bricks[[3]][]
    
    p <-sum(vec_p, na.rm = T)
    np <- sum(vec_np, na.rm = T)
    nppl <- sum(vec_ppl, na.rm = T)
    
    s[i,1] <- p;
    s[i,2] <- np;
    s[i,3] <- nppl;
print(i)
i <- i + 1;
  }
  return(s);
}

stats <- indication()

summary(stats)
1941984/1000
11856
971
1
