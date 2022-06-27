#' Grows forest
#' @inheritParams deforecast
#' @importFrom stats aggregate quantile rnorm
#' @param trees # 4 column matrix with all trees (x/y/age/protected)
#' @name growth

growth <- function(trees, 
                   k,
                   r,
                   maturity,
                   max_age,
                   dispersion,
                   agebands){

  trees <- matrix(trees, ncol = 4);  
  k <- k * max(trees[,1]) * max(trees[,2]);
  n <- length(trees[,1]);
  s <- n + (r * n) * (1 - n/k);
  r_l <- (s-n)/n;
    if (r_l < 0) {
      return(trees);
  } else {  
  probbands <- c(0,dbinom(x = 1:10, size = 10, prob = 1/2));
  pnew <- probseed(trees[,3], agebands, probbands);
  pnew <- pnew * (r_l/0.1);
  pnew[pnew <= 0] <- 0.00001;
  pnew[pnew >= 1] <- 1;
  seed <- seedsample(pr = pnew);
if(sum(seed) == 0) {
  trees <- matrix(trees, ncol = 4);
} else {
  new <- cbind(trees, seed); # binds the trees with the probability of seeding
  new <- matrix(new[new[,5] == 1,], ncol = 5); # just selects seeded trees
  new[,2] <- new[,2] + sample(c(-dispersion:dispersion),1); # xloc for new trees
  new[,3] <- new[,3] + sample(c(-dispersion:dispersion),1); # yloc for new trees
  new[,4] <- 0; # age of new trees is 0
  trees <- rbind(trees, new[,1:4]); # combines old and new trees
  trees <- matrix(
    trees[order(trees[,4],decreasing = TRUE), ], 
    ncol = 4
  )
}
}
  return(trees)
}
