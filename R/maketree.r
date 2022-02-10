#' Makes trees
#' @importFrom stats quantile rnorm aggregate
#' @inheritParams deforecast
#' @return A 2-member list with protected and unprotected trees.

make_trees <- function(
  xdim,
  ydim,
  p_n,
  np_n,
  mean_age,
  sd_age){
  
    p_xloc <- sample(c(1:xdim), size = p_n, replace = TRUE);
    p_yloc <- sample(c(1:ydim), size = p_n, replace = TRUE);
    age_p <- rnorm(n = length(p_xloc), mean = mean_age, sd = sd_age)
    age_p[age_p < 0] <- 0
    age_p <- round(age_p, digits = 0)
    p_trees <- cbind(p_xloc, p_yloc, age_p, 1)
    
    np_xloc <- sample(c(1:xdim), size = np_n, replace = TRUE);
    np_yloc <- sample(c(1:ydim), size = np_n, replace = TRUE);
    age_np <- rnorm(n = length(np_xloc), mean = mean_age, sd = sd_age)
    age_np[age_np < 0] <- 0
    age_np <- round(age_np, digits = 0)
    np_trees <- cbind(np_xloc, np_yloc, age_np, 0)
    
  trees <- rbind(p_trees, np_trees);
  return(trees);
}
