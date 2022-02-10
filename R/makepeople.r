#' Makes people
#' @inheritParams deforecast

make_people <- function(
  xdim, 
  ydim,
  ppl_n){
    ppl_xloc <- sample(c(0:xdim), 
                       size = ppl_n, replace = TRUE);
    ppl_yloc <- sample(c(0:ydim), 
                       size = ppl_n, replace = TRUE);
    ppl <- cbind(ppl_xloc, ppl_yloc)
  return(ppl);
}
