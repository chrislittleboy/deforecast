#' Makes people
#' @inheritParams deforecast

get_people <- function(ppl_loc, ppl_scaling){
  ppl <- ppl_loc
  if(ppl_scaling != 1){
     scaled_off <- sample(c(0,1), 
                         length(ppl[,1]), 
                         replace = T, 
                         prob = c(1-ppl_scaling, ppl_scaling))
    scaled_off[1] <- ifelse(sum(scaled_off) == 0, 1, scaled_off[1])
    ppl <- cbind(ppl,scaled_off)
    ppl[,3] <- ppl[,3] * ppl[,4]
    ppl <- ppl[ppl[,3] != 0,1:3]
  }
  return(ppl)
}

