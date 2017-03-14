#' @title Autocorrel
#' @description Compute autocorrelated variable, p = autocorrelation, lastx = last value of variable x
#' @param p autocorrelation
#' @param lastx last value of variable x
#' @param x
#' @return New random variable (scalar)
Autocorrel = function(p, lastx, x){        
  val = p * lastx + (1 - p) * x;
  return(val)
}

