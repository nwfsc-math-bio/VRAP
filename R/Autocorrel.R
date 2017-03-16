#' @title Autocorrel
#' @description Compute autocorrelated variable, p = autocorrelation, lastx = last value of variable x
#' @param p autocorrelation
#' @param lastx last value of variable x
#' @param x A random (non-correlated) variable generated from the gamma function for the variable.
#' @return New random autocorrelated variable (scalar)
Autocorrel = function(p, lastx, x){        
  val = p * lastx + (1 - p) * x;
  return(val)
}

