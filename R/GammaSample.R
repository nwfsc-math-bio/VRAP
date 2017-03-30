#' @title GammmaSample
#' @description Function generates a random gamma deviate with shape
#'paramater alpha and scale parameter beta
#' @param Alpha alpha parameter of gamma
#' @param Beta beta parameter of gamma
#' @return Gamma distributed random variable (scalar)
GammaSample = function(Alpha, Beta){
  #EEH: removed the no random variables option
  # if(get("CONSTRUN", pkg_globals)){ #no random variables
  #   val = Alpha * Beta #expected value
  # }else{
  val = rgamma(1, Alpha, scale=Beta)
  # }
  return(val)
}