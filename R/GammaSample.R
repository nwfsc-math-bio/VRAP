# '*****   GammmaSample   *****
# 'Function generates a random gamma deviate with shape
# 'paramater alpha and scale parameter beta
# '******************************************************************
GammaSample = function(Alpha, Beta){
# if(get("CONSTRUN", pkg_globals)){ #no random variables
#   val = Alpha * Beta #'expected value
# }else{
  val = rgamma(1, Alpha, scale=Beta)
# }
return(val)

}