#' @title CompBetaVariate
#' @description This subroutine generates a beta random variable.
#' @details
#' EEH for testing purposes the original VB code was duplicated.  
#' This could have been replaced with rbeta(alpha,beta).
#' 
#' Original VB codes creates beta r.v. by using the gamma distribution
#' Mean = alpha/(alpha+beta)
#' Variance = (a*b)/((a+b)^2*(a+b+1))
#' TAB: not sure this is correct way of getting beta distr
#' since Beta(a,b) = (Gamma(a)*Gamma(b))/(Gamma(a+b))
#'***********************************************
#'   Function CompBetaVariate(Alpha As Double, Beta As Double)
#' Dim G1 As Double
#' Dim G2 As Double
#' 
#' If CONSTRUN = True Then
#' CompBetaVariate = Alpha / (Alpha + Beta) 'expected value
#'     Else
#'         If Alpha <= 1 Then    'TAB: changed ALP to Alpha in this line
#' G1 = Gam1(Alpha, 1)
#' Else
#' G1 = Gam2(Alpha, 1)
#' End If
#' 
#' If Beta <= 1 Then     'TAB: changed BET to Beta in this line
#'             G2 = Gam1(Beta, 1)
#'         Else
#'             G2 = Gam2(Beta, 1)
#'         End If
#'         
#'         CompBetaVariate = G1 / (G1 + G2)
#'     End If
#' End Function
#' @param Alpha alpha parameter of gamma
#' @param Beta beta parameter of gamma
#' @return Beta distributed random variable (scalar)
CompBetaVariate = function(Alpha, Beta){
#EEH: removed the no random variables option
#  if(get("CONSTRUN", pkg_globals)){ #no random variables
#     val = Alpha / (Alpha + Beta) #expected value
#   }else{
    G1 = rgamma(1, Alpha, scale=1)
    G2 = rgamma(1, Beta, scale=1)
    val = G1 / (G1 + G2)
#   }
  return(val)
}
