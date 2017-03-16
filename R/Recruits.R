#' @title Recruits
#' @details
#' Function Recruits() As Double
#' Compute factor to convert calculated spawner equivalent
#' production to age cohort (source is PSC Chinook Model).
#'
#' Tmp = 0
#' X9 = 1 - NatMort(1)
#' For Age% = MinAge% To MaxAge%
#' X9 = X9 * (1 - NatMort(Age%))
#' Tmp = Tmp + X9 * MatRate(Age%)
#' X9 = X9 * (1 - MatRate(Age%))
#' Next Age%
#' Recruits = Tmp
#' 
#' End Function

#' The SR function gets us the AEQRecruits from spawners in year t.
#' We needs to then translate that to age 1 indiv in pop (Cohort[1])
#' We know AEQRecruit.  How many Age 1 individuals does that translate to?
#' Age1 * (1- total fraction lost) = AEQRecruits
#' So Age1 = AEQRecruits/(1-total fraction lost)
#' Tmp here is total fraction of age 1 ind that eventually return
#' AEQRecruits/Tmp = Age 1 or Cohort[1]
#' @param inputs Inputs from .rav file
#' @return Recruits at age 1
Recruits = function(inputs){    
Tmp = 0
X9 = 1 - inputs$NatMort[1] #survival ag 1
for(Age in inputs$MinAge:inputs$MaxAge){
X9 = X9 * (1 - inputs$NatMort[Age])
Tmp = Tmp + X9 * inputs$MatRate[Age]
X9 = X9 * (1 - inputs$MatRate[Age])
}
#AEQRecruits / compvars$RecruitsAtAge1
return(Tmp) #Recruits at age 1
}