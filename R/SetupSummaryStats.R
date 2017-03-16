#' @title SetupSummaryStats
#' @description Create an list set up for all the summary stats. Each list itme is length of BufMax and is all 0.
#' @param inputs Inputs from .rav file
#' @return SummaryStats list
SetupSummaryStats = function(inputs){
  SummaryStats = list()

  #most get set to 0 because I add info/NRuns each rep
  #mins and maxs get NA because I use min(a,b,na.rm=TRUE) to ignore initial setting
  SummaryStats[["AvgEscpmnt"]] = matrix(0, inputs$BufMax, inputs$NYears)
for(el in c("MinEscpmnt","MaxEscpmnt"))
  SummaryStats[[el]] = matrix(NA, inputs$BufMax, inputs$NYears)

  SummaryStats[["AvgBYrHR"]] = matrix(0, inputs$BufMax, inputs$NYears - (inputs$MaxAge - inputs$MinAge))
  for(el in c("MinBYrHR","MaxBYrHR"))
    SummaryStats[[el]] = matrix(NA, inputs$BufMax, inputs$NYears - (inputs$MaxAge - inputs$MinAge))
  
for(el in c("AvgAEQMort","AvgECrit","AvgCaHR", "BufAvgBYrHR", "PropExt", "PropRec", "BufAvgBYrHR", "AveRanFlow", "AveRanMarine"))
  SummaryStats[[el]] = rep(0, inputs$BufMax)
  
  return(SummaryStats)
}