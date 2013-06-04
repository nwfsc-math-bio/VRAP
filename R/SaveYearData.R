# '*****  SaveYearData  ******
# '******************************************************************
SaveYearData = function(Year, YearStats){
  
  if(YearStats$TotAEQMort[Year] + YearStats$TotEscpmnt[Year] > 0.000000001){
    HR = YearStats$TotAEQMort[Year] / (YearStats$TotAEQMort[Year] + YearStats$TotEscpmnt[Year])
  }else{
    HR = 0
  }
  
  #Update AEQMort to hold total
  #in VB code Escpmnt (a vector for each age) is redefined to be TotEscpmnt (sum over ages)
  #unclear why
#   YearStats$AEQMort[Year] = YearStats$TotAEQMort
#   YearStats$Escpmnt[Year] = YearStats$TotEscpmnt 
  YearStats$CalendarHR[Year] = HR
#   YearStats$Age2AEQMort[Year] = YearStats$AEQMort[Year,2]
#   YearStats$Age3AEQMort[Year] = YearStats$AEQMort[Year,3]
#   YearStats$Age4AEQMort[Year] = YearStats$AEQMort[Year,4]
#   YearStats$Age5AEQMort[Year] = YearStats$AEQMort[Year,5]
#   YearStats$Age2Escpmnt[Year] = YearStats$Escpmnt[Year,2]
#   YearStats$Age3Escpmnt[Year] = YearStats$Escpmnt[Year,3]
#   YearStats$Age4Escpmnt[Year] = YearStats$Escpmnt[Year,4]
#   YearStats$Age5Escpmnt[Year] = YearStats$Escpmnt[Year,5]
  
  return(YearStats)
  
}