#' @title SUB AEQcalc
#' @description Compute the AEQs for each age group.  Original VB code:
#' @details
#' TAB: what are AEQs? Adult Equivalents!
#' Sub AEQcalc()
#' Dim TmpA As Double  'TAB: added line
#' Dim TmpS As Double  'TAB: added line
#' Dim Age As Integer   'TAB: added line
# 
#' TmpA = 0
#' TmpS = 0
#' For Age = MaxAge% To MinAge% Step -1
#' AEQ(Age) = MatRate(Age) + TmpS * (1 - MatRate(Age)) * TmpA
#' TmpA = AEQ(Age)
#' TmpS = 1 - NatMort(Age)
#' Next Age
#' 
#' End Sub
#' @return AEQ (scalar)
AEQcalc = function(input){ 
  MinAge=input$MinAge
  MaxAge=input$MaxAge
  MatRate = input$MatRate
  NatMort = input$NatMort
  
  AEQ = rep(0,MaxAge);
  TmpA = 0
  TmpS = 0
  for(Age in MaxAge:MinAge){
    AEQ[Age] = MatRate[Age] + TmpS * (1 - MatRate[Age]) * TmpA
    TmpA = AEQ[Age]
    TmpS = 1 - NatMort[Age]
  }
  return(AEQ)
}