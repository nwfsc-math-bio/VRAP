# '*****  CompEscpmnt  *****

CompEscpmnt = function(Regime, Year, inputs, BufTargetU, Cohort, AEQ, YearStats){
  #If you have breakpoints, then Regime refers to which of these to use      
  #'INITIALIZE PARAMETERS, VARIABLES, AND ARRAYS
  Converge = "No"
  MaxAge=inputs$MaxAge
  
  #'SET EXPLOITATION RATE TARGET AFTER ADJUSTMENT FOR MANAGEMENT ERROR.
  #' SET INITIAL FISHERY SCALE To 1.0  UNLESS TARGET U=0
  ErrorScale = 1
  if(inputs$MgmtError == "YES") ErrorScale = rgamma(1, inputs$GammaMgmtA, scale=inputs$GammaMgmtB)
  
  if(BufTargetU[Regime] > 0){
    
    BufTargetUError = BufTargetU[Regime] * ErrorScale
    
    if(BufTargetUError >= 1){
      #cat("Warning - Target HR Reduced to 0.99\n")
      BufTargetUError = 0.99  #not allowed to take all fish?
    }    
    FishScale = 1  #there is fishing
  }else{
    TempCohort = Cohort*(1-inputs$MatRate)
    Escpmnt = Cohort * inputs$MatRate
    Escpmnt[Escpmnt < 1] = 0
    AEQMort = rep(0, inputs$MaxAge)
    TotAEQMort = 0
    TotEscpmnt = sum(Escpmnt)
    Converge = "Yes" #don't need to go through the while loop
  }
  
   n = 0
   while(Converge == "No"){
     n = n + 1
     # The pre-terminal and mature harvest will be scaled up or down until
     # the actual exploitation rate ActualU = TotAEQMort / (TotAEQMort + TotEscpmnt)
     # reaches the target for the simulation BufTargetUError.
     
    #'COMPUTE PRETERMINAL MORTALITY AND UPDATE COHORT
    tmp=FishScale * inputs$PTU
    tmp[tmp>1] = 1 #can't take more fish than are there
    PTMort = tmp * Cohort
    TempCohort = Cohort - PTMort
# This VB code not needed since R can work on a vector
#     for(Age in inputs$MinAge:inputs$MaxAge){
#       if(FishScale * inputs$PTU[Age] < 1){ #FishScale is 1 or 0
#         PTMort[Age] = FishScale * Cohort[Age] * inputs$PTU[Age] #take a fraction of fish
#       }else{
#         PTMort[Age] = Cohort[Age] #take all fish at that age
#       }
#       TempCohort[Age] = Cohort[Age] - PTMort[Age] #pre-term mort
#     } #end for loop age
    
    #'COMPUTE MATURE RUN AND UPDATE COHORT
    MatRun = TempCohort * inputs$MatRate
    TempCohort = TempCohort - MatRun
# This VB code not needed since R can work on a vector
#     for(Age in inputs$MinAge:inputs$MaxAge){
#       MatRun[Age] = TempCohort[Age] * inputs$MatRate[Age]
#       TempCohort[Age] = TempCohort[Age] - MatRun[Age]
#     } #end for loop Age
    
    #'COMPUTE MATURE MORTALITY AND ESCAPEMENT
    #'MatU, MatMort, Escpmnt, MatRun are all 1:MaxAge vectors
    tmp = FishScale * inputs$MatU
    tmp[tmp>1] = 1 #can't take more fish than are there
    MatMort = tmp * MatRun
    Escpmnt = MatRun - MatMort #spawners
    Escpmnt[Escpmnt < 1] = 0
    #     for(Age in inputs$MinAge:inputs$MaxAge){
#       if(FishScale * inputs$MatU[Age] < 1){ 
#         MatMort[Age] = FishScale * MatRun[Age] * inputs$MatU[Age] #these are the fish that return and are taken
#       }else{ #all mature fish taken
#         MatMort[Age] = MatRun[Age]
#       }
#       Escpmnt[Age] = MatRun[Age] - MatMort[Age] #spawners      
#       if(Escpmnt[Age] < 1) Escpmnt[Age] = 0    
#     }# end Age for loop
    
    #'COMPUTE AEQ TOTAL MORTALITY EXPLOITATION RATE
    AEQMort = AEQ * PTMort + MatMort
    TotAEQMort = sum(AEQMort)
    TotEscpmnt = sum(Escpmnt)
#     for(Age in inputs$MinAge:inputs$MaxAge){
#       AEQMort[Age] = (AEQ[Age] * PTMort[Age]) + MatMort[Age]
#       TotAEQMort = TotAEQMort + AEQMort[Age]
#       TotEscpmnt = TotEscpmnt + Escpmnt[Age]
#       #!!! MinAge fish (age 2) not counted
#       if(Age > inputs$MinAge) TotAdultEscpmnt = TotAdultEscpmnt + Escpmnt[Age]     
#     } #end for loop
    
    #'COMPARE ACTUAL AND TARGET; SCALE EXPLOITATION RATES IF NECESSARY
    #'if any of these met, exit
    #'Criteria 1: unclear why needed.  Criteria 2: target ER < 0. Criteria 3: extinct
    if(n > 100 | BufTargetU[Regime] <= 0 | (TotAEQMort + TotEscpmnt) < 1){ 
       Converge = "Yes"
    }else{ #none are met, so determine if converged
      ActualU = TotAEQMort / (TotAEQMort + TotEscpmnt)
      PercentError = abs((ActualU - BufTargetUError) / BufTargetUError)
      if(PercentError > inputs$ConvergeCrit){ 
        FishScale = FishScale * BufTargetUError / ActualU
      }else{ 
       Converge = "Yes" 
      }
    }
     
  } #end of while
  #not needed inside while loop
  #TotAdultEscpmnt = sum(Escpmnt[(inputs$MinAge+1):inputs$MaxAge]) 
  
  YearStats$AEQMort[Year,] = AEQMort
  YearStats$Escpmnt[Year,] = Escpmnt
  YearStats$TotAdultEscpmnt[Year] = sum(Escpmnt[(inputs$MinAge+1):inputs$MaxAge])
  YearStats$TotAEQMort[Year] = TotAEQMort
  YearStats$TotEscpmnt[Year] = TotEscpmnt
  YearStats$TempCohort = TempCohort
  return(YearStats)
}