# '*****  RepInit  *****
# '******************************************************************
RepInit = function(inputs){
  rtn.list=list()
  #Set default values used when RanFlow and RanMarine are used (2 and 3 param SR models)
  rtn.list$LastRanFlow = 0
  rtn.list$LastRanMarine = 0

  rtn.list$Cohort = inputs$CohortStart
  
  #'SET INITIAL SEED FOR AUTOCORRELATED RESIDUALS
  if(inputs$SurvScale == "YES"){
    rtn.list$LastRanError = rnorm(1, 0, sd=sqrt(inputs$SRErrorB))
  }
  
  #'SET INITIAL VALUES FOR LAST RANMARINE AND RANFLOW
  #'EEH: VB code was misisng HOC3 and BEV3 from this
  if(any(inputs$SRType==c("HOC3", "BEV3", "RIC3"))){
    if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
      rtn.list$LastRanFlow = inputs$FlowAve
    }else{
      if(inputs$TCF2 > 0){ rtn.list$LastRanFlow = inputs$TCF2; 
      }else{ rtn.list$LastRanFlow = rgamma(1,inputs$GammaFlowA, scale=inputs$GammaFlowB); }
    }
  }
  if(any(inputs$SRType==c("HOC4", "BEV4", "RIC4"))){
    if(inputs$GammaMarA + inputs$GammaMarB == 0){
      rtn.list$LastRanMarine = inputs$MarAve
    }else{
      if(inputs$TCM2 > 0){ rtn.list$LastRanMarine = inputs$TCM2 
      }else{ rtn.list$LastRanMarine = rgamma(1,inputs$GammaMarA, scale=inputs$GammaMarB) }
    }
    
    if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
      rtn.list$LastRanFlow = inputs$FlowAve
    }else{
      if(inputs$TCF2 > 0){ rtn.list$LastRanFlow = inputs$TCF2; 
      }else{ rtn.list$LastRanFlow = rgamma(1,inputs$GammaFlowA, scale=inputs$GammaFlowB); }
    }
    
  }
    
  return(rtn.list)
}