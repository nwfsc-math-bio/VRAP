#' @title CompRecruits
#' @description Compute recruits
#' @param YearStats list of computed variables for each year: AEQMort, Escpmnt[Year,] = Escpmnt, TotAdultEscpmnt, TotAEQMort, TotEscpmnt,TempCohort.
#' @param Year Year to compute the escapement for.
#' @param inputs Inputs from .rav file
#' @param repvars repvars
#' @param staticvars Static variables
#' @param BufSRb Capacity.  Changes if StepFunc=Pop.  Otherwise stays the same.
#' @return updated repvars list with: Cohort[1]=CohortAge1, LastRanFlow, LastRanError, LastRanMarine.
CompRecruits = function(YearStats, Year, inputs, repvars, staticvars, BufSRb){
  
  #if StepFunc="Pop" then SRb (capacity) is being changed.
  if(inputs$StepFunc=="POP"){
    SRb=BufSRb
  }else{
    SRb=inputs$BSRb
  }
  #cummulative stats
  LastRanFlow = repvars$LastRanFlow
  LastRanError = repvars$LastRanError
  LastRanMarine = repvars$LastRanMarine
  
  #Horrid; renaming TotEscpmnt to Escpmnt
  if(inputs$EscChoice == "YES"){
    Escpmnt = YearStats$TotAdultEscpmnt[Year]
  }else{ Escpmnt = YearStats$TotEscpmnt[Year] }
  if(Escpmnt < 1){ Escpmnt = 0 }
  
  # COMPUTE AEQ RECRUITMENT  
  if(inputs$SRType == "HOC2"){
    if(inputs$BSRa * Escpmnt > SRb){ 
      AEQRecruits =  SRb 
    }else{
      AEQRecruits = inputs$BSRa * Escpmnt
    }
  }
  
  if(inputs$SRType == "RIC2"){ 
    AEQRecruits = inputs$BSRa * Escpmnt * exp(-Escpmnt / SRb)
  }
  
  if(inputs$SRType == "BEV2"){
    AEQRecruits = 0
    if(Escpmnt > 0) AEQRecruits = 1/((1 / SRb) + (1 / inputs$BSRa) * (1 / Escpmnt))
  }
  
  if(any(inputs$SRType==c("HOC2","RIC2","BEV2")) & inputs$SurvScale == "YES"){
    #StockScalar = GammaSample(inputs$SRErrorA, inputs$SRErrorB)
    StockScalar = rgamma(1, inputs$SRErrorA, scale=inputs$SRErrorB)
    AEQRecruits = StockScalar * AEQRecruits
  }
  
  
  if(any(inputs$SRType==c("HOC3", "RIC3", "BEV3"))){
    
    if(inputs$TrndCycF == "Autoc"){
      if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
        RanFlow = inputs$FlowAve
      }else{
        #RanFlow = GammaSample(inputs$GammaFlowA, inputs$GammaFlowB)
        RanFlow = rgamma(1, inputs$GammaFlowA, scale=inputs$GammaFlowB)
        RanFlow = Autocorrel(inputs$TCF1, LastRanFlow, RanFlow)
      }
      
    } #Autoc
    
    if(inputs$TrndCycF == "Trend"){
      if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
        RanFlow = inputs$FlowAve
      }else{
        Mean = Trend(inputs$TCF1, Year, inputs$FlowAve, inputs$TCF2)
        Var = (inputs$FlowCV * Mean)^2
        if(Var == 0){
          RanFlow = Mean
        }else{
          #EEH: In old code this was GammaFlowA which meant the global was overwritten
          GammaFlowTrA = Mean * Mean / Var
          GammaFlowTrB = Var / Mean
          #RanFlow = GammaSample(GammaFlowTrA, GammaFlowTrB)
          RanFlow = rgamma(1, GammaFlowTrA, scale=GammaFlowTrB)
        }
      }
    }
    
    if(inputs$TrndCycF == "Cycle"){
      Mean = Cycle(inputs$TCF1, inputs$TCF2, inputs$TCF3, Year)
      #  should use TCF4 as scalar in place of FlowAve
      Var = (inputs$FlowCV * Mean)^2
      if(Var == 0){
        RanFlow = Mean
      }else{
        GammaFlowCyA = Mean * Mean / Var
        GammaFlowCyB = Var / Mean
        #RanFlow = GammaSample(GammaFlowCyA, GammaFlowCyB)
        RanFlow = rgamma(1, GammaFlowCyA, scale=GammaFlowCyB)
      }
    }
    
    if(RanFlow < 0){ RanFlow = 0 }
    LastRanFlow = RanFlow
    
    if(inputs$CenterCov=="YES"){
      #This is how DM also writes the effect of marine survival
      #flow is log-transformed so this works since FlowAve is mean of log(flow)
      FWS = exp(inputs$BSRd * (RanFlow-inputs$logFlowMu))
    }else{
      FWS = exp(inputs$BSRd * RanFlow)
    }
    if(FWS < 0) FWS = 0  
    
    if(inputs$SRType == "HOC3")
      if(inputs$BSRa * Escpmnt > SRb){ r = SRb * FWS
      }else{ r = inputs$BSRa * Escpmnt * FWS }
    
    if(inputs$SRType == "RIC3"){
      r = inputs$BSRa * Escpmnt * exp(-Escpmnt / SRb) * FWS
    }
    
    if(inputs$SRType == "BEV3"){
      r = 0;
      if(Escpmnt > 0) r = FWS / ((1 / SRb) + (1 / inputs$BSRa) * (1 / Escpmnt))
    }
    
    AEQRecruits = r
    
    if(inputs$SurvScale == "YES" & AEQRecruits > 0){
      RanError = inputs$ResCorParameter * repvars$LastRanError + (1 - inputs$ResCorParameter) * rnorm(1) * sqrt(inputs$SRErrorB)
      LastRanError = RanError
      AEQRecruits = AEQRecruits * exp(RanError)
    }
    
  }#end 3 parameter cases
  
  
  if(any(inputs$SRType==c("HOC4", "RIC4", "BEV4"))){
    
    #Set Marine Surv Params
    if(inputs$TrndCycM == "Autoc"){
      if(inputs$GammaMarA + inputs$GammaMarB == 0){
        RanMarine = inputs$MarAve
      }else{
        #RanMarine = GammaSample(inputs$GammaMarA, inputs$GammaMarB)
        RanMarine = rgamma(1, inputs$GammaMarA, scale=inputs$GammaMarB)
        RanMarine = Autocorrel(inputs$TCM1, LastRanMarine, RanMarine)
      }
    }
    
    if(inputs$TrndCycM == "Trend"){
      if(inputs$GammaMarA + inputs$GammaMarB == 0){
        RanMarine = inputs$MarAve
      }else{
        Mean = Trend(inputs$TCM1, Year, inputs$MarAve, inputs$TCM2)
        Var = (inputs$MarCV * Mean)^2
        if(Var == 0){
          RanMarine = Mean
        }else{
          GammaMarTrA = Mean * Mean / Var
          GammaMarTrB = Var / Mean
          #RanMarine = GammaSample(GammaMarTrA, GammaMarTrB)
          RanMarine = rgamma(1, GammaMarTrA, scale=GammaMarTrB)
        }
      }
    }
    
    if(inputs$TrndCycM == "Cycle"){
      Mean = Cycle(inputs$TCM1, inputs$TCM2, inputs$TCM3, Year)
      #  should use TCF4 as scalar in place of MarAve      
      Var = (inputs$MarCV * Mean)^2
      if(Var == 0){
        RanMarine = inputs$MarAve
      }else{
        GammaMarCyA = Mean * Mean / Var
        GammaMarCyB = Var / Mean
        #RanMarine = GammaSample(GammaMarCyA, GammaMarCyB)
        RanMarine = rgamma(1, GammaMarCyA, scale=GammaMarCyB)
      }
    }
    if(RanMarine < 0) RanMarine = 0      
    LastRanMarine = RanMarine
    
    if(inputs$CenterCov=="YES"){
      #This is how DM writes the effect of marine survival
      #log-transformed and centered
      MS = exp(inputs$BSRc * (log(RanMarine)-inputs$logMSMu))
    }else{
      MS = RanMarine^inputs$BSRc
    }
    if(MS < 0) MS = 0
    
    #Set the flow parameters
    if(inputs$TrndCycF =="Autoc"){
      if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
        RanFlow = inputs$FlowAve
      }else{
        #RanFlow = GammaSample(inputs$GammaFlowA, inputs$GammaFlowB)
        RanFlow = rgamma(1, inputs$GammaFlowA, scale=inputs$GammaFlowB)
        RanFlow = Autocorrel(inputs$TCF1, LastRanFlow, RanFlow)
      }
    }
    
    if(inputs$TrndCycF =="Trend"){
      if(inputs$GammaFlowA + inputs$GammaFlowB == 0){
        RanFlow = inputs$FlowAve
      }else{
        Mean = Trend(inputs$TCF1, Year, inputs$FlowAve, inputs$TCF2)
        Var = (inputs$FlowCV * Mean)^2
        if(Var == 0){
          RanFlow = Mean
        }else{
          GammaFlowTrA = Mean * Mean / Var
          GammaFlowTrB = Var / Mean
          #RanFlow = GammaSample(GammaFlowTrA, GammaFlowTrB)
          RanFlow = rgamma(1, GammaFlowTrA, scale=GammaFlowTrB)
        }
      }
    }
    
    if(inputs$TrndCycF =="Cycle"){
      Mean = Cycle(inputs$TCF1, inputs$TCF2, inputs$TCF3, Year)
      #  should use TCF4 as scalar in place of FlowAve
      Var = (inputs$FlowCV * Mean)^2
      if(Var == 0){
        RanFlow = inputs$FlowAve
      }else{
        GammaFlowCyA = Mean * Mean / Var
        GammaFlowCyB = Var / Mean
        #RanFlow = GammaSample(GammaFlowCyA, GammaFlowCyB)
        RanFlow = rgamma(1, GammaFlowCyA, scale=GammaFlowCyB)
      }
    }
    
    if(RanFlow < 0) RanFlow = 0      
    LastRanFlow = RanFlow
    
    if(inputs$CenterCov=="YES"){
      FWS = exp(inputs$BSRd * (RanFlow-inputs$logFlowMu))
    }else{
      FWS = exp(inputs$BSRd * RanFlow)
    }
    if(FWS < 0) FWS = 0  
    
    if(inputs$SRType == "HOC4"){
      if(inputs$BSRa * Escpmnt > SRb){
        r=SRb * MS * FWS
      }else{ r = inputs$BSRa * Escpmnt * MS * FWS }
    }
    
    if(inputs$SRType == "RIC4"){
      r = inputs$BSRa * Escpmnt * exp(-Escpmnt / SRb) * MS * FWS
    }
    
    if(inputs$SRType == "BEV4"){
      r = 0;
      if(Escpmnt > 0) r = MS * FWS / ((1 / SRb) + (1 / inputs$BSRa) * (1 / Escpmnt))
    }
    
    AEQRecruits = r
    
    if(inputs$SurvScale == "YES" & AEQRecruits > 0){
      RanError = inputs$ResCorParameter * LastRanError + (1 - inputs$ResCorParameter) * rnorm(1) * sqrt(inputs$SRErrorB)
      LastRanError = RanError
      AEQRecruits = AEQRecruits * exp(RanError)
    }
  } #end param 4 cases
  
  if(inputs$depen == "YES"){
    if(Escpmnt < inputs$DL1 + 1){
      if(Escpmnt < inputs$DL2 + 1){ 
        fac = Escpmnt / inputs$DL2 * inputs$DR
      }else{
        fac = ((1 - (inputs$DL1 - Escpmnt) / (inputs$DL1 - inputs$DL2)) * (1 - inputs$DR)) + inputs$DR }
      
      if(fac < 0) fac = 0
      
      AEQRecruits = fac * AEQRecruits
    }
  }
  
  
  # COMPUTE RECRUITS that are age 1;
  # RecruitsAtAge1 is from Recruits() and is "total fraction of age 1 ind that eventually return"
  # AEQRecruits/RecruitsAtAge1 = Age 1 or Cohort[1]
  CohortAge1 = AEQRecruits / staticvars$RecruitsAtAge1
  
  # ADD MARINE SURVIVAL IF STOCK RECRUIT FUNCTION IS SPAWNER TO SMOLT
  if(inputs$MarSurv == "YES"){
    BetaVariate = CompBetaVariate(inputs$BetaMarA, inputs$BetaMarB)
    CohortAge1 = CohortAge1 * BetaVariate
  }
  repvars$Cohort[1]=CohortAge1
  repvars$LastRanFlow = LastRanFlow
  repvars$LastRanError = LastRanError
  repvars$LastRanMarine = LastRanMarine
  return(repvars)
}
