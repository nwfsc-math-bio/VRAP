#'*****  GetInput   *****
#'Read in a .rav file and assign all the variables
#'Returns the list of all inputs
#'
GetInput = function(InFile){
  
  #The rav file has , as end of input/separator
  
  inputs = list()
  inputs$InFile = InFile

  readit = function(skip, n){ read.table(InFile,nrows=1,sep=",",stringsAsFactors=FALSE,skip=skip)[1,n] }
  
  #'GET TITLE FOR RUN
  inputs$Title = readit(0,1) #line 1
  
  # '-------------------- RUN PARAMETERS SECTION ---------------------------------
  # 'INPUT RANDOM NUMBER SEED, NUMBER OF CYCLES AND REPETITIONS,
  # ' MINIMUM AND MAXIMUM AGE
  inputs$RanSeed = readit(1,1) #line 2, RanSeed
  inputs$NRuns = readit(2,1) #line 2, NRuns
  inputs$NYears = readit(3,1) #line 3, NYears
  inputs$MinAge = readit(4,1) #line 4, NYears
  inputs$MaxAge = readit(4,2) #line 4, NYears
  inputs$ConvergeCrit = readit(5,1) #line 5, ConvergeCrit
  inputs$Debugg = readit(6,1) #line 6, Debugg
  inputs$Debugg = toupper(inputs$Debugg)
  if(!(inputs$Debugg %in% c("YES", "NO"))) stop("Unknown debug selection (yes/no only)")  
  
  #'----- END OF RUN PARAMETERS SECTION ------------------------
  
  # '----- STOCK-RECRUIT SECTION -------------------------------
  #   'GET FORM OF SPAWNER RECRUIT FUNCTION AND PARAMETERS
  # 'njs     updated these explanations
  # ' BSRa = productivity parameter
  # ' BSRb = density dependent prarameter
  # ' BSRc = marine survival paramater - M^c
  # ' BSRd = freshwater survival parameter - exp(dF)(d should be entered as negative)
  # '  HOC2 - Hockey stick R=Min(aS,b)   a = producitvity b=MaxRecruits
  # '  HOC3 - R = Min(aS,) exp(dF) (freshwater index - may be used to predict smolts)
  # '  HOC4 - R= Min(aS,) M^c exp(dF)
  # '  RIC2 - Ricker R=aS exp(-bS)       a = productvity  b=1/capacity
  # '  RIC3 - R= aS exp(-bS+dF) (freshwater index - may be used to predict smolts)
  # '  RIC4 - Ricker with marine survival and freshwater survival
  # '         R=aS M^c exp(-bS+dF)
  # '  BEV2 - Beverton-Holt R=1/(b+a/S)  a = 1/productivity  b=1/MaxRecruits
  # '  BEV3 - R=1/(b+a/S) exp(dF)(freshwater index - may be used to predict smolts)
  # '  BEV4 - BH with marine survival and freshwater survival
  # '         R=1/(a+b/S)M^c exp(dF)
  
  inputs$SRType = readit(7,1) #line 8, SRType
  inputs$SRType = toupper(inputs$SRType)
  SRType=inputs$SRType
  allowedSRType = c("HOC2", "HOC3", "HOC4", "BEV2", "BEV3", "BEV4", "RIC2", "RIC3", "RIC4")
  if(!(SRType %in% allowedSRType)) stop("Unknown stock recruit type")
  
  if(SRType %in% c("HOC2", "RIC2", "BEV2")){
    inputs$BSRa = readit(8,1) #line 4, NYears
    inputs$BSRb = readit(8,2) #line 4, NYears
    inputs$BSRc = 0;
    inputs$BSRd = 0;
    inputs$AveEnv = 1
  }
  #Skip the next 6 lines and jump to line 16
  
  if(SRType %in% c("HOC3", "RIC3", "BEV3")){
    inputs$BSRa = readit(8,1) 
    inputs$BSRb = readit(8,2)
    inputs$BSRc = 0;
    inputs$BSRd = readit(8,3)
    #skip next 3 lines
    inputs$FlowAve = readit(12,1) 
    inputs$FlowCV = readit(12,2) 
    inputs$TrndCycF = readit(13,1)
    if(!(inputs$TrndCycF %in% c("Autoc", "Trend", "Cycle"))) stop("Unknown trend/cycle function for Flow")
    inputs$TCF1 = readit(14,1) 
    inputs$TCF2 = readit(14,2) 
    inputs$TCF3 = readit(14,3)
    inputs$BSRc = 0
    inputs$FlowSD = inputs$FlowCV * inputs$FlowAve
    if(inputs$FlowSD > 0){
      inputs$GammaFlowA = inputs$FlowAve * inputs$FlowAve / (inputs$FlowSD * inputs$FlowSD)
      inputs$GammaFlowB = (inputs$FlowSD * inputs$FlowSD) / inputs$FlowAve
    }else{
      inputs$GammaFlowA = 0
      inputs$GammaFlowB = 0
    }
    
    inputs$AveEnv = exp(inputs$BSRd * inputs$FlowAve)
  }
  
  if(SRType %in% c("HOC4", "RIC4", "BEV4")){
    inputs$BSRa = readit(8,1) 
    inputs$BSRb = readit(8,2)
    inputs$BSRc = readit(8,3);
    inputs$BSRd = readit(8,4)
    inputs$MarAve = readit(9,1) 
    inputs$MarCV = readit(9,2) 
    inputs$TrndCycM = readit(10,1)
    if(!(inputs$TrndCycM %in% c("Autoc", "Trend", "Cycle"))) stop("Unknown trend/cycle function for marine survival")
    inputs$TCM1 = readit(11,1) 
    inputs$TCM2 = readit(11,2) 
    inputs$TCM3 = readit(11,3)
    inputs$FlowAve = readit(12,1) 
    inputs$FlowCV = readit(12,2) 
    inputs$TrndCycF = readit(13,1)
    if(!(inputs$TrndCycF %in% c("Autoc", "Trend", "Cycle"))) stop("Unknown trend/cycle function for Flow")
    inputs$TCF1 = readit(14,1) 
    inputs$TCF2 = readit(14,2) 
    inputs$TCF3 = readit(14,3)
    inputs$MarSD = inputs$MarCV * inputs$MarAve
    if(inputs$MarSD > 0){
      inputs$GammaMarA = inputs$MarAve * inputs$MarAve / (inputs$MarSD * inputs$MarSD)
      inputs$GammaMarB = (inputs$MarSD * inputs$MarSD) / inputs$MarAve
    }else{
      inputs$GammaMarA = 0
      inputs$GammaMarB = 0
    }
    
    inputs$FlowSD = inputs$FlowCV * inputs$FlowAve
    if(inputs$FlowSD > 0){
      inputs$GammaFlowA = inputs$FlowAve * inputs$FlowAve / (inputs$FlowSD * inputs$FlowSD)
      inputs$GammaFlowB = (inputs$FlowSD * inputs$FlowSD) / inputs$FlowAve
    }else{
      inputs$GammaFlowA = 0
      inputs$GammaFlowB = 0
    }
    
    inputs$AveEnv = inputs$MarAve^inputs$BSRc * exp(inputs$BSRd * inputs$FlowAve)
  }
  
  #'depensation with DL1=depensation esc; DL2 = QET; DR % of predicted R realized at QET
  inputs$depen = readit(15,1)
  inputs$depen = toupper(inputs$depen)
  if(!(inputs$depen %in% c("YES", "NO"))) stop("Unknown depensation selection (yes/no only)")  
  inputs$DL1 = readit(16,1) 
  inputs$DL2 = readit(16,2) 
  inputs$DR = readit(16,3)
  
  inputs$EscChoice = readit(17,1)
  inputs$EscChoice = toupper(inputs$EscChoice)
  if(!(inputs$EscChoice %in% c("YES", "NO"))) stop("Unknown escapement choice selection (yes/no only)")  
  
  # 'GET PARAMETERS TO ADD VARIABILITY TO STOCK RECRUIT FUNCTION
  # '  SRErrorA and B are used for Hoc2, Ric2 and Bev2, SRErrorA, SRErrorB and ResCorPar (optional)
  # '  are used for Hoc3, Ric3, Bev3, Hoc4, Ric4, and Bev4
  
  inputs$SurvScale = readit(18,1)
  inputs$SurvScale = toupper(inputs$SurvScale)
  if(!(inputs$SurvScale %in% c("YES", "NO"))) stop("Unknown SR variability selection (yes/no only)")  
  if(inputs$SurvScale == "YES"){
    inputs$SRErrorA = readit(19,1) 
    inputs$SRErrorB = readit(19,2) 
    inputs$ResCorParameter = readit(19,3)
  }
  
  # 'GET PARAMETERS FOR SMOLT TO ADULT (MARINE) SURVIVAL IF STOCK RECRUIT FUNCTION
  # '  IS FOR SMOLTS FROM SPAWNERS (FRESHWATER PRODUCTION)
  
  inputs$MarSurv = readit(20,1)
  inputs$MarSurv = toupper(inputs$MarSurv)
  if(!(inputs$SurvScale %in% c("YES", "NO"))) stop("Unknown marine survival selection (yes/no only)")  
  if(inputs$MarSurv == "YES"){
    inputs$BetaMarA = readit(21,1) 
    inputs$BetaMarB = readit(21,2) 
    inputs$CorrMS = readit(21,3)
  }
  
  #'--------- END OF STOCK RECRUIT SECTION -----------------------------
  
  # '--------- FISHERY MANAGEMENT PARAMETERS ---------------------------
  # 'INPUT THE NUMBER OF BREAKPOINTS AND DIMENSION ARRAYS
  inputs$NumBreakPoints = readit(22,1)
  
  #'IDENTIFY WHICH LEVEL IS THE BASE REGIME
  inputs$BaseRegime = readit(23,1)
  
  inputs$EscpmntBreakPoint = rep(NA,inputs$NumBreakPoints+1)
  inputs$TargetU = rep(NA,inputs$NumBreakPoints+1)
  
  #begin dynamically keeping track of line number
  cline=24
  #'INPUT BREAKPOINTS AND TARGET EXPLOITATION RATES
  for(BreakPoint in 1:(inputs$NumBreakPoints + 1)){
    if(BreakPoint <= inputs$NumBreakPoints){
      inputs$EscpmntBreakPoint[BreakPoint] = readit(cline,1)
      inputs$TargetU[BreakPoint] = readit(cline,2)
      cline=cline+1
    }else{
      inputs$TargetU[BreakPoint] = readit(cline,1)
      cline=cline+1
    }
  }
  
  #'INPUT PARAMETERS FOR MANAGEMENT ERROR
  inputs$MgmtError = readit(cline,1); cline=cline+1
  inputs$MgmtError = toupper(inputs$MgmtError)
  if(!(inputs$MgmtError %in% c("YES", "NO"))) stop("Unknown manag error selection (yes/no only)")
  #'Read in dummy values if management error is No
  if(inputs$MgmtError == "YES"){
    inputs$GammaMgmtA = readit(cline,1);
    inputs$GammaMgmtB = readit(cline,2); cline=cline+1
  }else{
    inputs$GammaMgmtA = 0
    inputs$GammaMgmtB = 0
  }
  
  # '------------------ END OF FISHERY MANAGMENT INPUTS --------------------
  inputs$ECrit = readit(cline,1); cline=cline+1
  inputs$ERecovery = readit(cline,1); 
  inputs$EndAv = readit(cline,2); cline=cline+1
  
  #'INPUT STEP SIZE AND RANGE FOR TARGET EXPLOITATION RATES OR STARTING ESCAPEMENT
  # 'This program outputs information for a range of either exploitation rates or
  # 'starting escapement levels.
  # 'The BUFFER levels input below represent percentages of the base target
  # 'exploitation rate or the starting escapement level.
  # '(e.g., a Buffer of .05 means that the ER will be 5% of the base target rate.)
  # 'You may enter a number greater than 1.0, which means that the ER will be
  # 'greater than the base target rate.  Under normal runs with 0 breakpoints,
  # 'the base target rate is not used other than to determine start, end, and step
  # 'levels.
  inputs$StepFunc = readit(cline,1); cline=cline+1
  inputs$StepFunc = toupper(inputs$StepFunc)
  if(!(inputs$StepFunc %in% c("POP", "ER"))) stop("Unknown step selection")
  
  inputs$BufferStep = readit(cline,1); cline=cline+1
  inputs$BufferStart = readit(cline,1); 
  inputs$BufferEnd = readit(cline,2); cline=cline+1
  #integer of the number of steps of ER or Pop Capacity to do  # is 1:BufMax
  inputs$BufMax = round((inputs$BufferEnd - inputs$BufferStart) / inputs$BufferStep + 1)
  
  # 'INPUT LOWER AND UPPER ESCAPEMENT TEST LEVELS
  # 'The LOWER ESCAPEMENT LEVEL (ECrit) is the escapement level used by the
  # ' program to test how often the observed escapements fall below this level.
  # 'It may represent a "critical" level below which the spawner-recruit function
  # ' destabilizes, and the stock increases risk of extinction, or it could be any
  # ' level one just wanted to monitor frequency of achieving less than or equal
  # ' to that level.
  # 'The UPPER ECSAPEMENT LEVEL (ERecovery) is the escapement level used to compare
  # ' against the geometric mean of the last n (EndAv) years of the run.  It may be a
  # ' management escapement goal, an interim recovery level, or some other target level.
  
  # '----------------------- BASE STOCK DATA SECTION -----------------------------
  #   
  # 'INPUT INITIAL POPULATION SIZE BY AGE
  # ' The initial population size by age, for all ages, is used to seed the model.
  # ' In year 1, the management actions are applied to this population, and a portion of
  # ' each size class escapes.  By running the program over a range of starting population
  # ' sizes, one can determine what minimum population size (or escapement) is needed
  # ' to guarentee a probability of population viability.
  # ' NJS Although the input allows for different MaxAge% from 5, the internal workings of the
  # ' are fixed to 5 in many places.
  
  inputs$CohortStart = rep(0,inputs$MaxAge)
  for(Age in (inputs$MinAge - 1):inputs$MaxAge){
    inputs$CohortStart[Age] = readit(cline,1); cline=cline+1
  }
  
  #'INPUT NATURAL MORTALITY RATES
  inputs$NatMort = rep(0,inputs$MaxAge)
  for(Age in (inputs$MinAge - 1):inputs$MaxAge){
    inputs$NatMort[Age] = readit(cline,1); cline=cline+1
  }    
  
  #'INPUT MATURATION RATES BY AGE (AEQ will be calculated)
  inputs$MatRate = rep(0,inputs$MaxAge)
  for(Age in inputs$MinAge:inputs$MaxAge){
    inputs$MatRate[Age] = readit(cline,1); cline=cline+1
  }
  
  #'INPUT PRETERMINAL AND MATURE EXPLOITATION RATES BY AGE
  #' These will be used to proportion target ER by age and fishery
  #' by def, PTU and MatU are 0 before MinAge
  inputs$PTU = rep(0,inputs$MaxAge)
  inputs$MatU = rep(0,inputs$MaxAge)
  for(Age in inputs$MinAge:inputs$MaxAge){
    inputs$PTU[Age] = readit(cline,1); 
    inputs$MatU[Age] = readit(cline,2); cline=cline+1
  }
  return(inputs)
}