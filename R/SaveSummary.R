# '*****  SaveSummary  ******
# '********************************************************************
SaveSummary = function(inputs, SummaryStats, staticvars){
  
  file = inputs$OutFileSum;
  output=""
  #'PRINT HEADER INFORMATION
  output=c(output, format(paste("RAPVIABILITY (R) Version ", packageVersion("VRAP")),width=54), "Date:", format(Sys.Date()),"\r\n");
  output=c(output, format("Title:",width=14), inputs$Title, "\r\n");
  output=c(output, format("Input File:",width=19), inputs$InFile, "\r\n");
  output=c(output, "Copy of Input File:", inputs$InFile, "\r\n");
  output=c(output, "\r\n");
  
  output=c(output, "Basic Simulation Input Parameters:","\r\n");
  output=c(output, "    ","# of Years=", inputs$NYears, " # of Reps=", inputs$NRuns, " HR Conv.Crit=", inputs$ConvergeCrit, " Seed", inputs$RanSeed, "\r\n");
  output=c(output, " Range start ", inputs$BufferStart, " end ", inputs$BufferEnd, " by ", inputs$BufferStep, "\r\n");
  output=c(output, "\r\n");
  
  output=c(output, "Stock Recruit Function Input Parameters:", "\r\n");
  output=c(output, "    ", "Function Type: ", inputs$SRType, "\r\n");
    
  if(inputs$SRType == "HOC2"){
    output=c(output, "  ", "Recruits = min(a*Spawners, b)", "\r\n");
    output=c(output, format(paste("    ", "a=productivity=", inputs$BSRa, sep=""),width=24), " b=maxrecruits=", inputs$BSRb, "\r\n");
  }
  if(inputs$SRType == "HOC3"){
    output=c(output, "  ",  "Recruits = min(a*Spawners, b)* exp(d*FWindex)", "\r\n");
    output=c(output, format(paste("    ", "a=productivity=", inputs$BSRa, " b=maxrecruits=", inputs$BSRb, sep=""),width=34), " d=FW parameter= ", inputs$BSRd, "\r\n");
  }
  if(inputs$SRType == "HOC4"){
    output=c(output, "  ",  "Recruits = min(a*Spawners, b)* exp(d*FWindex) * MarineIndex^c", "\r\n");
    output=c(output, "    ", "a=productivity=", inputs$BSRa, " b=maxrecruits=", inputs$BSRb, " c=MSparameter=", inputs$BSRc, " d=FWparameter= ", inputs$BSRd, "\r\n");
  }
  
  if(inputs$SRType == "RIC2"){
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b)", "\r\n");
    output=c(output, format(paste("    ", "a=productivity=", inputs$BSRa, sep=""),width=24), " b=capacity=", inputs$BSRb, "\r\n");
  }
  if(inputs$SRType == "RIC3"){ 
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b + d*FWindex)", "\r\n");
    output=c(output, "    ", "a=productivity=", inputs$BSRa, " b=capacity=", inputs$BSRb, " d=FWparameter= ", inputs$BSRd, "\r\n");
  }
  if(inputs$SRType == "RIC4"){ 
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b + d*FWindex) * MarineIndex^c", "\r\n");
    output=c(output, "    ", "a=productivity=", inputs$BSRa, " b=capacity=", inputs$BSRb, " c=MSparameter=", inputs$BSRc, " d=FW parameter= ", inputs$BSRd, "\r\n");
  }
  
  if(inputs$SRType == "BEV2"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] ", "\r\n");
    output=c(output, format(paste("    ", "a=productivity=", inputs$BSRa, sep=""),width=24), " b=maxrecruits=", inputs$BSRb, "\r\n");
  }
  if(inputs$SRType == "BEV3"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] * exp(d*FWindex)", "\r\n");
    output=c(output, "    ", "a=productivity=", inputs$BSRa, " b=maxrecruits", inputs$BSRb, " d=FWparameter= ", inputs$BSRd, "\r\n");
  }
  if(inputs$SRType == "BEV3"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] * exp(d*FWindex) * MarineIndex^c", "\r\n");
    output=c(output, "    ", "a=productivity=", inputs$BSRa, " b=maxrecruits", inputs$BSRb, " c=MSparameter", inputs$BSRc, " d=FWparameter= ", inputs$BSRd, "\r\n");
  }
  output=c(output, "\r\n");
  
  if(inputs$SurvScale == "YES"){
    if(inputs$SRType %in% c("HOC2", "RIC2", "BEV2")){
      output=c(output, "Stock-Recruit Error Parameters (gamma distr.) [R=f(s)*e]:", "\r\n");
      output=c(output, format(paste("    ", "A=", inputs$SRErrorA, sep=""), width=29), " B=", inputs$SRErrorB, "\r\n");
      output=c(output, "\r\n");
    }
    if(inputs$SRType %in% c("HOC3", "RIC3", "BEV3", "HOC4", "RIC4", "BEV4")){
      output=c(output, "Stock-Recruit Error Parameters [R=f(S)*exp(e)]:", "\r\n");
      output=c(output, format(paste("    ", "MSE=", inputs$SRErrorB, sep=""), width=29), " ResCor=", inputs$ResCorParameter, "\r\n");
      output=c(output, "\r\n");
    }
  }
  
  if(inputs$depen == "YES"){
    output=c(output, "Depensation at escap:", inputs$DL1, " QET:", inputs$DL2, "fraction of depensation at QET", inputs$DR, "\r\n");
    output=c(output, "\r\n");
  }
  
  if(inputs$MarSurv == "YES"){
    if( inputs$SRType %in% c("HOC2", "RIC2", "BEV2", "HOC3", "RIC3", "BEV3")){
      output=c(output, "Smolt to Adult Survival Rate Parameters:", "\r\n");
      output=c(output, format(paste("    ", "Beta_A=", inputs$BetaMarA, sep=""), width=34), " Beta_B=", inputs$BetaMarB, "\r\n");
      output=c(output, "\r\n");
    }else{
      output=c(output, "Should not be using this for ",  inputs$SRType, "\r\n");
      output=c(output, "\r\n");
    }
  }
  
  BufMax = (inputs$BufferEnd - inputs$BufferStart) / inputs$BufferStep + 1
  
  if( inputs$SRType %in% c("HOC3", "RIC3", "BEV3")){
    output=c(output, "Freshwater Survival Parameters:", "\r\n");
    output=c(output, format(paste("    ", "Gamma A=", inputs$GammaFlowA, sep=""), width=34), " Gamma B=", inputs$GammaFlowB, "\r\n");
    output=c(output, format(paste("    ", "mean=", inputs$GammaFlowA * inputs$GammaFlowB, sep=""), width=34), " var=", inputs$GammaFlowB * inputs$GammaFlowB * inputs$GammaFlowA, "\r\n");
    output=c(output, "    ", inputs$TrndCycM, inputs$TCF1, inputs$TCF2, inputs$TCF3, "\r\n");
    output=c(output, "    ", "First Flow index generated = ", SummaryStats$FirstRanFlow, "\r\n");
    output=c(output, "    ", "Average Flow index generated = ", SummaryStats$AveRanFlow / inputs$NYears / inputs$NRuns / inputs$BufMax, "\r\n");
    output=c(output, "\r\n");
  }
  
  if( inputs$SRType %in% c("HOC4", "RIC4", "BEV4")){
    output=c(output, "Marine Survival Parameters:", "\r\n"); 
    output=c(output, format(paste("    ", "Gamma A=", inputs$GammaMarA, sep=""), width=34), " Gamma B=", inputs$GammaMarB, "\r\n");
    output=c(output, format(paste("    ", "mean=", inputs$MarAve, sep=""), width=34), " st dev=", inputs$MarSD, "\r\n");
    output=c(output, "    ", inputs$TrndCycM, inputs$TCM1, inputs$TCM2, inputs$TCM3, "\r\n");
    output=c(output, "    ", "First Marine index generated = ", SummaryStats$FirstRanMarine, "\r\n");
    output=c(output, "    ", "Average Marine index generated = ", mean(SummaryStats$AveRanMarine), "\r\n");
    output=c(output, "\r\n");
    output=c(output, "Freshwater Survival Parameters:", "\r\n");
    output=c(output, format(paste("    ", "Gamma A=", inputs$GammaFlowA, sep=""), width=34), " Gamma B=", inputs$GammaFlowB, "\r\n");
    output=c(output, format(paste("    ", "mean=", inputs$FlowAve, sep=""), width=34), " st dev=", inputs$FlowSD, "\r\n");
    output=c(output, "    ", inputs$TrndCycF, inputs$TCF1, inputs$TCF2, inputs$TCF3, "\r\n");
    output=c(output, "    ", "First Flow index generated = ", SummaryStats$FirstRanFlow, "\r\n");
    output=c(output, "    ", "Average Flow index generated = ", mean(SummaryStats$AveRanFlow), "\r\n");
    output=c(output, "\r\n");
  }
  
  output=c(output, "Fishery Regime Parameters:", "\r\n");
  if(inputs$NumBreakPoints >= 1){
    for(Break in 1:inputs$NumBreakPoints){
      output=c(output, "    ", "Breakpoint", format(Break, digits=2), "\r\n");
      output=c(output, "      ", "HR Below Breakpoint=", inputs$TargetU[Break], "\r\n");
      output=c(output, "      ", "Escapement=", inputs$EscpmntBreakPoint[Break], "\r\n");
      output=c(output, "\r\n");
    }
    output=c(output, "      ", "HR Above BreakPoint=", inputs$TargetU[inputs$NumBreakPoints + 1], "\r\n");
    output=c(output, "\r\n");
  }else{
    output=c(output, "    ", "Base ER = ", inputs$TargetU[inputs$BaseRegime], "\r\n");
  }
  
  if(inputs$MgmtError == "YES"){
    output=c(output, "Management Variability Parameters:", "\r\n");
    output=c(output, format(paste("    ", "Gamma A=", inputs$GammaMgmtA, sep=""), width=34), " Gamma B=", inputs$GammaMgmtB, "\r\n");
    output=c(output, format(paste("    ", "mean=", inputs$GammaMgmtA * inputs$GammaMgmtB, sep=""), width=34), " var=", inputs$GammaMgmtB * inputs$GammaMgmtB * inputs$GammaMgmtA, "\r\n");
    output=c(output, "\r\n");
  }
  
  output=c(output, "AEQ for age class", "\r\n");
  for(Age in inputs$MinAge:inputs$MaxAge){
    output=c(output, "    ", "Age", Age, " AEQ =", staticvars$AEQ[Age], "\r\n");
  }
  output=c(output, "Recruits At Age 1= ", staticvars$RecruitsAtAge1, "\r\n");
  output=c(output, "\r\n");
  output=c(output, "Regime Evaluation Parameters:    QET = ", inputs$DL2, "\r\n");
  output=c(output, "    ", "Lower Escapement Level (LEL)=", inputs$ECrit, "\r\n");
  output=c(output, "    ", "Upper Escapement Level (UEL)=", inputs$ERecovery, "\r\n");
  if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
    output=c(output, "    ", "Max Return (under average variability) =", inputs$BSRa * inputs$AveEnv * inputs$BSRb / 2.71828, "\r\n");
  }else{
    output=c(output, "    ", "Max Return (under average variability) =", inputs$BSRb * inputs$AveEnv, "\r\n");
  }
  output=c(output, "\r\n");
  
  output=c(output, "SUMMARY STATISTICS", "\r\n");
  output=c(output, "All statistics are averaged over repetitions", "\r\n");
  output=c(output, " . . . .                          __________Escapement___________", "\r\n");
  output=c(output, ".   b     Total-Exploit.-Rate . . #fish  %runs   %yrs   %runs   1st LastYrs  pop_size.", "\r\n");
  output=c(output, ". param.  TgtER   CYrER   BYrER   Mort. extnct   <LEL end>UEL  Year   Ave.   at_equil. ", "\r\n");
  
  Buffer = inputs$BufferStart
  for(BufNum in 1:inputs$BufMax){    
    if(inputs$StepFunc == "POP"){
      PBff = Buffer
      EBff = 1
    }
    if(inputs$StepFunc == "ER"){
      PBff = 1
      EBff = Buffer
    }
    output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7));  
    output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff, digits=2),nsmall=2,width=7));  
    output=c(output, format(round(SummaryStats$AvgCaHR[BufNum], digits=3),nsmall=3,width=7));  
    output=c(output, format(round(SummaryStats$BufAvgBYrHR[BufNum], digits=3),nsmall=3,width=7));  
    output=c(output, format(round(SummaryStats$AvgAEQMort[BufNum]),nsmall=0,width=6));  
    output=c(output, format(round(100 * SummaryStats$PropExt[BufNum],digits=1),nsmall=1,width=6));  
    output=c(output, format(round(100 * SummaryStats$AvgECrit[BufNum],digits=1),nsmall=1,width=6));  
    output=c(output, format(round(100 * SummaryStats$PropRec[BufNum],digits=1),nsmall=1,width=6));  
    output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, 1]),nsmall=0,width=7));  
    output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, inputs$NYears]),nsmall=0,width=6));  
    
    if(inputs$StepFunc == "POP"){
      #' njs MxR adjusted for average environmental conditions
      if(inputs$SRType %in% c("HOC2", "HOC3", "HOC4")){
        output=c(output, format(round(PBff * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime])),nsmall=0,width=8));  
      }
      if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
        output=c(output, format(round(PBff * inputs$BSRb * log(inputs$AveEnv * inputs$BSRa * (1 - inputs$TargetU[inputs$BaseRegime]))),nsmall=0,width=8));  
      }
      if(inputs$SRType %in% c("BEV2", "BEV3", "BEV4")){
        output=c(output, format(round(PBff * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime] - ((1 / inputs$BSRa) * inputs$AveEnv))),nsmall=0,width=8));  
      }
      
    }
    output=c(output, "\r\n");
    Buffer = Buffer + inputs$BufferStep
  } #for loop BufNum
  cat(file=file, output)
  
  invisible(output)
}