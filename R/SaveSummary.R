#' @title SaveSummary
#' @description Write the .esc output file
#' @param inputs Inputs from .rav file
#' @param SummaryStats list with the summary statistics to be updated
#' @param staticvars Static variables
#' @return Nothing.  Writes file.
SaveSummary = function(inputs, SummaryStats, staticvars){
  
  file = inputs$OutFileSum;
  output=""
  # PRINT HEADER INFORMATION
  output=c(output, format(paste("RAPVIABILITY (R) Version ", packageVersion("VRAP")),width=54), "Date:", format(Sys.Date()),"\n");
  output=c(output, format("Title:",width=14), inputs$Title, "\n");
  output=c(output, format("Input File:",width=19), inputs$InFile, "\n");
  output=c(output, "Copy of Input File:", inputs$InFile, "\n");
  output=c(output, "\n");
  
  output=c(output, "Basic Simulation Input Parameters:","\n");
  output=c(output, "    ","# of Years= ", inputs$NYears, "  # of Reps= ", inputs$NRuns, "  HR Conv.Crit= ", inputs$ConvergeCrit, "  Seed ", inputs$RanSeed, " \n");
  output=c(output, " Range start  ", inputs$BufferStart, "  end  ", inputs$BufferEnd, "  by  ", inputs$BufferStep, " \n");
  output=c(output, "\n");
  
  output=c(output, "Stock Recruit Function Input Parameters:", "\n");
  output=c(output, "    ", "Function Type: ", inputs$SRType, "\n");
    
  if(inputs$SRType == "HOC2"){
    output=c(output, "  ", "Recruits = min(a*Spawners, b)", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa," \n");
    output=c(output, "                        ", "b=max recruits= ", inputs$BSRb, " \n");
  }
  if(inputs$SRType == "HOC3"){
    output=c(output, "  ",  "Recruits = min(a*Spawners, b)* exp(d*FWindex)", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=max recruits= ", inputs$BSRb, " \n");
    output=c(output,"                                  ", "d=FW parameter=", inputs$BSRd, " \n");
  }
  if(inputs$SRType == "HOC4"){
    output=c(output, "  ",  "Recruits = min(a*Spawners, b)* exp(d*FWindex) * MarineIndex^c", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=max recruits= ", inputs$BSRb, " c=MS parameter= ", inputs$BSRc, " d=FW parameter=", inputs$BSRd, " \n");
  }
  
  if(inputs$SRType == "RIC2"){
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b)", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa," \n");
    output=c(output,"                        b=capacity= ", inputs$BSRb, " \n");
  }
  if(inputs$SRType == "RIC3"){ 
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b + d*FWindex)", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=capacity= ", inputs$BSRb, " d=FW parameter=", inputs$BSRd, " \n");
  }
  if(inputs$SRType == "RIC4"){ 
    output=c(output, "  ", "Recruits = a * Spawners * exp(-Spawners/b + d*FWindex) * MarineIndex^c", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=capacity= ", inputs$BSRb, " c=MS parameter= ", inputs$BSRc, " d=FW parameter=", inputs$BSRd, " \n");
  }
  
  if(inputs$SRType == "BEV2"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] ", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " \n");
    output=c(output,"                        ", "b=max recruits ", inputs$BSRb, " \n");
  }
  if(inputs$SRType == "BEV3"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] * exp(d*FWindex)", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=max recruits ", inputs$BSRb, " d=FW parameter", inputs$BSRd, " \n");
  }
  if(inputs$SRType == "BEV4"){
    output=c(output, "  ", "Recruits = 1/[(1/b) + 1/(a*Spawners)] * exp(d*FWindex) * MarineIndex^c", "\n");
    output=c(output, "    ", "a=productivity= ", inputs$BSRa, " b=max recruits ", inputs$BSRb, " c=MS parameter ", inputs$BSRc, " d=FW parameter", inputs$BSRd, "\n");
  }
  output=c(output, "\n");
  
  if(inputs$SurvScale == "YES"){
    if(inputs$SRType %in% c("HOC2", "RIC2", "BEV2")){
      output=c(output, "Stock-Recruit Error Parameters (gamma distr.) [R=f(s)*e]:", "\n");
      output=c(output, "    ", format(paste("A= ", inputs$SRErrorA,sep=""),width=25), "B= ", inputs$SRErrorB, " \n");
      output=c(output, "\n");
    }
    if(inputs$SRType %in% c("HOC3", "RIC3", "BEV3", "HOC4", "RIC4", "BEV4")){
      output=c(output, "Stock-Recruit Error Parameters [R=f(S)*exp(e)]:", "\n");
      output=c(output, "    ", "MSE= ", inputs$SRErrorB, "    Res Cor=", inputs$ResCorParameter, " \n");
      output=c(output, "\n");
    }
  }
  
  if(inputs$depen == "YES"){
    output=c(output, "Depensation at escap: ", inputs$DL1, " QET: ", inputs$DL2, " fraction of depensation at QET ", inputs$DR, " \n");
    output=c(output, "\n");
  }
  
  if(inputs$MarSurv == "YES"){
    if( inputs$SRType %in% c("HOC2", "RIC2", "BEV2", "HOC3", "RIC3", "BEV3")){
      output=c(output, "Smolt to Adult Survival Rate Parameters:", "\n");
      output=c(output, format(paste("    ", "Beta_A=", inputs$BetaMarA, sep=""), width=34), " Beta_B=", inputs$BetaMarB, "\n");
      output=c(output, "\n");
    }else{
      output=c(output, "Should not be using this for ",  inputs$SRType, "\n");
      output=c(output, "\n");
    }
  }
  
  BufMax = (inputs$BufferEnd - inputs$BufferStart) / inputs$BufferStep + 1
  
  if( inputs$SRType %in% c("HOC3", "RIC3", "BEV3")){
    output=c(output, "Freshwater Survival Parameters:", "\n");
    output=c(output, "    ", "Gamma A= ", inputs$GammaFlowA, "    Gamma B= ", inputs$GammaFlowB, " \n");
    output=c(output, "    ", "mean= ", inputs$GammaFlowA * inputs$GammaFlowB, "        var= ", inputs$GammaFlowB * inputs$GammaFlowB * inputs$GammaFlowA, " \n");
    output=c(output, "    ", inputs$TrndCycM, ifelse(is.null(inputs$TrndCycM),"","  "), inputs$TCF1, "  ", inputs$TCF2, "  ", inputs$TCF3, " \n");
    output=c(output, "    ", "First Flow index generated =  ", SummaryStats$FirstRanFlow, " \n");
    output=c(output, "    ", "Average Flow index generated =  ", mean(SummaryStats$AveRanFlow), " \n");
    output=c(output, "\n");
  }
  
  if( inputs$SRType %in% c("HOC4", "RIC4", "BEV4")){
    output=c(output, "Marine Survival Parameters:", "\n"); 
    output=c(output, "    ", "Gamma A= ", inputs$GammaMarA, "    Gamma B= ", inputs$GammaMarB, " \n");
    output=c(output, "    ", "mean= ", inputs$MarAve, "        st dev= ", inputs$MarSD, " \n");
    output=c(output, "    ", inputs$TrndCycM, ifelse(is.null(inputs$TrndCycM),""," "), inputs$TCM1, "  ", inputs$TCM2, "  ", inputs$TCM3, " \n");
    output=c(output, "    ", "First Marine index generated =  ", SummaryStats$FirstRanMarine, " \n");
    output=c(output, "    ", "Average Marine index generated =  ", mean(SummaryStats$AveRanMarine), " \n");
    output=c(output, "\n");
    output=c(output, "Freshwater Survival Parameters:", "\n");
    output=c(output, "    ", "Gamma A= ", inputs$GammaFlowA, "    Gamma B= ", inputs$GammaFlowB, " \n");
    output=c(output, "    ", "mean= ", inputs$FlowAve, "        st dev= ", inputs$FlowSD, " \n");
    output=c(output, "    ", inputs$TrndCycF, ifelse(is.null(inputs$TrndCycF), "", ""), inputs$TCF1, "  ", inputs$TCF2, "  ", inputs$TCF3, " \n");
    output=c(output, "    ", "First Flow index generated =  ", SummaryStats$FirstRanFlow, " \n");
    output=c(output, "    ", "Average Flow index generated =  ", mean(SummaryStats$AveRanFlow), " \n");
    output=c(output, "\n");
  }
  
  output=c(output, "Fishery Regime Parameters:", "\n");
  if(inputs$NumBreakPoints >= 1){
    for(Break in 1:inputs$NumBreakPoints){
      output=c(output, "    ", "Breakpoint", "              ", format(Break, digits=2), "\n");
      output=c(output, "      ", "HR Below Breakpoint= ", inputs$TargetU[Break], " \n");
      output=c(output, "      ", "Escapement= ", inputs$EscpmntBreakPoint[Break], " \n");
      output=c(output, "\n");
    }
    output=c(output, "      ", "HR Above BreakPoint= ", inputs$TargetU[inputs$NumBreakPoints + 1], " \n");
    output=c(output, "\n");
  }else{
    output=c(output, "    ", "Base ER =  ", inputs$TargetU[inputs$BaseRegime], " \n");
  }
  
  if(inputs$MgmtError == "YES"){
    output=c(output, "Management Variability Parameters:", "\n");
    output=c(output, "    ", "Gamma A= ", inputs$GammaMgmtA, "              Gamma B= ", inputs$GammaMgmtB, " \n");
    output=c(output, "    ", "mean= ", inputs$GammaMgmtA * inputs$GammaMgmtB, "              var= ", inputs$GammaMgmtB * inputs$GammaMgmtB * inputs$GammaMgmtA, " \n");
    output=c(output, "\n");
  }
  
  output=c(output, "AEQ for age class", "\n");
  for(Age in inputs$MinAge:inputs$MaxAge){
    output=c(output, "    ", "Age ", Age, " AEQ = ", staticvars$AEQ[Age], " \n");
  }
  output=c(output, "Recruits At Age 1            ", staticvars$RecruitsAtAge1, " \n");
  output=c(output, "\n");
  output=c(output, "Regime Evaluation Parameters:    QET = ", inputs$DL2, " \n");
  output=c(output, "    ", "Lower Escapement Level (LEL)= ", inputs$ECrit, " \n");
  output=c(output, "    ", "Upper Escapement Level (UEL)= ", inputs$ERecovery, " \n");
  if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
    output=c(output, "    ", "Max Return (under average variability) = ", inputs$BSRa * inputs$AveEnv * inputs$BSRb / 2.71828, " \n");
  }else{
    output=c(output, "    ", "Max Return (under average variability) = ", inputs$BSRb * inputs$AveEnv, " \n");
  }
  output=c(output, "\n");
  
  output=c(output, "SUMMARY STATISTICS", "\n");
  output=c(output, "All statistics are averaged over repetitions", "\n");
  output=c(output, " . . . .                          __________Escapement___________", "\n");
  output=c(output, ".   b     Total-Exploit.-Rate . . #fish  %runs   %yrs   %runs   1st LastYrs  pop_size.", "\n");
  output=c(output, ". param.  TgtER   CYrER   BYrER   Mort. extnct   <LEL end>UEL  Year   Ave.   at_equil. ", "\n");
  
  #This is special to R code to fix bug in VB code that causes odd formating
  # when numbers are very large
  #Dynamically find the column width so formatting isn't off if numbers are big
  cwid5=max(str_length(format(round(SummaryStats$AvgAEQMort),nsmall=0)))
  cwid9=max(str_length(format(round(SummaryStats$AvgEscpmnt[, 1]),nsmall=0)))
  cwid10=max(str_length(format(round(SummaryStats$AvgEscpmnt[, inputs$NYears]),nsmall=0)))
  if(inputs$StepFunc == "POP"){
    PBffs=inputs$BufferStart + (0:(inputs$BufMax-1))*inputs$BufferStep
    if(inputs$SRType %in% c("HOC2", "HOC3", "HOC4")){
      cwid11=max(str_length(format(round(PBffs * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime])),nsmall=0)));  
    }
    if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
      cwid11=max(str_length(format(round(PBffs * inputs$BSRb * log(inputs$AveEnv * inputs$BSRa * (1 - inputs$TargetU[inputs$BaseRegime]))),nsmall=0)));  
    }
    if(inputs$SRType %in% c("BEV2", "BEV3", "BEV4")){
      cwid11=max(str_length(format(round(PBffs * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime] - ((1 / inputs$BSRa) * inputs$AveEnv))),nsmall=0)));  
    }
  }
    
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
    #Hard coded in space between columns so they cannot run into each other
    #subtracted 1 off width to account for this
    output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7)," ");  
    output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff, digits=2),nsmall=2,width=6)," ");  
    output=c(output, format(round(SummaryStats$AvgCaHR[BufNum], digits=3),nsmall=3,width=7)," ");  
    output=c(output, format(round(SummaryStats$BufAvgBYrHR[BufNum], digits=3),nsmall=3,width=7)," ");  
    output=c(output, format(round(SummaryStats$AvgAEQMort[BufNum]),nsmall=0,width=max(7,cwid5))," ");  
    output=c(output, format(round(100 * SummaryStats$PropExt[BufNum],digits=1),nsmall=1,width=6)," ");  
    output=c(output, format(round(100 * SummaryStats$AvgECrit[BufNum],digits=1),nsmall=1,width=6)," ");  
    output=c(output, format(round(100 * SummaryStats$PropRec[BufNum],digits=1),nsmall=1,width=6)," ");  
    output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, 1]),nsmall=0,width=max(7,cwid9))," ");  
    output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, inputs$NYears]),nsmall=0,width=max(6,cwid10)));  
    
    if(inputs$StepFunc == "POP"){
      # njs MxR adjusted for average environmental conditions
      if(inputs$SRType %in% c("HOC2", "HOC3", "HOC4")){
        output=c(output, " ", format(round(PBff * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime])),nsmall=0,width=max(7,cwid11)));  
      }
      if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
        output=c(output, " ", format(round(PBff * inputs$BSRb * log(inputs$AveEnv * inputs$BSRa * (1 - inputs$TargetU[inputs$BaseRegime]))),nsmall=0,width=max(7,cwid11)));  
      }
      if(inputs$SRType %in% c("BEV2", "BEV3", "BEV4")){
        output=c(output, " ", format(round(PBff * inputs$BSRb * inputs$AveEnv * (1 - inputs$TargetU[inputs$BaseRegime] - ((1 / inputs$BSRa) * inputs$AveEnv))),nsmall=0,width=max(7,cwid11)));  
      }
      
    }
    output=c(output, "\n");
    Buffer = Buffer + inputs$BufferStep
  } #for loop BufNum
  cat(file=file, output, sep="")
  
  invisible(output)
}