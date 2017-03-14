# This function takes inputs list and creates a RAV file
WriteRavFile = function( inputs, filename ) {
  includeMarineSurvival=includeFlow=FALSE
  if(inputs$SRType %in% c("BEV3","HOC3","RIC3")) includeFlow=TRUE
  if(inputs$SRType %in% c("BEV4","HOC4","RIC4")){ includeMarineSurvival=TRUE; includeFlow=TRUE }
  
  ravText <- paste(
    inputs$Title, ", Title\n",
    inputs$RanSeed,", Random seed; 0 gives random seed; numbers give fixed seed\n",
    inputs$NRuns,", Number of runs\n",
    inputs$NYears,", Number of years\n",
    inputs$MinAge,", ", inputs$MaxAge,", Minimum and maximum age (for now this is fixed; do not change)\n",
    inputs$ConvergeCrit,", Convergence criterion (% error) for target ER\n",
    inputs$CenterCov, ", Center covariates in SR function (as DM does)?\n",
    inputs$SRType,", Spawner Recruit function (Ric2;Ric3;Ric4; Bev2;Bev3;Bev4; Hoc2;Hoc3;Hoc4)\n",
    inputs$BSRa,",",inputs$BSRb,",",inputs$BSRc,",", inputs$BSRd, ", S/R a; b parameters; c (Marine); d (Freshwater)\n",
    ifelse(includeMarineSurvival, paste(inputs$MarAve,inputs$MarCV,sep=", "),""),ifelse(includeMarineSurvival,", ",""),"Mean and CV  for marine survival index (M^c)\n",
    ifelse(includeMarineSurvival, paste(inputs$TrndCycM,", ",sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Marine Survival?,\n",
    ifelse(includeMarineSurvival, paste(inputs$TCM1, ", ", inputs$TCM2, ", ", inputs$TCM3, ", ",sep=""),""), "Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    ifelse(includeFlow, paste(inputs$FlowAve,inputs$FlowCV,sep=", "),""),ifelse(includeFlow, ", ",""),"Mean and CV  for flow (or other fw) index (exp(dF))\n",
    ifelse(includeFlow, paste(inputs$TrndCycF,", ", sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Flow?,\n",
    ifelse(includeFlow, paste(inputs$TCF1, ", ", inputs$TCF2, ", ", inputs$TCF3,", ", sep=""),""), "Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    inputs$depen,", Depensation? (YES or NO)\n",
    inputs$DL1,", ",inputs$DL2,",",inputs$DR,", 1) Esc. level for depensation to start 2) QET 3)% predicted return at QET (or for r/s=1 third parameter = 1)\n",
    inputs$EscChoice,", Determine recruits from adult spawners (not total)?\n",
    inputs$SurvScale,", Stock-recruit variation (YES or NO)\n",
    ifelse(inputs$SurvScale=="YES", paste(inputs$SRErrorA,inputs$SRErrorB, inputs$ResCorParameter,sep=", "),""),ifelse(inputs$SurvScale=="YES", ", ",""),"A and B parameters S/R error and error autocorrelation\n",
    inputs$MarSurv, ", Smolt to adult survival w/variation (YES or NO);  if Yes beta variation on cohort size (2 parameters) on next line\n",
    ifelse(inputs$MarSurv=="YES", paste(inputs$BetaMarA,inputs$BetaMarA, inputs$BetaMarA,sep=", "),""),ifelse(inputs$MarSurv=="YES", ", ",""),"Beta distribution a and b parameters and autocorrelation\n",
    inputs$NumBreakPoints, ", Number of breakpoints; in escapement to trigger management action\n",
    inputs$BaseRegime, ", Level to use as base regime\n",
    ifelse(inputs$NumBreakPoints==1, paste(inputs$EscpmntBreakPoint[1],", ",inputs$TargetU[1],", Esc & ER if esc less than this level\n", sep=""),""),
    ifelse(inputs$NumBreakPoints==1,inputs$TargetU[2],inputs$TargetU[1]),", base exploitation rate\n",
    inputs$MgmtError, ", Include error (YES or NO) in ER management\n",
    ifelse(inputs$MgmtError == "YES", paste(inputs$GammaMgmtA,", ",inputs$GammaMgmtB,", Gamma parameters for management error\n",sep=""),""),
    inputs$ECrit,", Lower escapement threshold\n",
    inputs$ERecovery,", ", inputs$EndAv,", Upper escapement threshold (MSY);  # yrs to ave.\n",
    inputs$StepFunc,", Step ER (ER) or  Pop Capacity (Pop)?\n",
    inputs$BufferStep,", Buffer step size as percent of base ER or Pop capacity\n",
    inputs$BufferStart,", ",inputs$BufferEnd,", Min & max buffer (x base for start & end)\n",
    paste(paste(inputs$CohortStart,", Initial population size at Age ",sep=""),1:length(inputs$CohortStart),"\n",collapse=""),
    paste(paste(inputs$NatMort,", Age",sep=""),1:length(inputs$NatMort),"natural mortality\n",collapse=""),
    paste(paste(inputs$MatRate[inputs$MinAge:inputs$MaxAge],", Age",sep=""),inputs$MinAge:inputs$MaxAge,"average maturation rate\n",collapse=""),
    paste(paste(paste(inputs$PTU[inputs$MinAge:inputs$MaxAge],", ",inputs$MatU[inputs$MinAge:inputs$MaxAge],sep=""),", Age",sep=""),inputs$MinAge:inputs$MaxAge,"average mixed-maturity and mature fishery fishing rates\n",collapse=""),
    "endofinput, end of input indicator\n",
    sep="")
  
  # write to file
  cat(ravText,file=filename)
}
