#######################################################################
## RunSims takes the input list and runs the VRAP functions
#######################################################################
RunSims = function(inputs, silent) {

  ## Set up list that will hold static computed variables.
  ## These don't change with each rep or buffer
  staticvars=list()

  #'CALCULATE AEQs
  #'staticvars are computed variables; they are static
  staticvars$AEQ = AEQcalc(inputs) 
  
  #'COMPUTE FACTOR TO TRANSLATE AEQ RECRUITMENT TO AGE 1
  staticvars$RecruitsAtAge1 = Recruits(inputs)

  ptm <- proc.time()
  ##----------------- PROGRAM EXECUTION SECTION --------------------------------
  
  Buffer = inputs$BufferStart
  ##Set up the SummaryStats
  SummaryStats = SetupSummaryStats(inputs)

  ##Set up the YearStats
  YearStats = list()
  YearStats$AEQMort = matrix(0, inputs$NYears, inputs$MaxAge)
  YearStats$Escpmnt = matrix(0, inputs$NYears, inputs$MaxAge)
  YearStats$TotAEQMort = matrix(NA, inputs$NYears) 
  YearStats$TotAdultEscpmnt = matrix(NA, inputs$NYears)
  YearStats$TotEscpmnt = matrix(NA, inputs$NYears)
  YearStats$RanFlow = matrix(NA, inputs$NYears)
  YearStats$RanMarine = matrix(NA, inputs$NYears)

  ## function combines gathered individual loop results
  ## re-formatted into SummaryStats when Buffer loops are done
  stats.combine <- function(s1, s2) {
    bn <- s2$bufnum
    scalars <- list("FirstRanMarine","FirstRanFlow")
    vecs <- list("AvgAEQMort","AvgECrit","AvgCaHR","BufAvgBYrHR","PropExt",
                 "PropRec","AveRanFlow","AveRanMarine")
    
    for (elem in names(s1$ss)) {
      targ <- paste0('^',elem,'$')
      if (any(grepl(targ,scalars))) {
        s1$ss[[elem]] <- s2$ss[[elem]]
      } else {
        if (any(grepl(targ,vecs))) {
          s1$ss[[elem]][bn] <- s2$ss[[elem]][bn]
        } else {
          s1$ss[[elem]][bn,] <- s2$ss[[elem]][bn,]
        }
      }
    }
    return(s1)
  }
  
  if(!silent) prev=progressBar()
  BufNum <- NULL
  ## For each ER or Pop Cap level, go loop through NRuns,
  ## and for each NRun, loop through Year
  loopres <- foreach(BufNum = 1:inputs$BufMax, .combine=stats.combine) %dopar% {
    ## set ER level or Pop Cap (SRb) level

    Buffer = inputs$BufferStart + (BufNum - 1) * inputs$BufferStep

    ## INITIALIZE BUFFER SPECIFIC PARAMETERS AND ARRAYS
    out=BufferInit(Buffer, inputs)
    inputs$DR=out$DR
    ## This is the ER or Pop Cap to use for this loop
    ## used in CompEscpmnt
    BufTargetU = out$BufTargetU
    ## This is the SRb to use when StepFunc = "POP";
    ## not used when StepFunc = "ER"
    ## used in CompRecruits and CompStats
    BufSRb = out$BufSRb
    
    ## REPETITION LOOP
    for(Rep in 1:inputs$NRuns){
      if(!silent) prev=progressBar((inputs$NRuns*(BufNum-1)+Rep)/(inputs$BufMax*inputs$NRuns),prev)
      
      #'INITIALIZE REPETITION SPECIFIC PARAMETERS AND ARRAYS
      #'repvar is a list of variables that change each year:
      #'Cohort, LastRanError, LastRanFlow, LastRanMarine
      #'These are updated with each year
      #'starts Cohort at the init # at each age in the input file
      #'Cohort seems to mean population #s at each age
      #'YearStats is a list of variables that are saved for each year
      #'SummaryStats passed in to fix first year Ran to be same across reps
      repvars = RepInit(inputs)
      
      if(Rep == 1){
        SummaryStats$FirstRanMarine = repvars$LastRanMarine
        SummaryStats$FirstRanFlow = repvars$LastRanFlow
      }
      
      #'BEGIN Year LOOP
      for(Year in 1:inputs$NYears){
                                        #Save this for Summary at end
        YearStats$RanMarine[Year] = repvars$LastRanMarine
        YearStats$RanFlow[Year] = repvars$LastRanFlow 
        
        #'NATURAL MORTALITY PROCESS
        #'Update Cohort (pop size) at Age with Natural Mortality
        repvars$Cohort =CompNatMort(inputs, repvars$Cohort)
        
        #'COMPUTE ESCAPEMENT USING BASE LEVEL EXPLOITATION RATE
        ## Escapment is spawners
        YearStats=CompEscpmnt(inputs$BaseRegime, Year, inputs, BufTargetU, repvars$Cohort, staticvars$AEQ, YearStats)
        repvars$TempCohort = YearStats$TempCohort
        
        #'CHECK STOCK STATUS
        #'Going to be a number between 1 and NumBreaks
        Status = CompStockStatus(YearStats$TotAdultEscpmnt[Year], inputs)
        
        #'ADJUST REGIME IF WARRANTED
        if(Status != inputs$BaseRegime){
          YearStats=CompEscpmnt(Status, Year, inputs, BufTargetU, repvars$Cohort, staticvars$AEQ, YearStats)
          repvars$TempCohort = YearStats$TempCohort
        }
        
        #'AGE COHORT
        #'now update cohort (non-spawner pop size at age) for next year
        #'using the updated "cohort" from CompEscpmnt
        repvars$Cohort = CompAgeCohort(repvars$TempCohort, repvars$Cohort, inputs)
        
        #'COMPUTE RECRUITS FROM ESCAPEMENT
        #'Uses SR (Escpmnt to AEQRecruits) function with error added to that to get AEQRecruits from this years spawners (Escpmnt)
        #'Needs to then translate that to age 1 indiv in pop (Cohort[1])
        #'We know AEQRecruit.  How many Age 1 individuals does that translate to?  Age1 * (1- total fraction lost) = AEQRecruits
        #'So Age1 = AEQRecruits/(1-total fraction lost)
        #'Set Cohort[1], LastRanError, LastRanFlow, LastRanMarine
        repvars = CompRecruits(YearStats, Year, inputs, repvars, staticvars, BufSRb)

        #'SAVE YEAR DATA
        #'stores the year data that will be needed for the statistics at the end
        YearStats=SaveYearData(Year, YearStats)
        
      } ## for loop for Year
      ## this cumulates some stats over each rep
      SummaryStats= CompStatsEEH(BufNum, inputs, BufSRb, YearStats, SummaryStats)
    } ##for loop for Rep
    ## return list value for loop result combination
    list(bufnum=BufNum, ss=SummaryStats)
  } #for loop for BufNum

  ## extract SummaryStats from complete loop results list
  SummaryStats <- loopres$ss
  
  comp.time = proc.time() - ptm
  
  return(list(inputs=inputs, SummaryStats=SummaryStats, staticvars=staticvars, time=comp.time))
}

