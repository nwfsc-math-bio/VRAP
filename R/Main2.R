Main2 = function(InFile=NULL, OutFileBase=NULL){
  require(stringr)
# Enable just in time compile
#   require(compiler)
#   enableJIT(3)
  
#   #if not called with input file, then user is prompted to input one
#   if(is.null(InFile)) InFile = file.choose()
#   if(!file.exists(InFile)) stop("Specified input file does not exist.")
#   if(is.null(OutFileBase)){
#     tmp=strsplit(InFile,"\\\\")[[1]]
#     InFileBase=tmp[length(tmp)]
#     tmp=strsplit(InFileBase,"/")[[1]]
#     InFileBase=tmp[length(tmp)]   
#     if(str_detect(InFileBase,"[.]")){
#       OutFileBase=strsplit(InFileBase,"[.]")[[1]][1]; 
#     }else{
#       OutFileBase=InFileBase;
#     }
#   }
#   
#   
# #Two lists will be passed in and out of functions
# #   inputs = list() #is everything from the .rav file
# #   staticvars = list() #is anything computed from that; static
#   
#   #'READ INPUT DATA AND CALCULATE AEQs
#   #'direct from .rav file or simple calculation from rav file inputs
#   inputs=GetInput(InFile)
#   #add the output file names to the inputs
#   inputs = SetOutFileNames(OutFileBase, inputs)
#   
#   #Set up list that will hold static computed variables.  These don't change with each rep or buffer
#   staticvars=list()
#   #'CALCULATE AEQs
#   #'staticvars are computed variables; they are static
#   staticvars$AEQ = AEQcalc(inputs) 
#   
#   #'COMPUTE FACTOR TO TRANSLATE AEQ RECRUITMENT TO AGE 1
#   staticvars$RecruitsAtAge1 = Recruits(inputs)
# 
#   
#   #'----------------- PROGRAM EXECUTION SECTION ----------------------------------
#   
#   Buffer = inputs$BufferStart
#   #Set up the SummaryStats
#   SummaryStats = SetupSummaryStats(inputs)
# 
#   #Set up the YearStats
#   YearStats = list()
#   YearStats$AEQMort = matrix(0, inputs$NYears, inputs$MaxAge)
#   YearStats$Escpmnt = matrix(0, inputs$NYears, inputs$MaxAge)
#   YearStats$TotAEQMort = matrix(NA, inputs$NYears) 
#   YearStats$TotAdultEscpmnt = matrix(NA, inputs$NYears)
#   YearStats$TotEscpmnt = matrix(NA, inputs$NYears)
#   YearStats$RanFlow = matrix(NA, inputs$NYears)
#   YearStats$RanMarine = matrix(NA, inputs$NYears)
#   
#   prev=progressBar()
#   #'For each ER or Pop Cap level, go loop through NRuns, and for each NRun, loop through Year 
#   for(BufNum in 1:inputs$BufMax){
#     
#     #'INITIALIZE BUFFER SPECIFIC PARAMETERS AND ARRAYS
#     out=BufferInit(Buffer, inputs)
#     inputs$DR=out$DR
#     #This is the ER or Pop Cap to use for this loop
#     #used in CompEscpmnt
#     BufTargetU = out$BufTargetU
#     #This is the SRb to use when StepFunc = "POP"; not used when StepFunc = "ER"
#     #used in CompRecruits and CompStats
#     BufSRb = out$BufSRb
#           
#     #'REPETITION LOOP
#     for(Rep in 1:inputs$NRuns){
#       prev=progressBar((inputs$NRuns*(BufNum-1)+Rep)/(inputs$BufMax*inputs$NRuns),prev)
#       
#       #'INITIALIZE REPETITION SPECIFIC PARAMETERS AND ARRAYS
#       #'repvar is a list of variables that change each year: Cohort, LastRanError, LastRanFlow, LastRanMarine
#       #'These are updated with each year
#       #'starts Cohort at the init # at each age in the input file
#       #'Cohort seems to mean population #s at each age
#       #'YearStats is a list of variables that are saved for each year
#       #'SummaryStats passed in to fix first year Ran to be same across reps
#       repvars = RepInit(inputs)
#       
#       if(Rep == 1){
#         SummaryStats$FirstRanMarine = repvars$LastRanMarine
#         SummaryStats$FirstRanFlow = repvars$LastRanFlow
#       }
#       
#       #'BEGIN Year LOOP
#       for(Year in 1:inputs$NYears){
#         #Save this for Summary at end
#         YearStats$RanMarine[Year] = repvars$LastRanMarine
#         YearStats$RanFlow[Year] = repvars$LastRanFlow 
#         
#         #'NATURAL MORTALITY PROCESS
#         #'Update Cohort (pop size) at Age with Natural Mortality
#         repvars$Cohort =CompNatMort(inputs, repvars$Cohort)
#         
#         #'COMPUTE ESCAPEMENT USING BASE LEVEL EXPLOITATION RATE
#         #Escapment is spawners
#         YearStats=CompEscpmnt(inputs$BaseRegime, Year, inputs, BufTargetU, repvars$Cohort, staticvars$AEQ, YearStats)
#         repvars$TempCohort = YearStats$TempCohort
#         
#         #commented out since I want to test without the regime feature first
#         # #'CHECK STOCK STATUS
#         # #'Going to be a number between 1 and NumBreaks
#         # Status = CompStockStatus(YearStats$TotAdultEscpmnt, inputs)
#         # 
#         # #'ADJUST REGIME IF WARRANTED
#         # if(Status != inputs$BaseRegime){
#         # YearStats=CompEscpmnt(Status, Year, inputs, BufTargetU, repvars$Cohort, staticvars$AEQ, YearStats)
#         # }
#                 
#         #'AGE COHORT
#         #'now update cohort (non-spawner pop size at age) for next year using the updated "cohort" from CompEscpmnt
#         repvars$Cohort = CompAgeCohort(repvars$TempCohort, repvars$Cohort, inputs)
#         
#         #'COMPUTE RECRUITS FROM ESCAPEMENT
#         #'Uses SR (Escpmnt to AEQRecruits) function with error added to that to get AEQRecruits from this years spawners (Escpmnt)
#         #'Needs to then translate that to age 1 indiv in pop (Cohort[1])
#         #'We know AEQRecruit.  How many Age 1 individuals does that translate to?  Age1 * (1- total fraction lost) = AEQRecruits
#         #'So Age1 = AEQRecruits/(1-total fraction lost)
#         #'Set Cohort[1], LastRanError, LastRanFlow, LastRanMarine
#         repvars = CompRecruits(YearStats, Year, inputs, repvars, staticvars, BufSRb)
# 
#         #'SAVE YEAR DATA
#         #'stores the year data that will be needed for the statistics at the end
#         YearStats=SaveYearData(Year, YearStats)
#         
#       } #for loop for Year
#       #this cumulates some stats over each rep
#       SummaryStats= CompStatsEEH(BufNum, inputs, BufSRb, YearStats, SummaryStats)
#      } #for loop for Rep
#     
#     # next ER level or Pop Cap (SRb) level
#     Buffer = Buffer + inputs$BufferStep
#   } #for loop for BufNum
#   
#   # #'COMPUTE STATISTICS
#   # cat("Computing and saving statistics...")
#   # CompStats(inputs$BufMax, inputs, BufSRb)
#   
#   #'SAVE SUMMARY RESULTS .sum
#   cat("\nSaving summary...\n")
#   SaveSummary(inputs, SummaryStats, staticvars)
#   
#   #'SAVE ESCAPEMENT DATA .esc
#   cat("Saving escapement data...\n")
#   SaveEscpmntData(inputs, SummaryStats)
#   
#   #'SAVE BROOD YEAR EXPLOITATION RATE DATA .byr
#   cat("Saving BYr year data...\n")
#   SaveBYrData(inputs, SummaryStats)

  return(inputs)
}