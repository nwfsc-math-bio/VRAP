Main = function(InFile=NULL, OutFileBase=NULL, NRuns=-1, silent=FALSE){
  require(stringr)
  
  #if not called with input file, then user is prompted to input one
  if(is.null(InFile)) InFile = file.choose()
  if(!file.exists(InFile)) stop("Specified input file does not exist.")
  if(is.null(OutFileBase)){
    tmp=strsplit(InFile,"\\\\")[[1]]
    InFileBase=tmp[length(tmp)]
    tmp=strsplit(InFileBase,"/")[[1]]
    InFileBase=tmp[length(tmp)]   
    if(str_detect(InFileBase,"[.]")){
      OutFileBase=strsplit(InFileBase,"[.]")[[1]][1]; 
    }else{
      OutFileBase=InFileBase;
    }
  }
  
  
#Two lists will be passed in and out of functions
#   inputs = list() #is everything from the .rav file
#   staticvars = list() #is anything computed from that; static
  
  #'READ INPUT DATA AND CALCULATE AEQs
  #'direct from .rav file or simple calculation from rav file inputs
  inputs=GetInput(InFile)
  
  if(NRuns>0) inputs$NRuns=NRuns;
  
  #add the output file names to the inputs
  inputs = SetOutFileNames(OutFileBase, inputs)

  out=RunSims(inputs, silent)
  
  #'SAVE SUMMARY RESULTS .sum
  if!(silent) cat("\nSaving summary...\n")
  SaveSummary(out$inputs, out$SummaryStats, out$staticvars)
  
  #'SAVE ESCAPEMENT DATA .esc
  if(!silent) cat("Saving escapement data...\n")
  SaveEscpmntData(out$inputs, out$SummaryStats)
  
  #'SAVE BROOD YEAR EXPLOITATION RATE DATA .byr
  if(!silent) cat("Saving BYr year data...\n")
  SaveBYrData(out$inputs, out$SummaryStats)

  return(out)
}