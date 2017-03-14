Main = function(InFile=NULL, OutFileBase=NULL, 
                NRuns=-1, NYears=-1, Title=-1,
                TargetStart=-1, TargetEnd=-1, TargetStep=-1,
                ERecovery=-1, QET=-1, ECrit=-1, NewRavFileName="tmprav.rav",
                forceNewRav=NULL, silent=FALSE, lcores=1){

  c1 = makeCluster(lcores)
  registerDoParallel(c1)
  
  ## if not called with input file, then user is prompted to input one
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
  
  
  ## Two lists will be passed in and out of functions
  ##    inputs = list() ## is everything from the .rav file
  ##    staticvars = list() ## is anything computed from that; static
  
  ## 'READ INPUT DATA AND CALCULATE AEQs
  ## 'direct from .rav file or simple calculation from rav file inputs
  inputs=GetInput(InFile)
  
  # Check if any inputs will be changed
  newrav=FALSE
  if(NRuns>0){ inputs$NRuns=NRuns; newrav=TRUE }#NRuns passed in
  if(NYears>0){ inputs$NYears=NYears; newrav=TRUE } #NYears passed in
  if(Title != -1){ inputs$Title=NYears; newrav=TRUE } #NYears passed in
  if(TargetStart>0){ inputs$BufferStart=TargetStart/inputs$TargetU; newrav=TRUE }
  if(TargetEnd>0){ inputs$BufferEnd=TargetEnd/inputs$TargetU; newrav=TRUE }
  if(TargetStep>0){ inputs$BufferStep=TargetStep/inputs$TargetU; newrav=TRUE }
  if(QET>0){ inputs$DL2=QET; newrav=TRUE }
  if(ERecovery>0){ inputs$ERecovery=ERecovery; newrav=TRUE }
  if(ECrit>0){ inputs$ECrit=ECrit; newrav=TRUE }

  ## override newrav if desired
  newrav <- if(!is.null(forceNewRav) && !is.na(forceNewRav) &&
                 is.logical(forceNewRav)) {forceNewRav} else {newrav}
  
  #This was computed in GetInput(); need to recompute
  inputs$BufMax = round((inputs$BufferEnd - inputs$BufferStart) / inputs$BufferStep + 1)
  #Howard: I doubt this is ok for unix; needed a dir to save the new rav file
  if(newrav){
     WriteRavFile(inputs, paste(dirname(InFile),"/",NewRavFileName,sep=""))
  }
  
  ## add the output file names to the inputs
  inputs = SetOutFileNames(OutFileBase, inputs)
  
  out=RunSims(inputs, silent)
  
  outtm <- proc.time()
  
  ## 'SAVE SUMMARY RESULTS .sum
  if(!silent) cat("\nSaving summary...\n")
  SaveSummary(out$inputs, out$SummaryStats, out$staticvars)
  
  ## 'SAVE ESCAPEMENT DATA .esc
  if(!silent) cat("Saving escapement data...\n")
  SaveEscpmntData(out$inputs, out$SummaryStats)
  
  ## 'SAVE BROOD YEAR EXPLOITATION RATE DATA .byr
  if(!silent) cat("Saving BYr year data...\n")
  SaveBYrData(out$inputs, out$SummaryStats)
  
  outtm <- proc.time() - outtm
  
  stopCluster(c1)
  
  return(c(out,output.time=outtm[3]))
}
