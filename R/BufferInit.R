# '*****  BufferInit  *****
# 'ADJUST TARGET RATE FOR BUFFER EXPLOITATION

BufferInit = function(Buffer, inputs){
  #These are the internal function variables
  # Defines MxR, SRb, QetR
  # Changes DR, an original input
  rtn.list = list()
  
  if(inputs$StepFunc=="POP"){
    PBff = Buffer #population capacity buffer; SRb x this
    EBff = 1      #exploitation buffer; er x this
  }
  if(inputs$StepFunc=="ER"){ 
    PBff = 1      #population capacity buffer; SRb x this
    EBff = Buffer #exploitation buffer; er x this
  }
    
  BufSRb = PBff * inputs$BSRb  #adjust capacity upward
  
  if(inputs$SRType %in% c("HOC2", "HOC3", "HOC4")){
    MxR = BufSRb * inputs$AveEnv #never used except printed out; Max Recruits adj by average environment
    QetR = inputs$BSRa * inputs$AveEnv * inputs$DL2
  }
  
  if(inputs$SRType %in% c("BEV2", "BEV3", "BEV4")){
    MxR = BufSRb * inputs$AveEnv #never used except printed out;
    QetR = inputs$AveEnv / ((1 / BufSRb) + (1 / inputs$BSRa) * (1 / inputs$DL2))
  }
  
  if(inputs$SRType %in% c("RIC2", "RIC3", "RIC4")){
    MxR = inputs$BSRa * inputs$AveEnv * BufSRb / 2.71828 #never used except printed out;
    QetR = inputs$AveEnv * inputs$BSRa * inputs$DL2 * exp(-inputs$DL2 / BufSRb)
  }
  #cat(paste("Max Recruits", MxR))
  rtn.list$DR = inputs$DR
  #DR is one of the original inputs; as is DL2; QetR is local variable
  if(inputs$DR == 1){
    if(QetR == 0){
      rtn.list$DR = 0
    }else{
      rtn.list$DR = inputs$DL2 / QetR  #TAB: don't like this, changing DR which is one of the original inputs
    }
  }
  
  #NumBreakPoints is an original input
  #This is just the ER to use for a particular sim (Buffer); only changed if StepFunc="ER"
  rtn.list$BufTargetU = c()
  for(Break in 1:(inputs$NumBreakPoints + 1)){
    if(inputs$NumBreakPoints > 1){ #we have 3 or more Buffer targets      
      if(Break == 1){
        rtn.list$BufTargetU[1] = min(inputs$TargetU[2] * EBff, inputs$TargetU[1]) #use the smaller of these 2
      }else{
        rtn.list$BufTargetU[Break] = inputs$TargetU[Break] * EBff
      }
      
    }else{ # we have 1 or 2 Buffer targets
      rtn.list$BufTargetU[Break] = inputs$TargetU[Break] * EBff
    }
  }
  rtn.list$BufSRb = BufSRb  #adjusted if StepFunc = Pop
  return(rtn.list) 
}