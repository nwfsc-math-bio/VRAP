#'*****  SaveEscpmntData  *****
SaveEscpmntData = function(inputs, SummaryStats){

file = inputs$OutFileEsc;

output=""

BufNum = 0
for(Buffer in seq(inputs$BufferStart,inputs$BufferEnd,inputs$BufferStep)){
  BufNum = BufNum + 1
    
  if(inputs$StepFunc == "POP"){
    PBff = Buffer
    EBff = 1
  }
  if(inputs$StepFunc == "ER"){
    PBff = 1
    EBff = Buffer
  }
  
for(Year in 1:inputs$NYears){
  output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6));
  output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7));
  output=c(output, format(Year,nsmall=0,width=7));
#plus 2 because R indexing starts at 1
  output=c(output, format(round(SummaryStats$MinEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
  output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
  output=c(output, format(round(SummaryStats$MaxEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
  output=c(output, "\r\n");  
} #for loop for year
} #for loop for buffer
cat(file=file, output)
}
