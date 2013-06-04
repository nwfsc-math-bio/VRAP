#'*****   SaveBYrData  ******
SaveBYrData = function(inputs, SummaryStats){

file = inputs$OutFileByr

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

for(Byr in  -1:(inputs$NYears - inputs$MaxAge)){
  output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6));
  output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7));
  output=c(output, format(Byr,nsmall=0,width=7));
    #plus 2 because R indexing starts at 1
  output=c(output, format(round(SummaryStats$MinBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
  output=c(output, format(round(SummaryStats$AvgBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
  output=c(output, format(round(SummaryStats$MaxBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
  output=c(output, "\r\n"); 
    } #for loop for Byr
} #for loop for Buffer
cat(file=file, output)
}
