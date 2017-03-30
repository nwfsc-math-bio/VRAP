#' @title SaveBYrData
#' @description Write the .byr output file
#' @param inputs Inputs from .rav file
#' @param SummaryStats list with the summary statistics to be updated
#' @return Nothing.  Writes file.
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

# for(Byr in  -1:(inputs$NYears - inputs$MaxAge)){
#   output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6));
#   output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7));
#   output=c(output, format(Byr,nsmall=0,width=7));
#     #plus 2 because R indexing starts at 1
#   output=c(output, format(round(SummaryStats$MinBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
#   output=c(output, format(round(SummaryStats$AvgBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
#   output=c(output, format(round(SummaryStats$MaxBYrHR[BufNum, Byr+2],digits=3), nsmall=3,width=8));
#   output=c(output, "\n"); 
#     } #for loop for Byr

Byrs=-1:(inputs$NYears - inputs$MaxAge)
col1=format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6)
col2=format(round(inputs$BSRb * PBff),nsmall=0,width=7)
col3=format(Byrs,nsmall=0,width=7)
col4=format(round(SummaryStats$MinBYrHR[BufNum, Byrs+2],digits=3), nsmall=3,width=8)
col5=format(round(SummaryStats$AvgBYrHR[BufNum, Byrs+2],digits=3), nsmall=3,width=8)
col6=format(round(SummaryStats$MaxBYrHR[BufNum, Byrs+2],digits=3), nsmall=3,width=8)
col7="\n"
tmp=cbind(col1, col2, col3, col4, col5, col6, col7)
output=c(output, as.vector(t(tmp)))

} #for loop for Buffer
cat(file=file, output,sep="")
}
