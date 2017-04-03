#' @title SaveEscpmntData
#' @description Write the .esc output file
#' @param inputs Inputs from .rav file
#' @param SummaryStats list with the summary statistics to be updated
#' @return Nothing.  Writes file.
SaveEscpmntData = function(inputs, SummaryStats){

file = inputs$OutFileEsc;

output=c("ESCAPEMENT STATISTICS", "\n");
output=c(output, "All statistics are averaged over repetitions", "\n");
output=c(output, "                      _____________Escapement_____________", "\n");
output=c(output, "  TgtER     b Years     Min           Avg           Max", "\n");

Years=1:inputs$NYears

#Dynamically find the column width so formatting isn't off if numbers are big
cwid2=str_length(format(round(inputs$BSRb * ifelse(inputs$StepFunc=="POP",inputs$BufferEnd, 1)),nsmall=0))
cwid3=str_length(format(max(Years),nsmall=0))
cwid4=str_length(format(round(max(SummaryStats$MinEscpmnt),digits=0), nsmall=0))
cwid5=str_length(format(round(max(SummaryStats$AvgEscpmnt),digits=0), nsmall=0))
cwid6=str_length(format(round(max(SummaryStats$MaxEscpmnt),digits=0), nsmall=0))

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
  
# for(Year in 1:inputs$NYears){
#   output=c(output, format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6));
#   output=c(output, format(round(inputs$BSRb * PBff),nsmall=0,width=7));
#   output=c(output, format(Year,nsmall=0,width=7));
# #plus 2 because R indexing starts at 1
#   output=c(output, format(round(SummaryStats$MinEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
#   output=c(output, format(round(SummaryStats$AvgEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
#   output=c(output, format(round(SummaryStats$MaxEscpmnt[BufNum, Year],digits=0), nsmall=0,width=8));
#   output=c(output, "\n");  
# } #for loop for year
  
  #the col wid is the max of min width or max # width + 1 (+1 to leave a space if # is big)
  col1=format(round(inputs$TargetU[inputs$BaseRegime] * EBff,digits=2), nsmall=2,width=6)
  col2=format(round(inputs$BSRb * PBff),nsmall=0,width=max(7,cwid2+1))
  col3=format(Years,nsmall=0,width=max(7,cwid3+1))
  col4=format(round(SummaryStats$MinEscpmnt[BufNum, Years],digits=0), nsmall=0,width=max(8,cwid4+1))
  col5=format(round(SummaryStats$AvgEscpmnt[BufNum, Years],digits=0), nsmall=0,width=max(8,cwid5+1))
  col6=format(round(SummaryStats$MaxEscpmnt[BufNum, Years],digits=0), nsmall=0,width=max(8,cwid6+1))
  col7="\n"
  tmp=cbind(col1, col2, col3, col4, col5, col6, col7)
  output=c(output, as.vector(t(tmp)))
  
} #for loop for buffer
cat(file=file, output, sep="")
}
