#' @title CompStockStatus
#' @description Compute stock status: is TotAdultEscpmnt > EscpmntBreakPoint[Break]
#' @details Original VB code
#'  Sub CompStockStatus(TotAdultEscpmnt As Double, Status%)
#'     Dim Break%
#'     Dim EscpmntCheck$
#'     
#'     Break% = 1
#'     EscpmntCheck$ = "True"
#'     While Break% <= NumBreakPoints% And EscpmntCheck$ = "True"
#'         If TotAdultEscpmnt > EscpmntBreakPoint(Break%) Then
#'             EscpmntCheck$ = "True"
#'             Break% = Break% + 1
#'         Else
#'             EscpmntCheck$ = "False"
#'         End If
#'     Wend
#' 
#'     Status% = Break%
#' 
#' End Sub
#' @param TotAdultEscpmnt Total adult escapement
#' @param inputs Inputs from .rav file
#' @return Status ("True"/"False")
CompStockStatus = function(TotAdultEscpmnt, inputs){  
  Break = 1
  EscpmntCheck = "True"
  while(Break <= inputs$NumBreakPoints & EscpmntCheck == "True"){
    if(TotAdultEscpmnt > inputs$EscpmntBreakPoint[Break]){
      EscpmntCheck = "True"
      Break = Break + 1
    }else{
      EscpmntCheck = "False"
    }
  }
  
  Status = Break
  return(Status)
}