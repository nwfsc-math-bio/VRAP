# '*****   CompStockStatus   *****
# Sub CompStockStatus(TotAdultEscpmnt As Double, Status%)
#     Dim Break%
#     Dim EscpmntCheck$
#     
#     Break% = 1
#     EscpmntCheck$ = "True"
#     While Break% <= NumBreakPoints% And EscpmntCheck$ = "True"
#         If TotAdultEscpmnt > EscpmntBreakPoint(Break%) Then
#             EscpmntCheck$ = "True"
#             Break% = Break% + 1
#         Else
#             EscpmntCheck$ = "False"
#         End If
#     Wend
# 
#     Status% = Break%
# 
# End Sub

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