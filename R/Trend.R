#' @title Trend
#' @description Compute variable x with trend t for year y
#' @details
#' NJS: created 7/9/02. Original VB code:
#' Function Trend(t As Double, y, x As Double, z) As Double
#' t is trend rate, y is time increment, x is first value, z is type of trend
#' 
#' If z = 0 Then
#' Trend = x * (1 + t) ^ y
#' ElseIf z = 1 Then
#' Trend = x + (y * t)
#' Else
#' If Trend < 0 Then Trend = 0
#' 
#' Print "Unknown trend/cycle function"
#' Stop
#' End If
#' 
#' End Function
#' @param t trend rate
#' @param y time increment
#' @param x first value
#' @param z type of trend 0 or 1
#' @return trend variable (scalar or vector)
Trend = function(t, y, x, z){
  #' EEH: changed to work with vectors of y
  if(!(z == 0 | z==1)) stop("Unknown trend/cycle function")
  
  if(z == 0){
    val = x * (1 + t) ^ y
  }else{
    val = x + (y * t)
  }
  val[val < 0] = 0
  return(val)
}