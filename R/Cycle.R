#' @title Cycle
#' @description Compute cyclic variable, a = amplitude, p = period, s = start, y = year, x = mean value of variable
#' @details
#' NJS: created 7/9/02 corrected 9/16/03
#' 
#' Function Cycle(a As Double, p As Double, s As Double, y, x As Double) As Double
#' a is amplitude, p is period, s is starting point, y time period
#' what is x doing here? It is average value and is not needed here.
#'     Dim cy As Double
#'     cy = Sin(2# * 3.141592654 * (y + s - 1) / p)
#'     'in good survival, cycle ranges from 1 to a (amplitude)
#' in bad survival, cycle ranges from 1/a to 1 (this might be lower than expected)
#'     If cy >= 0 Then
#'         cy = (cy * (a - 1)) + 1
#'     Else
#'         cy = (cy * (1 - (1 / a))) + 1
#'     End If
#'    Cycle = cy + x    ' use if x is changed to scalar
#'     Cycle = cy
#'         
#' End Function
#' @param a amplitude
#' @param p period
#' @param s starting point
#' @param y time period
#' @return cyclic variable (scalar or vector)
Cycle = function(a, p, s, y){
  # EEH: changed to allow vectors of y
  cy = sin(2 * pi * (y + s - 1) / p)
  # 'in good survival, cycle ranges from 1 to a (amplitude)
  # 'in bad survival, cycle ranges from 1/a to 1 (this might be lower than expected)
  newcy = cy
  newcy[cy >= 0]= (cy[cy >= 0] * (a - 1)) + 1
  newcy[cy < 0] = (cy[cy < 0] * (1 - (1 / a))) + 1
  return(newcy)        
}