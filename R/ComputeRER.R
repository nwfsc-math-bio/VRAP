#' @title ComputeRER
#' @description Computes the LEL and UEL RERs=
#' @details This fits a spline to the harvest versus UET and LET lines to get the ER at the LEL and UEL.
#' @param VRAPList output list from Main() or RunSims()
#' @param UEL Upper target as percent of VRAP simulations that should be above recovery threshold
#' @param LEL lower target as percent of VRAP simulations that should be not hit the lower threshold
#' @return a list with:
#' \describe{
#'  \item{ERUEL}{The ER at the UEL}
#'  \item{ERLEL}{The ER at the LEL}
#'  \item{hr.vrap}{The harvest rates for each simulation in the VRAP output.}
#'  \item{hr.smooth}{The harvest rates used to computed the smoothed UEL and LEL.}
#'  \item{uel.vrap}{The UEL for each simulation in the VRAP output.}
#'  \item{uel.smooth}{The smoothed UEL.}
#'  \item{lel.vrap}{The LEL for each simulation in the VRAP output.}
#'  \item{lel.smooth}{The smoothed LEL.}
#'  \item{UEL}{The proportion to use for the upper threshold.}
#'  \item{LEL}{The proportion to use for the lower threshold.}
#' }
ComputeRER=function(VRAPList, UEL=80, LEL=5){
  EBff = VRAPList$inputs$BufferStart + (0:(VRAPList$inputs$BufMax-1))*VRAPList$inputs$BufferStep
  hr.vrap = VRAPList$input$TargetU*EBff
  #LEL is the fraction that hit the lower critical threshold
  lel.vrap = VRAPList$SummaryStats$AvgECrit[1:VRAPList$inputs$BufMax]
  #UEL is the fraction that hit the upper recovery threshold
  uel.vrap = VRAPList$SummaryStats$PropRec[1:VRAPList$inputs$BufMax]

  #use some smoothing
  x=hr.vrap
  y=uel.vrap
  smoothingSpline = smooth.spline(x, y, spar=0.35)
  pred=predict(smoothingSpline,seq(0,1,.01))
  ers=pred$x
  uel=pred$y #smoothed
  
  y=lel.vrap
  smoothingSpline = smooth.spline(x, y, spar=0.35)
  pred=predict(smoothingSpline,seq(0,1,.01))
  lel=pred$y #smoothed
  
  tmp=which(uel>=UEL/100)
  if(length(tmp)==0){ ERUEL=ers[1]
  }else{ ERUEL=ers[max(tmp)] } #greatest ER where Esc > UEL
  tmp=which(lel<=LEL/100)
  if(length(tmp)==0){ ERLEL=ers[1]
  }else{ ERLEL=ers[max(tmp)] }
  
  return(list(ERUEL=ERUEL, ERLEL=ERLEL, 
              hr.vrap=hr.vrap, hr.smooth=ers,
              uel.vrap=uel.vrap, uel.smooth=uel,
              lel.vrap=lel.vrap, lel.smooth=lel,
              UEL=UEL, LEL=LEL))
}