# '*****  FUNCTION Autocorrel  *****
# 'Compute autocorrelated variable, p = autocorrelation, lastx = last value of variable x
# 'NJS: created 7/9/02
# Function Autocorrel(p As Double, lastx As Double, x As Double) As Double
#         
#     Autocorrel = p * lastx + (1 - p) * x
#     
# End Function
# 

Autocorrel = function(p, lastx, x){        
  val = p * lastx + (1 - p) * x;
  return(val)
}

