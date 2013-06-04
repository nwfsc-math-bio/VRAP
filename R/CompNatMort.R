
# '*****  CompNatMort  *****
# 'Let the number of fish in each age class decrease according to
# 'the natural mortality in that age class.
# Sub CompNatMort()
#     Dim Age%     'counter for ages
# 
# For Age% = MinAge% - 1 To MaxAge%
# Cohort(Age%) = Cohort(Age%) * (1 - NatMort(Age%))
# Next Age%
# 
# End Sub


CompNatMort = function(inputs, CohortBeforeNatMort){
  #updates global array Cohort
#   CohortAfterNatMort = CohortBeforeNatMort 
#   for(Age in (inputs$MinAge - 1):inputs$MaxAge){
#     CohortAfterNatMort[Age] = CohortBeforeNatMort[Age] * (1 - inputs$NatMort[Age])
#   }
  CohortAfterNatMort = CohortBeforeNatMort * (1 - inputs$NatMort)
  return(CohortAfterNatMort)
}