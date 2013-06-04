# '*****  CompAgeCohort  *****
# Sub CompAgeCohort(TempCohort() As Double)
#     Dim Age As Integer      'counter for ages
# 
# For Age = MaxAge% To MinAge% + 1 Step -1
# Cohort(Age) = TempCohort(Age - 1)
# Next Age
# 
# 'recruitment I think
#     Cohort(MinAge%) = Cohort(MinAge% - 1)
# 
# End Sub

CompAgeCohort = function(TempCohort, Cohort, inputs){   
  #TempCohort is Cohort - mortality and fish that return to spawn
  #Cohort age t in next year is cohort age t-1 this year
#   for(Age in inputs$MaxAge:(inputs$MinAge + 1)){
#     Cohort[Age] = TempCohort[Age - 1]
#   }
  Cohort[inputs$MaxAge:(inputs$MinAge + 1)]=TempCohort[inputs$MaxAge:(inputs$MinAge + 1)-1]
  
  #This 'ages' recruitment in Cohort[1]
  #'EEH: this only works if MinAge=2, because Cohort[1] was set in CompRecruits (not Cohort[minage-1])
  #'EEH: why isn't TempCohort[inputs$MinAge - 1] = Cohort[inputs$MinAge - 1]?
  Cohort[inputs$MinAge] = Cohort[inputs$MinAge - 1]
  return(Cohort)
}