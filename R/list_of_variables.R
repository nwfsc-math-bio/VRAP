#inputs from .rav file or directed calculated from .rav file inputs
# AveEnv
# BaseRegime
# BetaMarA
# BetaMarB
# BSRa
# BSRb
# BSRc
# BSRd
# BufferEnd
# BufMax 
# BufferStart
# BufferStep
# CohortStart[Age]
# ConvergeCrit
# CenterCov; was Debugg
# CorrMS; in rav, not used in code
# depen
# DL1
# DL2
# DR
# ECrit
# ERecovery
# EndAv
# EscChoice
# EscpmntBreakPoint[BreakPoint]
# FlowAve
# FlowCV
# FlowSD
# GammaMgmtA
# GammaMgmtB
# GammaMarA
# GammaMarB
# GammaFlowA
# GammaFlowB
# MarAve
# MarCV
# MarSD
# MarSurv
# MatRate[Age]
# MatU[Age]
# MgmtError
# MaxAge
# MinAge
# NatMort[Age]
# NRuns
# NumBreakPoints
# NYears 
# PTU[Age]  
# RanSeed
# ResCorParameter
# SRErrorA
# SRErrorB
# SRType
# StepFunc
# SurvScale
# TargetU[BreakPoint]
# TrndCycF
# TCF1
# TCF2
# TCF3
# TrndCycM
# TCM1
# TCM2
# TCM3
# Title
  
#computed variables staticvars
# AEQ[Age] #AEQcalc
# BufTargetU[BreakPoint] #BufferInit
# MxR #BufferInit
# RecruitsAtAge1 #Recruits
# BufSRb #BufferInit; changed the name

#Year Statistics YearStats
# first defined in CompEscpmnt
# AEQMort
# Escpmnt
# PTMort
# MatMort
# MatRun
# TotAdultEscpmnt
# TotAEQMort
# TotEscpmnt

#Summary Statistics SummaryStats
# first defined in Main and update in CompRecruits
#   Dim AveRanFlow[Buf]
#   Dim AveRanMarine[Buf]
#   Dim AvgAEQMort[Buf]
#   Dim AvgBYrHR[Buf,Byr]
#   Dim AvgCaHR[Buf]
#   Dim AvgECrit[Buf]
#   Dim AvgEscpmnt[Buf,Year]
#   Dim BufAvgBYrHR[Buf]
#   Dim MaxBYrHR[Buf,Byr]
#   Dim MaxEscpmnt[Buf,Year]
#   Dim MinBYrHR[Buf]
#   Dim MinEscpmnt[Buf]
#   Dim PropRec[Buf]
#   Dim PropExt[Buf]
# FirstRanFlow #Main
# FirstRanMarine #Main

#repetition vars; things updated each year; repvars
# Cohort #first defined in RepInit and updated in CompNatMort and again in CompAgeCohort
# TempCohort
# LastRanError #Recruits
# LastRanFlow #Recruits
# LastRanMarine #Recruits
