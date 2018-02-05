VRAP (Viability and Risk Assessment Procedure)
==============================================================

To use

* Online access to the model: [VRAP Shiny App](https://dataexplorer.northwestscience.fisheries.noaa.gov/nwc/VRAP/)
* Install and run from the R command line:
    library(devtools)
    install_github("eeholmes/VRAP")
    
# Introduction
The Viability and Risk Assessment Procedure (VRAP) model was adapted from the Risk Assessment
Procedure (RAP) original developed by Jim Scott (WDFW) and Bob Hayman (Skagit Coop) in the
1990’s. The original purpose of the model was to assess the impact of various harvest rates on salmon
populations based on the variance in returns and in management precision, including a harvest rate
scale based on size of the returns. The model was based on the assumption of density dependence
in recruits per spawner and introduced stochasticity in the estimates with variance estimates around
the fit of the data to the predicted spawner-recruit curve and in management error. The model has
since been expanded to assess the level of productivity and capacity (the two parameters of density
dependent spawner recruit functions) needed to sustain a viable population given a level of desired
harvest and the estimated variance in returns and management precision.

Population viability is a measure of survival based on abundance, productivity, and/or other
conditions that give a population a low risk of extinction over some defined period of time. The
population viability criteria used by NOAA in evaluating salmon recovery describe the number,
productivity, spatial structure and diversity required for a naturally self-sustaining population to have
either a 0.95 or 0.99 probability of persistence over a 100 year time period.

# The VRAP Model
The Viability and Risk Assessment Procedure (VRAP) model is a stochastic simulation model
originally written in Visual Basic and later converted to R. VRAP was developed to address the complex life history of Chinook salmon,
returning to spawn at multiple ages and being susceptible to fisheries in multiple years. The model may
be used to assess various harvest rates given a spawner-recruit function or to determine the parameters
of a spawner-recruit function that would satisfy the viability assumption given a desired harvest rate.

VRAP allows one to choose among the Hockey stick, Ricker, or Beverton-Holt spawner recruit
functions to determine the production from spawning escapement. Harvest schemes are then applied
to estimate progeny spawners based on cohort analysis. Uncertainty may be introduced at several
levels in the analysis. The variance of estimated recruits (based on observed data of escapement
and catch) around the spawner recruit curve is used to stochastically determine recruits. In addition,
patterns and random variability due to environmental conditions may be introduced by way of two
covariates to the analysis. Also, error in achieving a targeted harvest rate may also be introduced.

VRAP uses risk assessment analysis to look at viability status and/or effects of various harvest rates
on viability/rebuilding of salmon based on predicted returns from the number of spawners using density
dependent spawner-recruit functions and estimated error around the estimates.

# Two Types of VRAP Simulations

VRAP’s two modes of operation are:

* Harvest mode to determine RERs: given a spawner recruit function with fit parameters, what
is the highest level of harvest that still allows rebuilding of the population, given set risk
assumptions, and

* Population mode to determine viability abundance and productivity: given a fixed harvest rate,
what spawner recruit parameters (productivity and capacity) are needed to guarantee viability,
given set risk levels.

# For more information

See the VRAP User Guide in the doc folder of the package.
