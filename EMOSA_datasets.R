rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
# update.packages("ask=F")
require(colorspace)
require(lme4)
require(Matrix)
require(lattice)
require(car)
require(MASS)
require(nnet)
require(ggplot2)
require(plyr)
require(reshape2)
require(colorspace) 
#######
# NOTE: you must have  "NLSY-97_Religiosity" repository in your local GitHub folder 
# in order to execute (1) and (2)
pathGitHub <- file.path("C:/Users/kovalav/Documents/GitHub") # locate the "GitHub" folder on your computer
#######


# (1) requires "NLSY-97_Religiosity" repository
# Read the source file from NLS Web Investigator and prepare the parent dataset "dsSource"
pathReadClean <- file.path(pathGitHub, "NLSY-97_Religiosity/01_reading_and_cleaning.R")
# C:/Users/USERNAME/Documents/GitHub  /Repository        /NN_Script.R 
source(pathReadClean)
rm(list=setdiff(ls(),(c("pathGitHub","dsSource")))) # Clean temp elements

# (2) requires "NLSY-97_Religiosity" repository
# breack up dsSource into smaller datasets, transform them for modeling and graphing
pathMeltCast <- file.path(pathGitHub,"NLSY-97_Religiosity/02_melting_and_casting.R")
# C:/Users/USERNAME/Documents/GitHub  /Repository       /NN_Script.R 
source(pathMeltCast)

# Collect model solutions from folders in EMOSA repository
pathCollectSolutions <- file.path(getwd(),"collect_model_solutions.R")
# GitHub/EMOSA                            /Script.R 
source(pathCollectSolutions)

# Choose dataset(s) to keep 
rm(list=setdiff(ls(),c(
  "dsSource",         # Clean and processed NLSY file from the source(08032013)
  "dsW_attend",       # Wide timeseris, subset(dsSource) vars:  original responses to the question (1-8)
  "dsL_attend",       # Long time series, elongated dsW_attend
  "dsW_catatrans",    # Wide timeseries: subset(dsSource)
  "dsL_catatrans",    # Long timeseries, elongated dsW_catatrans
  "dsWS_catatrans",   # Wide summaries of catatrans, both counts and proportions, summaried dsL_catatrans
  "dsWSP_catatrans",  # Wide summaries of catatrans as proportions, subset(dsWS_catatrans)
  "dsLSP_catatrans",  # Long summaries of catatrans as proportion, elongated dsWSP_catatrans
  "dsWSC_catatrans",  # Wide summaries of catatrans as counts, subset(dsWS_catatrans)
  "dsLSC_catatrans",  # Long summaries of catatrans as counts, elongated dsWSC_catatrans
  "dsOCont",          # Original Contagion complete model values: parameters, fit, precision, etc.
  "dsOContPars",      # Original Contagion parameters 
  "dsODiff",          # Original Diffusion complete model values: parameters, fit, precision, etc.
  "dsODiffPars",      # Original Diffusion parameters   
  "dsOHyb",           # Original Hybrid complete model values: parameters, fit, precision, etc.
  "dsOHybPars",       # Original Hybrid parameters 
  "dsModels",         # All tested models: complet model values:parameters, fit, precision, etc.
  "dsModelsPars",     # All tested models: parameters
  "dsModelsParsLong", # all tested models: parameters - LONG FORM 
  "dsDIC",            # Mean difference, penalty, and DIC of all models
  "dsDICLong"         # eLONGated dsDIC for easier ggplotting
)))



# write.table(dsWSP_catatrans,file="dsWSP_catatrans.csv",sep=",",row.names=F)






