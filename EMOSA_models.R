###  executes  "EMOSA_datasets.R"   ###
pathEMOSA_datasets <- file.path(getwd(),"EMOSA_datasets.R")
# C:/Users/USERNAME/Documents/GitHub  /Repository       /NN_Script.R 
source(pathEMOSA_datasets)

# Create lists of variables for later subsetting and sorting
timevars<-c("cohort", "time", "age")   # time variables
obsvars<- c("pG","pgg", "pgi", "pga",  # prevalences of Goers-Irregulars-Absentees
            "pI", "pig", "pii", "pia", # Goers-Irregulars-Absentees
            "pA" , "pag", "pai", "paa" # and transition among them betwen 2000-2010
)   
# Choose dataset(s) to keep 
keepds<-c("keepds","timevars","obsvars", "dsModel",
#   "dsSource",         # Clean and processed NLSY file from the source(08032013)
#   "dsW_attend",       # Wide timeseris, subset(dsSource) vars:  original responses to the question (1-8)
#   "dsL_attend",       # Long time series, elongated dsW_attend
#   "dsW_catatrans",    # Wide timeseries: subset(dsSource)
#   "dsL_catatrans",    # Long timeseries, elongated dsW_catatrans
  "dsWS_catatrans",   # Wide summaries of catatrans, both counts and proportions, summaried dsL_catatrans
  "dsWSP_catatrans",  # Wide summaries of catatrans as proportions, subset(dsWS_catatrans)
  "dsLSP_catatrans",  # (!)Long summaries of catatrans as proportion, elongated dsWSP_catatrans
#   "dsWSC_catatrans",  # Wide summaries of catatrans as counts, subset(dsWS_catatrans)
#   "dsLSC_catatrans",  # Long summaries of catatrans as counts, elongated dsWSC_catatrans
#   "dsOCont",          # Original Contagion complete model values: parameters, fit, precision, etc.
  "dsOContPars",      # Original Contagion parameters 
#   "dsODiff",          # Original Diffusion complete model values: parameters, fit, precision, etc.
  "dsODiffPars",      # Original Diffusion parameters 
#   "dsSCont",          # Scaled Contagion complete model values: parameters, fit, precision, etc.
  "dsSContPars",      # Scaled Contagion parameters 
#   "dsSDiff",          # Scaled Diffusion complete model values: parameters, fit, precision, etc.
  "dsSDiffPars",      # Scaled Diffusion parameters 
#   "dsModels",         # All tested models: complet model values:parameters, fit, precision, etc.
  "dsModelsPars",     # All tested models: parameters
  "dsModelsParsLong", # all tested models: parameters - LONG FORM 
#   "dsDIC",            # Mean difference, penalty, and DIC of all models
  "dsDICLong"         # eLONGated dsDIC for easier ggplotting
)
rm(list=setdiff(ls(),keepds))



# Original DIffusion ###############################################################
dsModel<-dsODiffPars
print(dsModel)
# Compute model forecast using observed values, specification, and the model solution
pathModel <- file.path(getwd(),"EMOSA_models/forecasting.R")
source(pathModel)

# Original Contagion ###############################################################
dsModel<-dsOContPars
print(dsModel)
# Compute model forecast using observed values, specification, and the model solution
pathModel <- file.path(getwd(),"EMOSA_models/forecasting.R")
source(pathModel)

# Scaled DIffusion ###############################################################
dsModel<-dsSDiffPars
print(dsModel)
# Compute model forecast using observed values, specification, and the model solution
pathModel <- file.path(getwd(),"EMOSA_models/forecasting.R")
source(pathModel)

# Scaled Contagion ###############################################################
dsModel<-dsSContPars
print(dsModel)
# Compute model forecast using observed values, specification, and the model solution
pathModel <- file.path(getwd(),"EMOSA_models/forecasting.R")
source(pathModel)

dsEMOSA<-join(dsLSP_catatrans,pred_OD)
dsEMOSA<-join(dsEMOSA,pred_OC)
dsEMOSA<-join(dsEMOSA,pred_SD)
dsEMOSA<-join(dsEMOSA,pred_SC)


dsEMOSA<-mutate(dsEMOSA,sqdif_OD=(obs_proportion-OD_proportion)^2,
                    sqdif_OC=(obs_proportion-OC_proportion)^2,
                    sqdif_SD=(obs_proportion-SD_proportion)^2,
                    sqdif_SC=(obs_proportion-SC_proportion)^2)

fit<-dcast(dsEMOSA, cohort ~ time,value.var="sqdif_OD", sum)



?dcast



