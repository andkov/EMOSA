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
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
# NOTE: you must have  "NLSY-97_Religiosity" repository in your local GitHub folder


####### ------------------- Read, clean, melt, cast, collect model solutions -----------------begin--------------
pathEMOSA_datasets <- file.path(getwd(),"EMOSA_datasets.R")
source(pathEMOSA_datasets) # execute "EMOSA_datasets.R" code
ls()
# Choose dataset(s) to keep 
# rm(list=setdiff(ls(),c("dsSource", #  original  NLSY97 datasource
#                        "dsSWprops", # observed prevalences and transitions S-summarized, W-Wide, props- proportions
#                        "dsSLprops", # elongated with time as ID and prevalence as RESPONS 
#                        "dsSWcatatrans", # both counts and proportions - for both prevalences and transitions
#                        "dsModelCoPars", # ds with Contagion original Parameters
#                        "dsModelDoPars", # ds with Contagion original Parameters
#                        "dsModelCsPars", # ds with Contagion scaled Parameters
#                        "dsModelDsPars", # ds with Contagion scaled Parameters
#                        "dsDIC"       # mean difference, penalty, and DIC of 4 models
# )))
####### ------------------- Read, clean, melt, cast, collect model solutions ----------------- end-_-------------

#  Choose model for which to produce forecast
dsModel<-dsModelDoPars 

# Load observed prevalences (and/or transitions) for the chosen cohort
timevars<-c("cohort", "time", "age")    # select time variables 
obsvars<- c("pG","pgg", "pgi", "pga",  # select observed prevalences (and/or transitions)
            "pI", "pig", "pii", "pia",
            "pA" , "pag", "pai", "paa"
)   

# Compute model forecast using observed values and the model solution
pathModel <- file.path(getwd(),"EMOSA_models/forecasting.R")
source(pathModel) 
head(dsPredLong)
modelDo<-join(dsSLprops,dsPredLong)


# 

