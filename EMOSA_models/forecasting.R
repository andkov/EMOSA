#  Choose model for which to produce forecast
dsModel<-dsModelDoPars 

# Load observed prevalences (and/or transitions) for the chosen cohort
timevars<-c("cohort", "time", "age")    # select time variables 
obsvars<- c("pG","pgg", "pgi", "pga",  # select observed prevalences (and/or transitions)
            "pI", "pig", "pii", "pia",
            "pA" , "pag", "pai", "paa"
)   

dsPars<-dsModel 
cohort=c(1980)  #Select the cohort for this round of preditions
pathModel <- file.path(getwd(),"EMOSA_models/forecasting_one_cohort.R")
source(pathModel) 
(pred1980<- cbind(obOBtimevars,pred))

dsPars<-dsModel 
cohort=c(1981)  #Select the cohort for this round of preditions
pathModel <- file.path(getwd(),"EMOSA_models/forecasting_one_cohort.R")
source(pathModel) 
(pred1981<- cbind(obOBtimevars,pred))

dsPars<-dsModel 
cohort=c(1982)  #Select the cohort for this round of preditions
pathModel <- file.path(getwd(),"EMOSA_models/forecasting_one_cohort.R")
source(pathModel) 
(pred1982<- cbind(obOBtimevars,pred))

dsPars<-dsModel 
cohort=c(1983)  #Select the cohort for this round of preditions
pathModel <- file.path(getwd(),"EMOSA_models/forecasting_one_cohort.R")
source(pathModel) 
(pred1983<- cbind(obOBtimevars,pred))
 
 dsPars<-dsModel 
 cohort=c(1984)  #Select the cohort for this round of preditions
 pathModel <- file.path(getwd(),"EMOSA_models/forecasting_one_cohort.R")
 source(pathModel) 
(pred1984<- cbind(obOBtimevars,pred))
 
 pred<-rbind(pred1980,pred1981,pred1982,pred1983,pred1984)

# melt into LONG with prevalance as the outcome
dsPredLong <- reshape2::melt(data.frame(pred), id.vars=c("time", "cohort","age"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsPredLong<- plyr::rename(dsPredLong, replace=c(variable="catatrans", value="pred_proportion"))
# Create toggle variables for faceting in the 3x3 parameter matrix
dsPredLong$mx =      ifelse((dsPredLong$catatrans %in% c("pA","pI","pG")),substr(dsPredLong$catatrans,2,2),
                           toupper(substr(dsPredLong$catatrans,2,2)))
dsPredLong$my =      ifelse((dsPredLong$catatrans %in% c("pA","pI","pG")),substr(dsPredLong$catatrans,2,2),
                           toupper(substr(dsPredLong$catatrans,3,3)))
dsPredLong$mx<-factor(dsPredLong$mx,levels=c("G","I","A"))
dsPredLong$my<-factor(dsPredLong$my,levels=c("G","I","A"))

# create an interaction variable catatran*time
dsPredLong<-mutate(dsPredLong,catatransT=paste0(catatrans,substr(time,3,4)))
# order $catatrans before casting
dsPredLong$catatrans<-factor(dsPredLong$catatrans,levels=c("cohort", "time", "age",
                                                         "pG","pgg", "pgi", "pga",
                                                         "pI", "pig", "pii", "pia",
                                                         "pA", "pag", "pai", "paa"))

