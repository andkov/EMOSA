
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
dsPredLong<- plyr::rename(dsPredLong, replace=c(variable="catatrans", 
                                                value=paste0(substr(specification,1,1),
                                                             substr(model,1,1),
                                                             "_proportion")))

# saves the forecast for this model in a individual dataset "pred_SM"
pred_SM<-paste0("pred_", substr(specification,1,1),substr(model,1,1))
assign(pred_SM, dsPredLong)
rm(list=setdiff(ls(),c(keepds,"pred_OD","pred_OC","pred_OH")))



