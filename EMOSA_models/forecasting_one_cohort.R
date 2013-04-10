# Prepare model parameters for the chosen cohort
dsPars<-dsPars[(dsPars$cohort %in% cohort),]    # subset the cohort work with
# Load parameter values into objects for easier model specification
Tgi<-dsPars$Tgi
Tga<-dsPars$Tga
Tig<-dsPars$Tig
Tag<-dsPars$Tag
Tia<-dsPars$Tia
Tai<-dsPars$Tai
specification<-dsPars$specification # collects what specification it is (Original, Scaled)
model<-dsPars$model                 # collects what model it is ( Diffusion, Contagion)
# dist<-dsPars$dist                 # collects what distribution it is (Gauss, Beta, Logit)
# collect observed prevalences
dsOBtimevars<- dsWS_catatrans[dsWS_catatrans$cohort %in% cohort,timevars] # select time variabels
dsOB <- dsWS_catatrans[dsWS_catatrans$cohort %in% cohort,obsvars] # selec cohort and responses

yearCount <- nrow(dsOB)    # Calculates the number of time points  1:11
groupCount <- ncol(dsOB)
#Establish a container to hold the predicted values.
pred <- matrix(NA_real_, ncol=groupCount, nrow=yearCount, dimnames=list(1:yearCount,obsvars))
ob<-as.matrix(dsOB) # the matrix of observed values
obOBtimevars<-as.matrix(dsOBtimevars)

# print(ob)
# print(pred)
# produce model prediction for selected Specification, Model and Cohort
pathModel <- file.path(getwd(),paste0(specification,model,"Gauss/",paste0(specification,model,".R")))
#  GitHub                            /SpecificationModelDistribution     /SpecificationModel.R
source(pathModel) 

