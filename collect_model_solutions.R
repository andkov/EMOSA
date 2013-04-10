# rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
library(reshape)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

pars12<- c("Tgi","Tga","Tig","Tag","Tia","Tai") # ,"Cgi","Cga","Cig","Cag","Cia","Cai")  # for Hybrid

#Model selection
spec1<-c("GiA431") # choose what definition of the behavioral categories to use
spec2<-c("Scaled")
distribution<-c("Gauss")
spec1name<-rep(spec1,5)
spec2name<-rep(spec2,5)

# input the Original Contagion model soution and attache observed prevalences
OCont<-paste0(spec1,"Contagion",distribution)
dsOCont<-read.csv(file.path(getwd(),OCont,paste0(OCont,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsOCont$specification<-spec1name
dsOContPars<-dsOCont[,c("specification","model","cohort",pars12)]

# input the Original Diffusion model soution and attache observed prevalences
ODiff<-paste0(spec1,"Diffusion",distribution)
dsODiff<-read.csv(file.path(getwd(),ODiff,paste0(ODiff,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsODiff$specification<-spec1name
dsODiffPars<-dsODiff[,c("specification", "model","cohort",pars12)]

# input the Original Hybrid model soution and attache observed prevalences
OHyb<-paste0(spec1,"Hybrid",distribution)
dsOHyb<-read.csv(file.path(getwd(),OHyb,paste0(OHyb,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsOHyb$specification<-spec1name
dsOHybPars<-dsOHyb[,c("specification", "model","cohort",pars12)]

# Combine model solutions into a single dataset
dsModelsPars<-rbind(dsOContPars,dsODiffPars,dsOHybPars)
dsModels<-rbind(dsOCont,dsODiff,dsOHyb) # stacked ds with models solutions (H,C,D)

# dsModels$specification<-factor(dsOCont$specification,levels=c("Original","Scaled"))  
# dsModels$model<-factor(dsODiff$model,levels=c("Original","Scaled")) 
# dsModelsPars$specification<-factor(dsSCont$specification,levels=c("Original","Scaled")) 
# dsModelsPars$model<-factor(dsSDiff$model,levels=c("Original","Scaled")) 

# selection only model perfomance incides: mean difference (md), penalty (p), and DIC (DIC)
keepvars <- c("specification","model","cohort", "md","p","DIC")
dsDIC<-dsModels[,keepvars]

# Create LONG with DIC as the variable
dsDICLong <- reshape2::melt(dsDIC, id.vars=c("specification","model","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsDICLong <- plyr::rename(dsDICLong, replace=c(variable="index"))
dsDICLong <- dsDICLong[order(dsDICLong$index, dsDICLong$cohort, dsDICLong$model), ] #Sort for the sake of visual inspection.
dsDICLong$index<-factor(dsDICLong$index,)
# create a joint toggle variable modelindex for presenting all nine lines
# dsDICLong$modelindex<-paste0(dsDICLong$index,"-",dsDICLong$model)

# Create LONG with Parameter value as the variable
dsModelsParsLong <- reshape2::melt(dsModelsPars, id.vars=c("specification","model","cohort"))  ## id.vars declares MEASURED variables 
dsModelsParsLong <- plyr::rename(dsModelsParsLong, replace=c(variable="parameter"))

