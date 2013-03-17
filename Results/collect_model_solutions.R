# rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
library(reshape)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

pars12<- c("Tgi","Tga","Tig","Tag","Tia","Tai") # ,"Cgi","Cga","Cig","Cag","Cia","Cai")  # for Hybrid

#Model selection
spec1<-c("Original")
spec2<-c("Scaled")
distribution<-c("Gauss")
spec1name<-rep(spec1,5)
spec2name<-rep(spec2,5)

# input the Original Contagion model soution and attache observed prevalences
modelCo<-paste0(spec1,"Contagion",distribution)
dsModelCo<-read.csv(file.path(getwd(),modelCo,paste0(modelCo,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelCo$specification<-spec1name
dsModelCoPars<-dsModelCo[,c("specification","model","cohort",pars12)]
dsModelCo<-join(dsModelCo,dsOBSprops, by="cohort")

# input the Original Diffusion model soution and attache observed prevalences
modelDo<-paste0(spec1,"Diffusion",distribution)
dsModelDo<-read.csv(file.path(getwd(),modelDo,paste0(modelDo,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelDo$specification<-spec1name
dsModelDoPars<-dsModelDo[,c("specification", "model","cohort",pars12)]
dsModelDo<-join(dsModelDo,dsOBSprops, by="cohort")


# input the Scaled Contagion model soution and attache observed prevalences
modelCs<-paste0(spec2,"Contagion",distribution)
dsModelCs<-read.csv(file.path(getwd(),modelCs,paste0(modelCs,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelCs$specification<-spec2name
dsModelCsPars<-dsModelCs[,c("specification", "model","cohort",pars12)]
dsModelCs<-join(dsModelCs,dsOBSprops, by="cohort")


# input the Scaled Diffusion model soution and attache observed prevalences
modelDs<-paste0(spec2,"Diffusion",distribution)
dsModelDs<-read.csv(file.path(getwd(),modelDs,paste0(modelDs,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelDs$specification<-spec2name
dsModelDsPars<-dsModelDs[,c("specification", "model","cohort",pars12)]
dsModelDs<-join(dsModelDs,dsOBSprops, by="cohort")


# Combine model solutions into a single dataset
dsModelsPars<-rbind(dsModelCoPars,dsModelDoPars,dsModelCsPars,dsModelDsPars)
dsModels<-rbind(dsModelCo,dsModelDo,dsModelCs,dsModelDs) # stacked ds with models solutions (H,C,D)

# dsModels$specification<-factor(dsModelCo$specification,levels=c("Original","Scaled"))  
# dsModels$model<-factor(dsModelDo$model,levels=c("Original","Scaled")) 
# dsModelsPars$specification<-factor(dsModelCs$specification,levels=c("Original","Scaled")) 
# dsModelsPars$model<-factor(dsModelDs$model,levels=c("Original","Scaled")) 

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

