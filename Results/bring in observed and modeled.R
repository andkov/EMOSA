# rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
library(reshape)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

pars12<- c("Tgi","Tga","Tig","Tag","Tia","Tai") # ,"Cgi","Cga","Cig","Cag","Cia","Cai")  # for Hybrid

#Model selection
specification<-c("Original")
distribution<-c("Gauss")

modelC<-paste0(specification,"Contagion",distribution)
modelD<-paste0(specification,"Diffusion",distribution)

# input the model soution
# reads in the result of a model run: Hybrid, Contagion, or Diffusion for 5 cohorts
dsModelC<-read.csv(file.path(pathDirectory,modelC,paste0(modelC,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelD<-read.csv(file.path(pathDirectory,modelD,paste0(modelD,"_resultsIn",".csv")),stringsAsFactors=FALSE)

# select only the parameter values
dsModelCPars<-dsModelC[,c("model","cohort",pars12)]
dsModelDPars<-dsModelD[,c("model","cohort",pars12)]

# attaches observed prevalences to the model solution

dsModelC<-join(dsModelC,dsOBSprops, by="cohort")
dsModelD<-join(dsModelD,dsOBSprops, by="cohort")

dsModelsPars<-rbind(dsModelHPars,dsModelCPars,dsModelDPars)
dsModels<-rbind(dsModelC,dsModelD) # stacked ds with models solutions (H,C,D)


# selection only model perfomance incides: mean difference (md), penalty (p), and DIC (DIC)
keepvars <- c("model","cohort", "md","p","DIC")
dsDIC<-dsModels[,keepvars]

# Create LONG with DIC as the variable
dsLongDIC <- reshape2::melt(dsDIC, id.vars=c("model","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsLongDIC <- plyr::rename(dsLongDIC, replace=c(variable="index"))
dsLongDIC <- dsLongDIC[order(dsLongDIC$index, dsLongDIC$cohort, dsLongDIC$model), ] #Sort for the sake of visual inspection.
dsLongDIC$index<-factor(dsLongDIC$index,)
# create a joint toggle variable modelindex for presenting all nine lines
dsLongDIC$modelindex<-paste0(dsLongDIC$index,"-",dsLongDIC$model)

# Create LONG with Parameter value as the variable
dsModelsParsLong <- reshape2::melt(dsModelsPars, id.vars=c("model","cohort"))  ## id.vars declares MEASURED variables 
dsModelsParsLong <- plyr::rename(dsModelsParsLong, replace=c(variable="parameter"))
# center cohort value at 1980
dsModelsParsLong <-mutate(dsModelsParsLong,cohort=cohort-1980)


# Plots
modelcolors<-c("black","goldenrod2","royalblue2")

# DIC plot
# 
p<-ggplot(dsDIC, aes(x=cohort,y=DIC,group=model))+
  geom_line( aes( colour = model),size=1,guide=FALSE)+ 
  geom_point(aes(colour = model ),size=4)+
  scale_color_manual(values = modelcolors)+
  ylim(c(-100,-40))+
  labs(title=paste0(specification," specification"))
p
# plast<-p
# pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_DIC.png")) 
# png (filename = pathFileOut ,
#      width = 600, height = 800 , units = "px")
# plot(plast)
# dev.off()


# Parameter Solution plot 1x12 
#
str(dsModelsParsLong)
  p<-ggplot(dsModelsParsLong, aes(x=cohort,colour=model))+
  geom_point(aes(y=value))+
  facet_grid(.~parameter)+
  scale_color_manual(values = modelcolors, name=specification)+
  geom_line(aes(y=value))+
  scale_y_continuous("value of parameter",
                   limits=c(0, 1),
                   breaks=c(.2,.4,.6,.8,1))+
  geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)
p
# plast<-p
# pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_Solution.png")) 
# png (filename = pathFileOut ,
#      width = 1600, height = 200 , units = "px")
# plot(plast)
# dev.off()

# proportion by 5 cohorts
dsFORp <- subset(dsPrevsLong, cohort %in% allCohorts)
p<-ggplot(dsFORp, aes(x=time,y=proportion,group=catatrans,fill=factor(catatrans)))+
  scale_color_manual(values = c("pG"="green2","pI"="goldenrod2","pA"="red3"))+
  geom_line(aes(colour = catatrans))+ facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show_guide = FALSE)+
  labs(title=paste0("Observed by predicted for each cohort"))
p



# Creates predictions based on observed prevalence and model solution

# A<-as.matrix(dsLong)
# dsModel<-mutate(dsModel, mpG00=pG00,mpI00=pI00, mpA00=pA00,
#                   mpG01 = pG00 - Tgi*pG00*(pI00^Cgi) - Tga*pG00*(pA00^Cga)
#                   + Tig*pI00*(pG00^Cig) + Tag*pA00*(pG00^Cag),
#                   mpI01 = pI00 - Tia*pI00*(pA00^Cia) - Tig*pI00*(pG00^Cig)
#                   + Tgi*pG00*(pI00^Cgi) + Tai*pA00*(pI00^Cai),
#                   mpA01 = 1- mpG01 -mpI01 
# 
#   )
# 
# # create a long file with Modeled prevalence
# dsModLong<-dsModel[,c("cohort",modeled)]
# dsModLong <- reshape2::melt(dsModLong, id.vars=c("cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
# dsModLong$cattrans<-substr(dsModLong$variable,1,3)
# dsModLong$variable <- gsub(pattern="mpA", replacement="", x=dsModLong$variable) 
# dsModLong$variable <- gsub(pattern="mpI", replacement="", x=dsModLong$variable) 
# dsModLong$variable <- gsub(pattern="mpG", replacement="", x=dsModLong$variable) 
# dsModLong$variable <- as.integer(dsModLong$variable) #Convert to a number.
# dsModLong <- plyr::rename(dsModLong, replace=c(variable="time", value="prevalence"))
#  
# 
# dsLong<-rbind(dsObsLong,dsModLong)






