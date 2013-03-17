rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

specification<-c("New")
distribution<-c("Gauss")

modelH<-paste0(specification,"Hybrid",distribution)
modelC<-paste0(specification,"Contagion",distribution)
modelD<-paste0(specification,"Diffusion",distribution)

#Dataprep
pathDirectory <- file.path(getwd())
dsModelH<-read.csv(file.path(pathDirectory,modelH,paste0(modelH,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelC<-read.csv(file.path(pathDirectory,modelC,paste0(modelC,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelD<-read.csv(file.path(pathDirectory,modelD,paste0(modelD,"_resultsIn",".csv")),stringsAsFactors=FALSE)

dsResults<-rbind(dsModelH,dsModelC,dsModelD)


myvars <- c("model","cohort", "DIC","md","p")
originalDIC <- dsResults[,myvars]
str(originalDIC)

#Transform the wide dataset into a long dataset
dsLongDIC <- reshape2::melt(originalDIC, id.vars=c("model","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsLongDIC <- plyr::rename(dsLongDIC, replace=c(variable="index"))
dsLongDIC <- dsLongDIC[order(dsLongDIC$index, dsLongDIC$cohort, dsLongDIC$model), ] #Sort for the sake of visual inspection.

dsLongDIC$index<-factor(dsLongDIC$index,)
dsLongDIC$modelindex<-paste0(dsLongDIC$index,"-",dsLongDIC$model)

str(dsLongDIC)

modelcolors<-c("black","goldenrod2","royalblue2")

# DIC plot

p<-ggplot(dsResults, aes(x=cohort,y=DIC,group=model))+
  geom_line( aes( colour = model),size=1,guide=FALSE)+ 
  geom_point(aes(colour = model ),size=4)+
  scale_color_manual(values = modelcolors)+
  ylim(c(-100,-40))+
  labs(title=paste0(specification," specification"))
p
plast<-p

pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_DIC.png")) 
png (filename = pathFileOut ,
     width = 600, height = 800 , units = "px")
plot(plast)
dev.off()











