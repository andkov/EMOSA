rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
library(reshape)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.


#Model selection
specification<-c("Alternative")
distribution<-c("Gauss")
num<-c("Plus 2")
pars12<- c("Tgi","Tga","Tig","Tag","Tia","Tai",
           "Cgi","Cga","Cig","Cag","Cia","Cai")	

modelH<-paste0(specification,"Hybrid",num)
modelC<-paste0(specification,"Contagion",distribution)
modelD<-paste0(specification,"Diffusion",distribution)


#links
# links for Aggregate
pathDirectory <- file.path(getwd())
pathObserved <- file.path(pathDirectory,"Data")
pathPrevTrans <-file.path(pathObserved,"prevalences_transitions.csv")

# data in
dsObs<-read.csv(pathPrevTrans,stringsAsFactors=FALSE)

# Defining elements to use
# Vectors
allCohorts<-c("1980","1981","1982","1983","1984" )
cattransPrev<-c("G","I","A")
cattransTrans<-c("gg","gi","ga","ig","ii","ia","ag","ai","aa" )
proportions<-c("cohort","cattrans","pt00","pt01","pt02","pt03","pt04","pt05","pt06","pt07","pt08")
counts     <-c("cohort","cattrans","t00" , "t01", "t02", "t03", "t04", "t05", "t06", "t07", "t08")

modeled<-c("mpA00","mpA01","mpA02","mpA03","mpA04","mpA05","mpA06","mpA07","mpA08",
           "mpI00","mpI01","mpI02","mpI03","mpI04","mpI05","mpI06","mpI07","mpI08",
           "mpG00","mpG01","mpG02","mpG03","mpG04","mpG05","mpG06","mpG07","mpG08"
)
# Palettes
attcol8<-c("blue3","blue","lightblue", "snow", "lightpink", "red" ,"red3", "red4")
attcol3<-c("blue2","blue2","snow", "snow", "snow", "red2" ,"red2", "red2")
prevcol3<-c("blue2","snow","red2")

# Labels 
categories8<-c("Never","Once or twice","Less than once a month/ 3-12 times","About once a month/ 12 times",
               "About tiwce a month/ 24 times", "About once a week", "Several times a week","Everyday")
categories8s<-c("Never","1-2/week","3-12/week","~1/month",
                    "~2/month", "~1/week", "2-3/week","Everyday ")
categories3<-c("Goers","Irregulars","Absentees")
categories38<-c("Absentees","Absentees","Irregulars","Irregulars","Irregulars","Goers","Goers","Goers")


#select chose prevalences(cattransPrev)   | counts(counts)
#             transitions(cattransTrans)  | proportions(proportions) 
dsObs <- dsObs[dsObs$cattrans %in% cattransPrev,proportions]

# input the observed prevalence from the agregated source ?should be replaced by live data
dsObsLong <- reshape2::melt(dsObs, id.vars=c("cattrans","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
# assign the variable name of the category
dsObsLong$variable <- gsub(pattern="pt", replacement="", x=dsObsLong$variable)
dsObsLong <- plyr::rename(dsObsLong, replace=c(variable="time", value="prevalence"))
dsObsLong$cattrans<-paste0("p",dsObsLong$cattrans,dsObsLong$time)
dsObsLong$time <- as.integer(dsObsLong$time) #Convert to a number.
dsObsWide<-dcast(dsObsLong, cohort ~ cattrans, mean)
 

# input the model soution
# reads in the result of a model run: Hybrid, Contagion, or Diffusion for 5 cohorts
dsModelH<-read.csv(file.path(pathDirectory,modelH,paste0(modelH,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelC<-read.csv(file.path(pathDirectory,modelC,paste0(modelC,"_resultsIn",".csv")),stringsAsFactors=FALSE)
dsModelD<-read.csv(file.path(pathDirectory,modelD,paste0(modelD,"_resultsIn",".csv")),stringsAsFactors=FALSE)

# select only the parameter values
dsModelHPars<-dsModelH[,c("model","cohort",pars12)]
dsModelCPars<-dsModelC[,c("model","cohort",pars12)]
dsModelDPars<-dsModelD[,c("model","cohort",pars12)]

# attaches observed prevalences to the model solution
dsModelH<-join(dsModelH,dsObsWide, by="cohort")
dsModelC<-join(dsModelC,dsObsWide, by="cohort")
dsModelD<-join(dsModelD,dsObsWide, by="cohort")

dsModelsPars<-rbind(dsModelHPars,dsModelCPars,dsModelDPars)
dsModels<-rbind(dsModelH,dsModelC,dsModelD) # stacked ds with models solutions (H,C,D)


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
plast<-p
pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_DIC_",num,".png")) 
png (filename = pathFileOut ,
     width = 600, height = 800 , units = "px")
plot(plast)
dev.off()


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
plast<-p
pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_Solution_",num,".png")) 
png (filename = pathFileOut ,
     width = 1600, height = 200 , units = "px")
plot(plast)
dev.off()


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






