rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
library(reshape)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

#Model selection
specification<-c("original")
model<-"Hybrid"
distribution<-c("Gauss")

pars12<- c("Tgi","Tga","Tig","Tag","Tia","Tai",
           "Cgi","Cga","Cig","Cag","Cia","Cai")	

cohortYear <- 1984 #1980, 1981, 1982, 1983, 1984
modelname<-paste0(specification,model,distribution)
modelH<-paste0(specification,"Hybrid",distribution)
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
propAnames<-c("cohort","cattrans","Apt00","Apt01","Apt02","Apt03","Apt04","Apt05","Apt06","Apt07","Apt08")
propInames<-c("cohort","cattrans","Ipt00","Ipt01","Ipt02","Ipt03","Ipt04","Ipt05","Ipt06","Ipt07","Ipt08")
propGnames<-c("cohort","cattrans","Gpt00","Gpt01","Gpt02","Gpt03","Gpt04","Gpt05","Gpt06","Gpt07","Gpt08")
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

# dsObs Into dsObsLong - Observed prevalence
dsObsLong <- reshape2::melt(dsObs, id.vars=c("cattrans","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsObsLong$catprev<-paste0(dsObsLong$cattran,dsObsLong$variable)
dsObsWide<-dcast(dsObsLong, cohort ~ catprev, mean)
dsObsLong$catprev<-NULL
dsObsLong$variable <- gsub(pattern="pt", replacement="", x=dsObsLong$variable) 
dsObsLong$variable <- as.integer(dsObsLong$variable) #Convert to a number.
dsObsLong <- plyr::rename(dsObsLong, replace=c(variable="time", value="prevalence"))


pathResults <- file.path(pathDirectory,modelname)
pathModelIn <-file.path(pathResults,paste0(modelname,"_resultsIn",".csv"))
dsModel <- read.csv(pathModelIn, stringsAsFactors=FALSE)
dsModel<-dsModel[,c("model","cohort",pars12)]
dsModel<-join(dsModel,dsObsWide, by="cohort")

# recreate the data using model solution - forecast
dsModel<-mutate(dsModel, mpG00=Gpt00,mpI00=Ipt00, mpA00=Apt00,
                  mpG01 = Gpt00 - Tgi*Gpt00*(Ipt00^Cgi) - Tga*Gpt00*(Apt00^Cga)
                  + Tig*Ipt00*(Gpt00^Cig) + Tag*Apt00*(Gpt00^Cag),
                  mpI01 = Ipt00 - Tia*Ipt00*(Apt00^Cia) - Tig*Ipt00*(Gpt00^Cig)
                  + Tgi*Gpt00*(Ipt00^Cgi) + Tai*Apt00*(Ipt00^Cai),
                  mpA01 = 1 - mpG01 - mpI01,
                  
                  mpG02 = Gpt01 - Tgi*Gpt01*(Ipt01^Cgi) - Tga*Gpt01*(Apt01^Cga)
                  + Tig*Ipt01*(Gpt01^Cig) + Tag*Apt01*(Gpt01^Cag),
                  mpI02 = Ipt01 - Tia*Ipt01*(Apt01^Cia) - Tig*Ipt01*(Gpt01^Cig)
                  + Tgi*Gpt01*(Ipt01^Cgi) + Tai*Apt01*(Ipt01^Cai),
                  mpA02 = 1 - mpG02 - mpI02,
                  
                  mpG03 = Gpt02 - Tgi*Gpt02*(Ipt02^Cgi) - Tga*Gpt02*(Apt02^Cga)
                  + Tig*Ipt02*(Gpt02^Cig) + Tag*Apt02*(Gpt02^Cag),
                  mpI03 = Ipt02 - Tia*Ipt02*(Apt02^Cia) - Tig*Ipt02*(Gpt02^Cig)
                  + Tgi*Gpt02*(Ipt02^Cgi) + Tai*Apt02*(Ipt02^Cai),
                  mpA03 = 1 - mpG03 - mpI03,
                  
                  mpG04 = Gpt03 - Tgi*Gpt03*(Ipt03^Cgi) - Tga*Gpt03*(Apt03^Cga)
                  + Tig*Ipt03*(Gpt03^Cig) + Tag*Apt03*(Gpt03^Cag),
                  mpI04 = Ipt03 - Tia*Ipt03*(Apt03^Cia) - Tig*Ipt03*(Gpt03^Cig)
                  + Tgi*Gpt03*(Ipt03^Cgi) + Tai*Apt03*(Ipt03^Cai),
                  mpA04 = 1 - mpG04 - mpI04,
                  
                  mpG05 = Gpt04 - Tgi*Gpt04*(Ipt04^Cgi) - Tga*Gpt04*(Apt04^Cga)
                  + Tig*Ipt04*(Gpt04^Cig) + Tag*Apt04*(Gpt04^Cag),
                  mpI05 = Ipt04 - Tia*Ipt04*(Apt04^Cia) - Tig*Ipt04*(Gpt04^Cig)
                  + Tgi*Gpt04*(Ipt04^Cgi) + Tai*Apt04*(Ipt04^Cai),
                  mpA05 = 1 - mpG05 - mpI05,
                  
                  mpG06 = Gpt05 - Tgi*Gpt05*(Ipt05^Cgi) - Tga*Gpt05*(Apt05^Cga)
                  + Tig*Ipt05*(Gpt05^Cig) + Tag*Apt05*(Gpt05^Cag),
                  mpI06 = Ipt05 - Tia*Ipt05*(Apt05^Cia) - Tig*Ipt05*(Gpt05^Cig)
                  + Tgi*Gpt05*(Ipt05^Cgi) + Tai*Apt05*(Ipt05^Cai),
                  mpA06 = 1 - mpG06 - mpI06,
                  
                  mpG07 = Gpt06 - Tgi*Gpt06*(Ipt06^Cgi) - Tga*Gpt06*(Apt06^Cga)
                  + Tig*Ipt06*(Gpt06^Cig) + Tag*Apt06*(Gpt06^Cag),
                  mpI07 = Ipt06 - Tia*Ipt06*(Apt06^Cia) - Tig*Ipt06*(Gpt06^Cig)
                  + Tgi*Gpt06*(Ipt06^Cgi) + Tai*Apt06*(Ipt06^Cai),
                  mpA07 = 1 - mpG07 - mpI07,
                  
                  mpG08 = Gpt07 - Tgi*Gpt07*(Ipt07^Cgi) - Tga*Gpt07*(Apt07^Cga)
                  + Tig*Ipt07*(Gpt07^Cig) + Tag*Apt07*(Gpt07^Cag),
                  mpI08 = Ipt07 - Tia*Ipt07*(Apt07^Cia) - Tig*Ipt07*(Gpt07^Cig)
                  + Tgi*Gpt07*(Ipt07^Cgi) + Tai*Apt07*(Ipt07^Cai),
                  mpA08 = 1 - mpG08 - mpI08,
  )

# create a long file with Modeled prevalence
dsModLong<-dsModel[,c("cohort",modeled)]
dsModLong <- reshape2::melt(dsModLong, id.vars=c("cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsModLong$cattrans<-substr(dsModLong$variable,1,3)
dsModLong$variable <- gsub(pattern="mpA", replacement="", x=dsModLong$variable) 
dsModLong$variable <- gsub(pattern="mpI", replacement="", x=dsModLong$variable) 
dsModLong$variable <- gsub(pattern="mpG", replacement="", x=dsModLong$variable) 
dsModLong$variable <- as.integer(dsModLong$variable) #Convert to a number.
dsModLong <- plyr::rename(dsModLong, replace=c(variable="time", value="prevalence"))
 


?gsub








