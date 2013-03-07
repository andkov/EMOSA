rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

cohortYear <- 1984 #1980, 1981, 1982, 1983, 1984
allCohorts<-c("1980","1981","1982","1983","1984" )
cattransPrev<-c("G","I","A")
cattransTrans<-c("gg","gi","ga","ig","ii","ia","ag","ai","aa" )
proportions<-c("cohort","cattrans","pt00","pt01","pt02","pt03","pt04","pt05","pt06","pt07","pt08")
counts     <-c("cohort","cattrans","t00" , "t01", "t02", "t03", "t04", "t05", "t06", "t07", "t08")

#Dataprep
#---------------#
#1 Links
# links for Aggregate
pathDirectory <- file.path(getwd())
pathObserved <- file.path(pathDirectory,"Data")
pathPrevTrans <-file.path(pathObserved,"prevalences_transitions.csv")

#links for Wide and Long
pathData <- file.path(getwd(), "Data")
pathInDataWide <- file.path(pathData, "subject_data_emosa_nonmiss.csv") #The name of the file to read in.
pathInDataLong<- file.path(pathData, "Long.csv")

#2 Data in and sorted
times <- 0:8
years <- 1980:1984 
#Aggregate In
dsObs<-read.csv(pathPrevTrans,stringsAsFactors=FALSE)

# Wide In
dsWide <- read.csv(pathInDataWide, stringsAsFactors=FALSE)
#Include only records with a valid birth year
dsWide <- dsWide[dsWide$byear %in% years, ]
dsWide$byear <- as.integer(dsWide$byear)

#Include only records with a valid ID
dsWide <- dsWide[dsWide$id != "V", ]
dsWide$id <- as.integer(dsWide$id)

#Drop the birth month variable
dsWide <- dsWide[, colnames(dsWide) != "bmonth"]
attach(dsWide)
       dsWide<-dsWide[order(byear),]
detach(dsWide)
head(dsWide,n=20)

# Long In
dsLongRaw <- read.csv(pathInDataLong, stringsAsFactors=FALSE)
attach(dsLongRaw)
dsLongRaw<-dsLongRaw[order(byear),]
detach(dsLongRaw)
# head(dsLongRaw,n=20)

# Values and palettes

attcol8<-c("blue3","blue","lightblue", "snow", "lightpink", "red" ,"red3", "red4")
attcol3<-c("blue2","blue2","snow", "snow", "snow", "red2" ,"red2", "red2")
prevcol3<-c("blue2","snow","red2")

categories8<-c("Never","Once or twice","Less than once a month/ 3-12 times","About once a month/ 12 times",
               "About tiwce a month/ 24 times", "About once a week", "Several times a week","Everyday")
categories8s<-c("Never","1-2/week","3-12/week","~1/month",
                "~2/month", "~1/week", "2-3/week","Everyday ")
categories3<-c("Goers","Irregulars","Absentees")
categories38<-c("Absentees","Absentees","Irregulars","Irregulars","Irregulars","Goers","Goers","Goers")


dsObs <- dsObs[dsObs$cattrans %in% cattransPrev,proportions]
dsObs80<-dsObs[dsObs$cohort==1980,]
dsObs81<-dsObs[dsObs$cohort==1981,]
dsObs82<-dsObs[dsObs$cohort==1982,]
dsObs83<-dsObs[dsObs$cohort==1983,]
dsObs84<-dsObs[dsObs$cohort==1984,]

                     
# dsObs Into dsLong
#Transform the wide dataset into a long dataset
dsLong <- reshape2::melt(dsObs, id.vars=c("cattrans","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsLong$variable <- gsub(pattern="pt", replacement="", x=dsLong$variable) #Strip off the 'att' prefix
dsLong$variable <- as.integer(dsLong$variable) #Convert to a number.
dsLong <- plyr::rename(dsLong, replace=c(variable="time", value="prevalence"))
dsLong <- dsLong[order(dsLong$cattrans, dsLong$cohort), ] #Sort for the sake of visual inspection.

#---------------#

str(dsObs)
str(dsLong)

# Import Model solutions


# Graphs
# 1 geom_bar()
# dsFORp <- subset(dsLongRaw, byear %in% cohortYear)
# p<-ggplot(dsFORp, aes(factor(time),fill=(factor(attendence)))) 
# plast<-p+geom_bar()+ scale_fill_manual(values = attcol3)
# plast
# 
# pathImageOut<-file.path(getwd(),"EMOSA_ggplot/ggplot_graphs/prevalences from long by cohort")
# pathFileOut<-file.path(pathImageOut,paste0(cohortYear,"_3col",".png"))
# png(filename = pathFileOut,
#     width =1600, height =900 , units = "px")
# plot(plast)
# dev.off()

#temp


# 2 geom_bar(position="fill")
dsFORp <- subset(dsLongRaw, byear %in% cohortYear)
p<-ggplot(dsFORp, aes(x=factor(time), fill=factor(attendence)))+
  geom_bar(position="fill")+
  scale_fill_manual(values = attcol8,
                    labels=categories8short,
                    name=" ",
                    )+
  scale_y_continuous("Prevalence: proportion of total",
                     limits=c(0, 1),
                     breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  scale_x_discrete("years since 2000")+
  labs(title=c("In the past year, how often have you attended a worship service?"))
p
plast<-p
pathImageOut<-file.path(getwd(),"EMOSA_ggplot/ggplot_graphs/prevalences from long by cohort")
pathFileOut<-file.path(pathImageOut,paste0(cohortYear,".png"))
png(filename = pathFileOut,
    width =1600, height =900 , units = "px")
plot(plast)
dev.off()


# brings in the model solution -connecting with "originalResults.R" - 
model<-c("originalHybridGauss")
pathResults <- file.path(pathDirectory,model)
pathModelIn <-file.path(pathResults,paste0(model,"_resultsIn",".csv"))
dsModel <- read.csv(pathModelIn, stringsAsFactors=FALSE)

dsModel80 <- merge(dsModel[dsModel$cohort==1980,], dsObs80,all.x=TRUE)

dsGpt00 <-dsObs80[dsObs80$cattrans=="G",c("pt00")]
dsGpt01 <-dsObs80[dsObs80$cattrans=="G",c("pt01")]
dsGpt02 <-dsObs80[dsObs80$cattrans=="G",c("pt02")]
dsGpt03 <-dsObs80[dsObs80$cattrans=="G",c("pt03")]
dsGpt04 <-dsObs80[dsObs80$cattrans=="G",c("pt04")]
dsGpt05 <-dsObs80[dsObs80$cattrans=="G",c("pt05")]
dsGpt06 <-dsObs80[dsObs80$cattrans=="G",c("pt06")]
dsGpt07 <-dsObs80[dsObs80$cattrans=="G",c("pt07")]
dsGpt08 <-dsObs80[dsObs80$cattrans=="G",c("pt08")]

dsModel80$Gpt00<-dsGpt00
dsModel80$Gpt01<-dsGpt01
dsModel80$Gpt02<-dsGpt02
dsModel80$Gpt03<-dsGpt03
dsModel80$Gpt04<-dsGpt04
dsModel80$Gpt05<-dsGpt05
dsModel80$Gpt06<-dsGpt06
dsModel80$Gpt07<-dsGpt07
dsModel80$Gpt08<-dsGpt08

dsApt00 <-dsObs80[dsObs80$cattrans=="A",c("pt00")]
dsApt01 <-dsObs80[dsObs80$cattrans=="A",c("pt01")]
dsApt02 <-dsObs80[dsObs80$cattrans=="A",c("pt02")]
dsApt03 <-dsObs80[dsObs80$cattrans=="A",c("pt03")]
dsApt04 <-dsObs80[dsObs80$cattrans=="A",c("pt04")]
dsApt05 <-dsObs80[dsObs80$cattrans=="A",c("pt05")]
dsApt06 <-dsObs80[dsObs80$cattrans=="A",c("pt06")]
dsApt07 <-dsObs80[dsObs80$cattrans=="A",c("pt07")]
dsApt08 <-dsObs80[dsObs80$cattrans=="A",c("pt08")]

dsModel80$Apt00<-dsApt00
dsModel80$Apt01<-dsApt01
dsModel80$Apt02<-dsApt02
dsModel80$Apt03<-dsApt03
dsModel80$Apt04<-dsApt04
dsModel80$Apt05<-dsApt05
dsModel80$Apt06<-dsApt06
dsModel80$Apt07<-dsApt07
dsModel80$Apt08<-dsApt08

dsIpt00 <-dsObs80[dsObs80$cattrans=="I",c("pt00")]
dsIpt01 <-dsObs80[dsObs80$cattrans=="I",c("pt01")]
dsIpt02 <-dsObs80[dsObs80$cattrans=="I",c("pt02")]
dsIpt03 <-dsObs80[dsObs80$cattrans=="I",c("pt03")]
dsIpt04 <-dsObs80[dsObs80$cattrans=="I",c("pt04")]
dsIpt05 <-dsObs80[dsObs80$cattrans=="I",c("pt05")]
dsIpt06 <-dsObs80[dsObs80$cattrans=="I",c("pt06")]
dsIpt07 <-dsObs80[dsObs80$cattrans=="I",c("pt07")]
dsIpt08 <-dsObs80[dsObs80$cattrans=="I",c("pt08")]

dsModel80$Ipt00<-dsIpt00
dsModel80$Ipt01<-dsIpt01
dsModel80$Ipt02<-dsIpt02
dsModel80$Ipt03<-dsIpt03
dsModel80$Ipt04<-dsIpt04
dsModel80$Ipt05<-dsIpt05
dsModel80$Ipt06<-dsIpt06
dsModel80$Ipt07<-dsIpt07
dsModel80$Ipt08<-dsIpt08





dsGpt00 <- rename(dsGpt00, c(pt00="Gpt00"))
dsModel <- merge(dsModel, dsGpt00,all=TRUE)


# dsModel Into dsLongM
#Transform the wide dataset into a long dataset
# icohort<-1980
# modelIn<-dsObs[which(dsObs$cohort==icohort),]
# dropvar<-names(modelIn) %in% c("cohort") 
# modelIn<-modelIn[!dropvar]



# Prevalence by 5 cohorts
dsFORp <- subset(dsLong, cohort %in% allCohorts)
p<-ggplot(dsFORp, aes(x=time,y=prevalence,group=cattrans,fill=factor(cattrans)))+
  scale_color_manual(values = c("G"="green2","I"="goldenrod2","A"="red3"))+
  geom_line(aes(colour = cattrans))+ facet_grid(. ~ cohort)+
  geom_point(aes(colour=cattrans),show_guide = FALSE)+
  labs(title=paste0("Observed by predicted for each cohort"))
p
plast<-p

pathImageOut<-file.path(getwd(),"EMOSA_ggplot/ggplot_graphs/prevalences from long by cohort")
pathFileOut<-file.path(pathImageOut,paste0("Observed prevalences",".png"))
png(filename = pathFileOut,
    width =1600, height =300 , units = "px")
plot(plast)
dev.off()


# dsObs4model <- reshape2::melt(dsObs, id.vars=c("cattrans","cohort"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
# dsObs4model$prevs<-paste0(dsObs4model$cattrans,dsObs4model$variable)
# 
# ?reshape
# 
# dsLongM$variable <- gsub(pattern="pt", replacement="", x=dsLongM$variable) #Strip off the 'att' prefix
# dsLongM$variable <- as.integer(dsLongM$variable) #Convert to a number.
# dsLongM <- plyr::rename(dsLongM, replace=c(variable="time", value="prevalence"))
# dsLongM <- dsLong[order(dsLongM$cattrans, dsLongM$cohort), ] #Sort for the sake of visual inspection.

  