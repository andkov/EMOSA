pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)
sketches<-4 # SketchesNN - number of the folder



# 5x1  vertical strip of recreated prevalence individually for all 5 cohorts 
#------------------------------------------------graph 1------#
dsFORp <- dsEMOSA[(dsEMOSA$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[(dsFORp$catatrans %in% c("pA","pI","pG")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:1980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)

ylimits<-c(0.0, .7) # min and max of the Y-axis
ybreaks<-c(0,.1,.2,.3,.4,.5,.6,.7) # tick-points of the Y-axis
agelimits<-c(16, 30) # min and max of the X-axis
agebreaks<-c(16,18,20,22,24,26,28,30) # tick-points of the X-axis
yearlimits<-c(0,14) # min and max of the X-axis
yearbreaks<-c(0:14) # tick-points of the X-axis


model<-"Hybrid" # change _proportion name
p<-ggplot(dsFORp, aes(x=age,group=catatrans,fill=factor(catatrans)))+
  labs(title=paste0("Recreating prevalences with diffusion"))+
  geom_line(aes(y=obs_proportion,colour = catatrans),size=2.5,alpha=.3,show_guide = FALSE)+
  geom_line(aes(y=OH_proportion), linetype="solid")+ # changes with the model choice
#   geom_point(aes(colour=catatrans),show_guide = FALSE)+
  
  scale_y_continuous("Prevalence: proportion of total",
                     limits=ylimits,
                     breaks=ybreaks)+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=agelimits,
                     breaks=agebreaks)+
  scale_color_manual(values =catatransColor)+
  facet_grid(cohort~.)
p
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),paste0("Recreating",model,"- 5x1 -prevalences_age.png")) 
png (filename = pathFileOut ,
     width = 400, height = 1200 , units = "px")
plot(plast)
dev.off()





