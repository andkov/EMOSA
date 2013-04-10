pathVectorsPalettes <- file.path(getwd(),"Results","factors_and_pallets.R")
source(pathVectorsPalettes)

sketches<-4 # SketchesNN - number of the folder


dsFORp <- dsLSP_catatrans#[(dsSLprops$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[(dsFORp$catatrans %in% c("pG","pI","pA")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:1980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)


ylimits<-c(0.0, .7) # min and max of the Y-axis
ybreaks<-c(0,.1,.2,.3,.4,.5,.6,.7) # tick-points of the Y-axis
agelimits<-c(16, 30) # min and max of the X-axis
agebreaks<-c(16,18,20,22,24,26,28,30) # tick-points of the X-axis
yearlimits<-c(0,14) # min and max of the X-axis
yearbreaks<-c(0:14) # tick-points of the X-axis

# 1x1 line graphs with prevalences of three categores and all cohorts - AGES
#------------------------------------------------graph 1------#
p<-ggplot(dsFORp, aes(x=age,y=obs_proportion,group=parcoh,fill=factor(catatrans)))+
  scale_color_manual(values =catatransColor)+
  geom_line(aes(colour = catatrans))+
#   facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show_guide = FALSE)+
  scale_y_continuous("Prevalence: proportion of total",
                     limits=ylimits,
                     breaks=ybreaks)+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=agelimits,
                     breaks=agebreaks)+
  labs(title=paste0("Prevalence of church attendance"))
p
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"observed_1x1_prevalences_age.png") 
png (filename = pathFileOut ,
     width = 350, height = 300 , units = "px")
plot(plast)
dev.off()


# 1x1 line graphs with prevalences of three categores and all cohorts - YEARS
#------------------------------------------------graph 2------#
p<-ggplot(dsFORp, aes(x=time-2000,y=obs_proportion,group=parcoh,fill=factor(catatrans)))+
  scale_color_manual(values =catatransColor)+
  geom_line(aes(colour = catatrans))+
  #   facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show_guide = FALSE)+
  scale_y_continuous("Prevalence: proportion of total",
                     limits=ylimits,
                     breaks=ybreaks)+
      scale_x_continuous("years after 2000",              # for aes(x=time-2000)
                         limits=yearlimits,
                         breaks=yearbreaks)+
  labs(title=paste0("Prevalence of church attendance"))
p

plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"observed_1x1_prevalences_years.png") 
png (filename = pathFileOut ,
     width = 350, height = 300 , units = "px")
plot(plast)
dev.off()