pathVectorsPalettes <- file.path(getwd(),"Results","factors_and_pallets.R")
source(pathVectorsPalettes)


# 5x1  vertical strip of recreated prevalence individually for all 5 cohorts 
#------------------------------------------------graph 1------#
dsFORp <- dsEMOSA[(dsEMOSA$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[(dsFORp$catatrans %in% c("pA","pI","pG")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:1980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)

p<-ggplot(dsFORp, aes(x=age,group=catatrans,fill=factor(catatrans)))+
  labs(title=paste0("Recreating prevalences with diffusion"))+
  geom_line(aes(y=obs_proportion,colour = catatrans),size=2.5,alpha=.3,show.legend =  FALSE)+
  geom_line(aes(y=OD_proportion), linetype="solid")+
#   geom_point(aes(colour=catatrans),show_guide = FALSE)+
  
  scale_y_continuous("Prevalence: proportion of total",
                     limits=c(0, .7),
                     breaks=c(0,.1,.2,.3,.4,.5,.6,.7))+
  #   scale_x_continuous("years after 2000",              # for aes(x=time-2000)
  #                      limits=c(0, 10),
  #                      breaks=c(0:10))+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=c(16,30),
                     breaks=c(16,18,20,22,24,26,28,30))+
  scale_color_manual(values =catatransColor)+
  facet_grid(cohort~.)
p
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig5c - Recreating diffusion - 5x1 -prevalences_age.png") 
png (filename = pathFileOut ,
     width = 400, height = 1200 , units = "px")
plot(plast)
dev.off()





