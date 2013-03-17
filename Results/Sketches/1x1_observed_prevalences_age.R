pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)

# 1x1 line graphs with prevalences of three categores and all cohorts
#------------------------------------------------graph 1------#
dsFORp <- dsSLprops[(dsSLprops$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[(dsFORp$catatrans %in% c("pG")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)


p<-ggplot(dsFORp, aes(x=time-2000,y=proportion,group=parcoh,fill=factor(catatrans)))+
  scale_color_manual(values =catatransColor)+
  geom_line(aes(colour = catatrans))+
#   facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show_guide = FALSE)+
  scale_y_continuous("Prevalence: proportion of total",
                     limits=c(0, .7),
                     breaks=c(0,.1,.2,.3,.4,.5,.6,.7))+
    scale_x_continuous("years after 2000",              # for aes(x=time-2000)
                       limits=c(0, 14),
                       breaks=c(0:14))+
#   scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
#                      limits=c(16,30),
#                      breaks=c(16,18,20,22,24,26,28,30))+
  labs(title=paste0("Observed prevalence of church attendance categories"))
p
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches","1x1_observed_prevalences_years_Goers.png") 
png (filename = pathFileOut ,
     width = 900, height = 900 , units = "px")
plot(plast)
dev.off()