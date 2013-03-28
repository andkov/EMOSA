pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)
palette <- choose_palette() 
cc<-palette(5)

# 3x3 matrix of OBSERVED transition probabilities as % total
#------------------------------------------------graph 2------#
dsFORp <- dsEMOSA#[(dsSLprops$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[!(dsFORp$catatrans %in% c("pG","pI","pA")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:1980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)

p<-ggplot(dsFORp, aes(x=age,y=obs_proportion,group=factor(parcoh),fill=factor(catatrans)))+
  scale_color_manual(values =catatransColor)+
  labs(title=paste0("Observed probability of transition. Clockwise direction"))+
  geom_line(aes(group=parcoh),size=3, alpha=.2)+
# geom_point(aes(colour=catatrans),show_guide = FALSE)+
  scale_y_continuous("Percent of total who make the transition",                     
                     limits=c(0, .6),
                     breaks=c(0,.1,.2,.3,.4,.5,.6,.7))+
#       scale_x_continuous("years after 2000",              # for aes(x=time-2000)
#                          limits=c(0, 14),
#                          breaks=c(0:14))+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=c(16,30),
                     breaks=c(16,18,20,22,24,26,28,30))+
#   geom_line(aes(group=parcoh))+
  facet_grid(mx ~ my)

p
# plast<-p
# pathFileOut<-file.path(getwd(),"Results","Sketches","3x3_observed_prevalences_years.png") 
# png (filename = pathFileOut ,
#      width = 900, height = 900 , units = "px")
# plot(plast)
# dev.off()
