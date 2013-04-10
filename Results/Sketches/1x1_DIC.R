pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)

# DIC plot - 

#------------------------------------------------graph 2------#
dsFORp <- dsDIC [(dsDIC$cohort %in% allCohorts),]
dsFORp <- mutate(dsFORp,specmod=paste(specification,model))
p<-ggplot(dsFORp, aes(x=cohort,y=DIC,group=specmod))+
  geom_line( aes( colour = specmod, linetype=specification),size=1,guide=FALSE)+ 
  geom_point(aes(colour = specmod),size=4)+
  scale_color_manual(values = modelcolors)+
  ylim(c(-100,-40))+
  labs(title=paste0("Model fit of 4 specifications"))+
scale_y_continuous("DIC (lower=better)")
p
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches","1x1_DIC_4Models.png") 
png (filename = pathFileOut ,
     width = 900, height = 900 , units = "px")
plot(plast)
dev.off()