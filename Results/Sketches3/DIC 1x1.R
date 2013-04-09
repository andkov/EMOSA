pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)

# DIC plot - 

#------------------------------------------------graph 2------#
dsFORp <- dsDICLong#[!(dsDICLong$specification %in% c("Scaled") & dsDICLong$model %in% c("Contagion")),]
dsFORp <- dsFORp[(dsFORp$index %in% c("DIC")),] 

p<-ggplot(dsFORp, aes(x=cohort,y=value,group=model))+
  geom_line( aes( colour = model),size=1,guide=FALSE)+ 
  geom_point(aes(colour = model),size=4)+
  scale_color_manual(values = modelcolors)+
  ylim(c(-100,-40))+
  labs(title=paste0("Deviance Information Criterion: 3 models"))+
scale_y_continuous("DIC (lower=better)")
p


plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches3","DIC 1x1 3 models.png") 
png (filename = pathFileOut ,
     width = 500, height = 400 , units = "px")
plot(plast)
dev.off()