pathVectorsPalettes <- file.path(getwd(),"Results","factors_and_pallets.R")
source(pathVectorsPalettes)

#  3x3 Parameter Solution 

#------------------------------------------------graph 3------#

dsFORp <- dsModelsParsLong [!(dsModelsParsLong$specification %in% c("Scaled") & dsModelsParsLong$model %in% c("Contagion")),]

dsFORp<- mutate(dsFORp,mx=substr(parameter,2,2),my=substr(parameter,3,3))
dsFORp$mx<-ordered(dsFORp$mx, levels=c("g","i","a"))
dsFORp$my<-ordered(dsFORp$my, levels=c("g","i","a"))  
str(dsFORp)
p<-ggplot(dsFORp, aes(x=cohort,colour=model))+
  labs(title=paste0("Parameter values for 3 models"))+
  facet_grid(mx~my)+
  scale_color_manual(values = modelcolors)+
  geom_line(aes(y=value),size=.7,alpha=.8)+
  geom_point(aes(y=value),size=6, shape=20)+
  scale_y_continuous("value of parameter",
                     limits=c(0, 1),
                     breaks=c(.2,.4,.6,.8,1))+
  geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)
p

plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig5b - Parameter values 3x3 Solutions.png") 
png (filename = pathFileOut ,
     width = 1200, height = 900 , units = "px")
plot(plast)
dev.off()