pathVectorsPalettes <- file.path(getwd(),"Results","Sketches","factors_and_pallets.R")
source(pathVectorsPalettes)

#  3x3 Parameter Solution 

#------------------------------------------------graph 3------#
str(dsModelsParsLong)
dsFORp <- dsModelsParsLong [(dsModelsParsLong$specification %in% c("Scaled","Original")),]
dsFORp <-mutate(dsFORp, specmod=paste0( tolower(substr(specification,1,10)), substr(model,1,10))) 
dsFORp<- mutate(dsFORp,mx=substr(parameter,2,2),my=substr(parameter,3,3))
dsFORp$mx<-ordered(dsFORp$mx, levels=c("g","i","a"))
dsFORp$my<-ordered(dsFORp$my, levels=c("g","i","a"))  
str(dsFORp)
p<-ggplot(dsFORp, aes(x=cohort,colour=specmod))+
  geom_point(aes(y=value),size=6, shape=21, fill="white")+
  facet_grid(mx~my)+
  scale_color_manual(values = modelcolors, name="specification")+
  geom_line(aes(y=value,linetype=specification),size=1.3)+
  scale_y_continuous("value of parameter",
                     limits=c(0, 1),
                     breaks=c(.2,.4,.6,.8,1))+
  geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)
p
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches","3x3_Solutions.png") 
png (filename = pathFileOut ,
     width = 1200, height = 900 , units = "px")
plot(plast)
dev.off()