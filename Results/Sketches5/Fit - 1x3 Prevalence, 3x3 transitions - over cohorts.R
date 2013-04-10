pathVectorsPalettes <- file.path(getwd(),"Results","factors_and_pallets.R")
source(pathVectorsPalettes)

sketches<-5 # SketchesNN - number of the folder


# 1x1 - Total Fit (x:cohorts, y=total misfit) - prevalences
dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-ggplot(dsFORp, aes(x=cohort))+
  labs(title=paste0("Total misfit in predicting prevalences"))+
  geom_line(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=.5,color="royalblue2")+
  geom_point(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=3,color="royalblue2")+
  geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")+
  geom_point(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=3,color="goldenrod1")+
  geom_line(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=.5,color="violet")+
  geom_point(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=3,color="violet")+
  annotate("text", x=1981,y=.05, label ="contagion", color="goldenrod1", size=7)+
  annotate("text", x=1981,y=.04, label ="diffusion", color="royalblue2", size=7)+
  annotate("text", x=1981,y=.03, label ="hybrid", color="violet", size=7)+
  scale_y_continuous("Misfit: SS (lower=better)")
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"Fit - 1x1 -Prevalences.png") 
png (filename = pathFileOut ,
     width = 300, height = 300 , units = "px")
plot(plast)
dev.off()

# 1x1 -Total Fit (x:cohorts, y=total misfit) - transitions
dsFORp <- dsEMOSA[!(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-ggplot(dsFORp, aes(x=cohort))+
  labs(title=paste0("Total misfit in predicting transitions"))+
  geom_line(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=.5,color="royalblue2")+
  geom_point(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=3,color="royalblue2")+
  geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")+
  geom_point(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=3,color="goldenrod1")+
  geom_line(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=.5,color="violet")+
  geom_point(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=3,color="violet")+
#   annotate("text", x=1981,y=1.0, label ="contagion", color="goldenrod1", size=7)+
#   annotate("text", x=1981,y=.85, label ="diffusion", color="royalblue2", size=7)+
#   annotate("text", x=1981,y=.70, label ="hybrid", color="violet", size=7)+
  scale_y_continuous("Misfit: SS (lower=better)")
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"Fit - 1x1 -Transitions.png") 
png (filename = pathFileOut ,
     width = 300, height = 300 , units = "px")
plot(plast)
dev.off()

# 1x3 - Fit (x:cohorts, y=total misfit) - prevalences
dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-ggplot(dsFORp, aes(x=cohort))+
  labs(title=paste0("Misfit in predicting prevalences"))+
  geom_line(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=.5,color="royalblue2")+
  geom_point(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=3,color="royalblue2")+
  geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")+
  geom_point(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=3,color="goldenrod1")+
  geom_line(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=.5,color="violet")+
  geom_point(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=3,color="violet")+
  scale_y_continuous("Misfit: SS (lower=better)")+
  facet_grid(.~catatrans)
p   
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"Fit - 1x3 -Prevalences.png") 
png (filename = pathFileOut ,
     width = 900, height = 300 , units = "px")
plot(plast)
dev.off()


# 3x3 - Fit (x:cohorts, y=total misfit) - transitions
dsFORp <- dsEMOSA[!(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-ggplot(dsFORp, aes(x=cohort))+
  labs(title=paste0("Misfit in predicting transitions"))+
  geom_line(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=.5,color="royalblue2")+
  geom_point(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=3,color="royalblue2")+
  geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")+
  geom_point(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=3,color="goldenrod1")+
  geom_line(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=.5,color="violet")+
  geom_point(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=3,color="violet")+
  scale_y_continuous("Misfit: SS (lower=better)")+
  facet_grid(mx~my)
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results",paste0("Sketches",sketches),"Fit - 3x3 -Transitions.png") 
png (filename = pathFileOut ,
     width = 800, height = 500 , units = "px")
plot(plast)
dev.off()


         