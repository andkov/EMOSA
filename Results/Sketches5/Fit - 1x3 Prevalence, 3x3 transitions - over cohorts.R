pathVectorsPalettes <- file.path(getwd(),"Results","factors_and_pallets.R")
source(pathVectorsPalettes)

# 1x1 - Total Fit (x:cohorts, y=total misfit) - prevalences

library(magrittr)
dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p <- 
  dsFORp %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(cohort) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=cohort))+
  geom_line( aes(y=sqdif_OD) ,size = .5 ,color="royalblue2" )+
  geom_point(aes(y=sqdif_OD) ,size = 3  ,color="royalblue2" )+
  geom_line( aes(y=sqdif_OC) ,size = .5 ,color="goldenrod1" )+
  geom_point(aes(y=sqdif_OC) ,size = 3  ,color="goldenrod1" )+
  geom_line( aes(y=sqdif_OH) ,size = .5 ,color="violet"     )+
  geom_point(aes(y=sqdif_OH) ,size = 3  ,color="violet"     )+
  annotate("text", x=1981,y=.05, label ="contagion", color="goldenrod1", size=7)+
  annotate("text", x=1981,y=.04, label ="diffusion", color="royalblue2", size=7)+
  annotate("text", x=1981,y=.03, label ="hybrid", color="violet", size=7)+
  scale_y_continuous("Misfit: SS (lower=better)")+
  labs(title=paste0("Total misfit in predicting prevalences"))
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig7b-Fit-1x1-Prevalences.png") 
png (filename = pathFileOut ,
     width = 300, height = 300 , units = "px")
plot(plast)
dev.off()

# 1x1 -Total Fit (x:cohorts, y=total misfit) - transitions
dsFORp <- dsEMOSA[!(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p <-
  dsFORp %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(cohort) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=cohort))+
  geom_line( aes(y=sqdif_OD) ,size = .5 ,color="royalblue2" )+
  geom_point(aes(y=sqdif_OD) ,size = 3  ,color="royalblue2" )+
  geom_line( aes(y=sqdif_OC) ,size = .5 ,color="goldenrod1" )+
  geom_point(aes(y=sqdif_OC) ,size = 3  ,color="goldenrod1" )+
  geom_line( aes(y=sqdif_OH) ,size = .5 ,color="violet"     )+
  geom_point(aes(y=sqdif_OH) ,size = 3  ,color="violet"     )+
  scale_y_continuous("Misfit: SS (lower=better)")+
  labs(title=paste0("Total misfit in predicting transitions"))
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig7a-Fit-1x1-Transitions.png") 
png (filename = pathFileOut ,
     width = 300, height = 300 , units = "px")
plot(plast)
dev.off()


# 1x3 - Fit (x:cohorts, y=total misfit) - prevalences
dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-
  dsFORp %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(cohort, catatrans) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=cohort))+
  geom_line( aes(y=sqdif_OD) ,size = .5 ,color="royalblue2" )+
  geom_point(aes(y=sqdif_OD) ,size = 3  ,color="royalblue2" )+
  geom_line( aes(y=sqdif_OC) ,size = .5 ,color="goldenrod1" )+
  geom_point(aes(y=sqdif_OC) ,size = 3  ,color="goldenrod1" )+
  geom_line( aes(y=sqdif_OH) ,size = .5 ,color="violet"     )+
  geom_point(aes(y=sqdif_OH) ,size = 3  ,color="violet"     )+
  facet_grid(.~catatrans)+
  scale_y_continuous("Misfit: SS (lower=better)")+
  labs(title=paste0("Misfit in predicting prevalences"))
p   
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig7c-Fit-1x3-Prevalences.png") 
png (filename = pathFileOut ,
     width = 900, height = 300 , units = "px")
plot(plast)
dev.off()


# 3x3 - Fit (x:cohorts, y=total misfit) - transitions
dsFORp <- dsEMOSA[!(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
p<-
  dsFORp %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(cohort, mx, my) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=cohort))+
  geom_line( aes(y=sqdif_OD) ,size = .5 ,color="royalblue2" )+
  geom_point(aes(y=sqdif_OD) ,size = 3  ,color="royalblue2" )+
  geom_line( aes(y=sqdif_OC) ,size = .5 ,color="goldenrod1" )+
  geom_point(aes(y=sqdif_OC) ,size = 3  ,color="goldenrod1" )+
  geom_line( aes(y=sqdif_OH) ,size = .5 ,color="violet"     )+
  geom_point(aes(y=sqdif_OH) ,size = 3  ,color="violet"     )+
  facet_grid(mx~my)+
  scale_y_continuous("Misfit: SS (lower=better)")+
  labs(title=paste0("Misfit in predicting transitions"))
p 
plast<-p
pathFileOut<-file.path(getwd(),"Results","Sketches5","Fig7d-Fit-3x3-Transitions.png") 
png (filename = pathFileOut ,
     width = 800, height = 500 , units = "px")
plot(plast)
dev.off()


         