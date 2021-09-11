library(magrittr)
# library(tidyverse)

# ---- 1 ----------------------------------

# We aim to debug:
# 1x1 - Total Fit (x:cohorts, y=total misfit) - prevalences
# dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] 
# p<-ggplot(dsFORp, aes(x=cohort))+
#   labs(title=paste0("Total misfit in predicting prevalences"))+
#   geom_line(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=.5,color="royalblue2")+
#   geom_point(aes(y=sqdif_OD), stat="summary",fun.y="sum",size=3,color="royalblue2")+
#   geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")+
#   geom_point(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=3,color="goldenrod1")+
#   geom_line(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=.5,color="violet")+
#   geom_point(aes(y=sqdif_OH), stat="summary",fun.y="sum",size=3,color="violet")+
#   # annotate("text", x=1981,y=.05, label ="contagion", color="goldenrod1", size=7)+
#   # annotate("text", x=1981,y=.04, label ="diffusion", color="royalblue2", size=7)+
#   # annotate("text", x=1981,y=.03, label ="hybrid", color="violet", size=7)+
#   scale_y_continuous("Misfit: SS (lower=better)")
# # scale_y_continuous("Misfit: SS (lower=better)", limits = c(0, .001))
# p 


dsFORp <- dsEMOSA[(dsEMOSA$catatrans %in% c("pG","pI","pA")),] %>% tibble::as_tibble()


d <- dsFORp %>% 
  dplyr::group_by(cohort) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  )
d %>% ggplot(aes(x=cohort))+
  geom_line(aes(y=sqdif_OC))


p <- 
  dsFORp %>% 
  dplyr::group_by(cohort) %>% 
  dplyr::summarize(
    sqdif_OD = sum(sqdif_OD, na.rm = T)
    ,sqdif_OC = sum(sqdif_OC, na.rm = T)
    ,sqdif_OH = sum(sqdif_OH, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=cohort))+
  geom_line(aes(y=sqdif_OD)  ,size=.5,color="royalblue2")+
  geom_point(aes(y=sqdif_OD) ,size=3,color="royalblue2")+
  geom_line(aes(y=sqdif_OC)  ,size=.5,color="goldenrod1")+
  geom_point(aes(y=sqdif_OC) ,size=3,color="goldenrod1")+
  geom_line(aes(y=sqdif_OH)  ,size=.5,color="violet")+
  geom_point(aes(y=sqdif_OH) ,size=3,color="violet")+
  annotate("text", x=1981,y=.05, label ="contagion", color="goldenrod1", size=7)+
  annotate("text", x=1981,y=.04, label ="diffusion", color="royalblue2", size=7)+
  annotate("text", x=1981,y=.03, label ="hybrid", color="violet", size=7)+
  scale_y_continuous("Misfit: SS (lower=better)")+
  labs(title=paste0("Total misfit in predicting prevalences"))
p 

p <- ggplot(dsFORp, aes(x=cohort))+
  # geom_line(aes(y=sqdif_OC), stat="summary",fun.y="sum",size=.5,color="goldenrod1")
  geom_line(aes(y = sqdif_OC), stat = "sum")
p


dsFORp %>% 
  filter(catatrans %in% c("pG","pI","pA")) %>% 
  ggplot(aes(x=cohort))+
  geom_line(aes(y=sqdif_OC))+
  geom_line(aes(y=sqdif_OD))+
  geom_line(aes(y=sqdif_OH))


# dsFORp %>% 
  
  install.packages("ggplot2", version='2.0.0')
  # install.packages("ggplot2", version='0.9.1')