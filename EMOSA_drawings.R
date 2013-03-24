# Values and palettes

attcol8<-c("blue3","blue","lightblue", "snow", "lightpink", "red" ,"red3", "red4")
attcol3<-c("blue2","blue2","snow", "snow", "snow", "red2" ,"red2", "red2")
prevcol3<-c("blue2","snow","red2")

categories8long<-c("Never",
                   "Once or twice","Less than once a month/ 3-12 times",
                   "About once a month/ 12 times",
                   "About tiwce a month/ 24 times",
                   "About once a week",
                   "Several times a week",
                   "Everyday")
categories8<-    c("Never    ",
                   "1-2/year ",
                   "3-12/year",
                   "~1/month ",
                   "~2/month ",
                   "~1/week  ",
                   "2-3/week ",
                   "Everyday ")
categories3<-c("Goers","Irregulars","Absentees")
categories38<-c("Absentees","Absentees","Irregulars","Irregulars","Irregulars","Goers","Goers","Goers")
# catatrans<-factor(c("G","gg", "gi", "ga",
#                     "I", "ig", "ii", "ia",
#                     "A", "ag", "ai", "aa"),order=TRUE)
# Graphs
cohortYear <- 1984
allCohorts<-c("1980","1981","1982","1983","1984" )


# Prevalence for all 5 cohorts - 1x5 strip
#------------------------------------------------graph 1------#
# str(dsSLprops)
# dsFORp <- dsSLprops[(dsSLprops$cohort %in% allCohorts),]
# # select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
# dsFORp <- dsFORp[(dsSLprops$catatrans %in% c("pA","pI","pG")),] 
# dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:1980))
# p<-ggplot(dsFORp, aes(x=age,y=proportion,group=catatrans,fill=factor(catatrans)))+
#   scale_color_manual(values = c("pG"="red2","pI"="black","pA"="blue2"))+
#   geom_line(aes(colour = catatrans))+
#   facet_grid(. ~ cohort)+
#   geom_point(aes(colour=catatrans),show_guide = FALSE)+
#   scale_y_continuous("Prevalence: proportion of total",
#                      limits=c(0, .7),
#                      breaks=c(0,.1,.2,.3,.4,.5,.6,.7))+
#   #   scale_x_continuous("years after 2000",              # for aes(x=time-2000)
#   #                      limits=c(0, 10),
#   #                      breaks=c(0:10))+
#   scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
#                      limits=c(16,30),
#                      breaks=c(16:30))+
#   labs(title=paste0("Observed by predicted for each cohort"))
# p

# All cohort together - over years and ages - unit plot
#------------------------------------------------graph 2------#
catatransColor<-c("pG"="red2","pgg"="red","pgi"="red","pga"="red",
                  "pI"="black","pig"="black","pii"="black","pia"="black",
                  "pA"="blue2","pag"="blue2","pai"="blue2","paa"="blue2")
str(dsSLprops)
dsFORp <- dsSLprops[(dsSLprops$cohort %in% allCohorts),]
# select prevalences ("pA","pI","pG") or transitions ("pgg","pgi","pga","pig","pii","pia","pag","pai","paa")
dsFORp <- dsFORp[!(dsFORp$catatrans %in% c("pA","pI","pG")),] 
dsFORp$cohort<-factor(dsFORp$cohort, levels=c(1984:980))
dsFORp <- mutate(dsFORp,parcoh=paste(cohort,catatrans))
table(dsFORp$catatrans)

p<-ggplot(dsFORp, aes(x=age,y=proportion,group=factor(parcoh),fill=factor(catatrans)))+
  scale_color_manual(values =catatransColor)+
  geom_line(aes(group=parcoh,colour = catatrans))+
  facet_grid(. ~ cohort)+
  geom_point(aes(colour=catatrans),show_guide = FALSE)+
  scale_y_continuous("Prevalence: proportion of total",                     
                     limits=c(0, .7),
                     breaks=c(0,.1,.2,.3,.4,.5,.6,.7))+
#     scale_x_continuous("years after 2000",              # for aes(x=time-2000)
#                        limits=c(0, 14),
#                        breaks=c(0:14))+
  scale_x_continuous("age in years at the time of the interview",     # for aes(x=age)
                     limits=c(16,30),
                     breaks=c(16:30))+
  labs(title=paste0("Observed by predicted for each cohort"))
p

# DIC plot - 
modelcolors<-c("goldenrod2","royalblue1","goldenrod1","royalblue2")
#------------------------------------------------graph 2------#
dsFORp <- dsDIC [(dsDIC$cohort %in% allCohorts),]
dsFORp <- mutate(dsFORp,specmod=paste(specification,model))
p<-ggplot(dsFORp, aes(x=cohort,y=DIC,group=specmod))+
  geom_line( aes( colour = specmod, linetype=specification),size=1,guide=FALSE)+ 
  geom_point(aes(colour = specmod),size=4)+
  scale_color_manual(values = modelcolors)+
  ylim(c(-100,-40))+
  labs(title=paste0(" specification"))
p
# plast<-p
# pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_DIC.png")) 
# png (filename = pathFileOut ,
#      width = 600, height = 800 , units = "px")
# plot(plast)
# dev.off()


# Parameter Solution 3x3 matrix
modelcolors<-c("goldenrod","steelblue2","gold","cyan2")
#------------------------------------------------graph 3------#
str(dsModelsParsLong)
dsFORp <- dsModelsParsLong [(dsModelsParsLong$specification %in% c("Scaled","Original")),]
dsFORp <-mutate(dsFORp, specmod=paste0( tolower(substr(specification,1,1)), substr(model,1,1))) 
dsFORp<- mutate(dsFORp,mx=substr(parameter,2,2),my=substr(parameter,3,3))
dsFORp$mx<-ordered(dsFORp$mx, levels=c("g","i","a"))
dsFORp$my<-ordered(dsFORp$my, levels=c("g","i","a"))  
str(dsFORp)
p<-ggplot(dsFORp, aes(x=cohort,colour=specmod))+
  geom_point(aes(y=value))+
  facet_grid(mx~my)+
  scale_color_manual(values = modelcolors, name="specification")+
  geom_line(aes(y=value))+
  scale_y_continuous("value of parameter",
                     limits=c(0, 1),
                     breaks=c(.2,.4,.6,.8,1))+
  geom_abline(intercept = .5, slope = 0, color="red", size=.1,linetype=4)
p
# plast<-p
# pathFileOut<-file.path(getwd(),paste0(specification,"Results","/",specification,"_Solution.png")) 
# png (filename = pathFileOut ,
#      width = 1600, height = 200 , units = "px")
# plot(plast)
# dev.off()


# Prevalences across the 2000-2011 and 16-30
modelcolors<-c("goldenrod","steelblue2","gold","cyan2")
#------------------------------------------------graph 3------#
