
rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
cohortYear <- 1984 #1980, 1981, 1982, 1983, 1984

## @knitr GoDogGo
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

model<-"AlternativeContagion"
distribution<-"Gauss"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AlternativeContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AlternativeContagionGauss.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AlternativeContagionGauss.bugs")
    pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")      
  }
  else {
    stop(paste0("The working directory '", basename(getwd()),"' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
  }
}

# curve(dbeta(x, 1,1))
# curve(dbeta(x, 10,10))
# curve(dlogis(x, location = .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors=FALSE)
ds <- ds[ds$byear == cohortYear, ] #Select only the desired cohort
ds <- ds[order(ds$time), ] #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

#Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check these with data; I may have messed up the order}
#For the 1984 cohort
# pg <- c(0.401088929, 0.340290381,  0.249546279,	0.218693285,	0.180580762,	0.167876588,	0.157894737,	0.158802178,  0.161524501)
# pi <- c(0.233212341, 0.256805808,	0.288566243,	0.305807623,	0.27676951,	  0.270417423,	0.229582577,	0.250453721,  0.237749546)
# pa <- c(0.36569873,  0.402903811,	0.461887477,	0.475499093,	0.542649728,	0.561705989,	0.612522686,	0.590744102,  0.600725953)
timeCount <- length(pg)
if( length(pi) != timeCount) stop("The proportions have a different number of time points.")
if( length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


#parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai","sumG", "sumI") ) #For Beta
parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss

#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100000#00#00

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
# mcarray <- jags.samples(model=jagsModel, variable.names=parametersToTrackWithDic, n.iter=countIterations ) #If I understand correctly, the following line is similar, but better
# as.mcmc.list(mcarray$Cag)
# mcarray <- mcmc(mcarray)
# mcarray <- mcmc.list(mcarray)
# nchain(mcarray)
# str(mcarray)
# class(mcarray)
# summary(mcarray)

chains <- coda.samples(jagsModel, variable.names=parametersToTrack, n.iter=countIterations)# updates the model, and coerces the output to a single mcmc.list object. 
# chains <- coda.samples(jagsModel, variable.names=parametersToTrackWithDic, n.iter=countIterations)# updates the model, and coerces the output to a single mcmc.list object.
# class(chains)
elapsed  <- Sys.time() - startTime
(condensed <- summary(chains))
# head(chains, 20)

# windows() # dev.off()
(rhat<-gelman.diag(chains, autoburnin=FALSE)) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
(effSize<-effectiveSize(chains)) #Sample size adjusted for autocorrelation
xyplot<-xyplot(chains) #Needs at least two parameters; else throws an error.
density<-densityplot(chains)

# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed

particularity<-countIterations
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
xyplotname<-"xyplot"  # assigns the name of the file with posterior distribution of parameters
densname<- "densname" # assigns the name of the file with evaluated parameter sets

modnumdens<-paste(densname,model,cohortYear,particularity,"png", sep='.') 
modnumxyplot<-paste(xyplotname,model,cohortYear,particularity,"png", sep='.') 

pathDensityOut<-file.path(pathOutData,modnumdens)
pathXYplotOut<- file.path(pathOutData,modnumxyplot)

png(filename = pathDensityOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
plot(density, main="some title")
dev.off()

png(filename = pathXYplotOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
plot(xyplot, main="some title")
dev.off()



fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,".iter",countIterations,"solution.txt"))
sink(fileSink)
distribution
model
particularity
cohortYear
dic
condensed
rhat
effSize
sink()
# THis file to be read and processed by "examining the results of the Alternative Hibrid from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AlternativeHybrid"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43