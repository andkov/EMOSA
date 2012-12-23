rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
cohortYear <- 1984 #1980, 1981, 1982, 1983, 1984

## @knitr GoDogGo
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

pathDirectory <- file.path(getwd())
pathModel <- file.path(pathDirectory, "Probit/HybridOnly/HybridProbit.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))
  
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
# pg <- c(0.401088929, 0.340290381,	0.249546279,	0.218693285,	0.180580762,	0.167876588,	0.157894737,	0.158802178,  0.161524501)
# pi <- c(0.233212341, 0.256805808,	0.288566243,	0.305807623,	0.27676951,	  0.270417423,	0.229582577,	0.250453721,  0.237749546)
# pa <- c(0.36569873,  0.402903811,	0.461887477,	0.475499093,	0.542649728,	0.561705989,	0.612522686,	0.590744102,  0.600725953)
timeCount <- length(pg)
if( length(pi) != timeCount) stop("The proportions have a different number of time points.")
if( length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))

zg <- qnorm(pg)
zi <- qnorm(pi)
za <- qnorm(pa)

#jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)
jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount, "zg"=zg)#, "zi"=zi, "za"=za)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 3#3 #6
countIterations <- 100#0#00

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
gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation

xyplot(chains) #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed


#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43