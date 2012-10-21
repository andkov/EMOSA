rm(list=ls(all=TRUE))
require(rjags)



if( Sys.info()["nodename"] == "MICKEY" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EmosaFork/EMOSA/DiffusionOnly"
  #pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion"
if( Sys.info()["nodename"] == "MERKANEZ-PC" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy" #Change this directory location

# pathModel <- file.path(pathDirectory, "DiffusionGauss.bugs")
# pathModel <- file.path(pathDirectory, "DiffusionLogit.bugs")
pathModel <- file.path(pathDirectory, "DiffusionBeta.bugs")
# curve(dbeta(x, .1,.1))
# curve(dbeta(x, 1,1))
# curve(dbeta(x, 10,10))
# curve(dlogis(x, location = .25, scale = 1), xlim=c(-5, 5))
# curve(dlogis(x, location = 1.25, scale = 1), xlim=c(-5, 5), add=T)
# curve(dlogis(x, location = .25, scale = 10), xlim=c(-5, 5))
# 
# curve(plogis(x, location = .25, scale = 1), xlim=c(-5, 5))
# curve(qlogis(x, location = .25, scale = 1), xlim=c(0, 1))

#Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check these with data; I may have messed up the order}
#For the 1984 cohort
pg <- c(0.401088929, 0.340290381,	0.249546279,	0.218693285,	0.180580762,	0.167876588,	0.157894737,	0.158802178,  0.161524501)
pi <- c(0.233212341, 0.256805808,	0.288566243,	0.305807623,	0.27676951,	  0.270417423,	0.229582577,	0.250453721,  0.237749546)
pa <- c(0.36569873,  0.402903811,	0.461887477,	0.475499093,	0.542649728,	0.561705989,	0.612522686,	0.590744102,  0.600725953)
timeCount <- length(pg)
if( length(pi) != timeCount) stop("The proportions have a different number of time points.")
if( length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)

# parameters <- c("mu")
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")#, "sumA")
# parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI", "sumA")
#parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sigmaG", "sigmaI")
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 3 #6
countIterations <- 10000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
# dic <- dic.samples(jagsModel, n.iter=countIterations) 
#mcarray <- jags.samples(model=jagsModel, c('mu'), n.iter=countIterations) #If I understand correctly, the following line is similar, but better
chains <- coda.samples(jagsModel, variable.names=parametersToTrack, n.iter=countIterations)# updates the model, and coerces the output to a single mcmc.list object. 
elapsed  <- Sys.time() - startTime
(condensed <- summary(chains))

# windows()
gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation

xyplot(chains) #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
elapsed