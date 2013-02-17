rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
cohortYear <- 1980 #1980, 1981, 1982, 1983, 1984

## @knitr GoDogGo
require(rjags)

model<-"Contagion"

pathDirectory <-file.path(getwd()) 
# pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionGauss.bugs")
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


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

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI") #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss

countChains <- 6#3 #6
countIterations <- 1000#00

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
#mcarray <- jags.samples(model=jagsModel, c('mu'), n.iter=countIterations) #If I understand correctly, the following line is similar, but better
chains <- coda.samples(jagsModel, variable.names=parametersToTrack, n.iter=countIterations)# updates the model, and coerces the output to a single mcmc.list object. 
elapsed  <- Sys.time() - startTime
(condensed <- summary(chains))

# windows() # dev.off()
gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation

xyplot<-xyplot(chains) #Needs at least two parameters; else throws an error.
density<-densityplot(chains)
# gelman.plot(chains)
elapsed

particularity<-"both removed"
pathOutData <- file.path(pathDirectory,"ContagionOnly/figure") # where to put images
modnum<-paste(model,as.character(cohortYear),particularity,".png") 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
plot(density, main="some title")
dev.off()
