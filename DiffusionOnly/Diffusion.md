<!-- Specify the report's official name, goal & description. -->
# Diffusion results
**Report Goal**:  Provide a minimalistic report prototype for future reports.
**Report Description**: This is a prototype of a simple report.  It should represent the one side of the spectrum of MIECHV automated reports..

<!-- Point knitr to the underlying code file so it knows where to look for the chunks. -->



### Cohort: 1980
Andrey -write something here.

```r
cohortYear <- 1980
```


```r
require(rjags)
```

```
## Loading required package: rjags
```

```
## Loading required package: coda
```

```
## Loading required package: lattice
```

```
## linking to JAGS 3.3.0
```

```
## module basemod loaded
```

```
## module bugs loaded
```

```r
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags
```

```
## module dic loaded
```

```r


pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')
pathModel <- file.path(pathDirectory, "DiffusionOnly/DiffusionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/Data/SummaryBirthYearByTime.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
```

```
## Error: object 'ds' not found
```

```r
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.
```

```
## Error: object 'ds' not found
```

```r

pg <- ds$ProportionGoers
```

```
## Error: object 'ds' not found
```

```r
pi <- ds$ProportionIrregulars
```

```
## Error: object 'ds' not found
```

```r
pa <- ds$ProportionAbsentees
```

```
## Error: object 'ds' not found
```

```r

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
```

```
## Error: object 'pg' not found
```

```r
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'timeCount' not found
```

```r
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'pa' not found
```

```r
mean(c(pg, pi, pa))
```

```
## Error: object 'pg' not found
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)
```

```
## Error: object 'pg' not found
```

```r

# parameters <- c('mu')
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")
# parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia', 'Kag', 'Kai', 'sumG',
# 'sumI', 'sumA') parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia',
# 'Kag', 'Kai', 'sigmaG', 'sigmaI')
parametersToTrackWithDic <- c("pD", parametersToTrack)
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1),
# Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6  #3 #6
countIterations <- 100  #000

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: object 'jagsModel' not found
```

```r
dic
```

```
## Error: object 'dic' not found
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
```

```
## Error: object 'jagsModel' not found
```

```r
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## Error: object 'chains' not found
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Error: object 'chains' not found
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: object 'chains' not found
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 0.006001 secs
```



### Cohort: 1981

```r
cohortYear <- 1981
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags


pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')
pathModel <- file.path(pathDirectory, "DiffusionOnly/DiffusionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/Data/SummaryBirthYearByTime.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
```

```
## Error: object 'ds' not found
```

```r
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.
```

```
## Error: object 'ds' not found
```

```r

pg <- ds$ProportionGoers
```

```
## Error: object 'ds' not found
```

```r
pi <- ds$ProportionIrregulars
```

```
## Error: object 'ds' not found
```

```r
pa <- ds$ProportionAbsentees
```

```
## Error: object 'ds' not found
```

```r

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
```

```
## Error: object 'pg' not found
```

```r
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'timeCount' not found
```

```r
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'pa' not found
```

```r
mean(c(pg, pi, pa))
```

```
## Error: object 'pg' not found
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)
```

```
## Error: object 'pg' not found
```

```r

# parameters <- c('mu')
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")
# parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia', 'Kag', 'Kai', 'sumG',
# 'sumI', 'sumA') parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia',
# 'Kag', 'Kai', 'sigmaG', 'sigmaI')
parametersToTrackWithDic <- c("pD", parametersToTrack)
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1),
# Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6  #3 #6
countIterations <- 100  #000

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: object 'jagsModel' not found
```

```r
dic
```

```
## Error: object 'dic' not found
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
```

```
## Error: object 'jagsModel' not found
```

```r
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## Error: object 'chains' not found
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Error: object 'chains' not found
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: object 'chains' not found
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 0.007 secs
```


### Cohort: 1982

```r
cohortYear <- 1982
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags


pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')
pathModel <- file.path(pathDirectory, "DiffusionOnly/DiffusionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/Data/SummaryBirthYearByTime.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
```

```
## Error: object 'ds' not found
```

```r
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.
```

```
## Error: object 'ds' not found
```

```r

pg <- ds$ProportionGoers
```

```
## Error: object 'ds' not found
```

```r
pi <- ds$ProportionIrregulars
```

```
## Error: object 'ds' not found
```

```r
pa <- ds$ProportionAbsentees
```

```
## Error: object 'ds' not found
```

```r

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
```

```
## Error: object 'pg' not found
```

```r
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'timeCount' not found
```

```r
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'pa' not found
```

```r
mean(c(pg, pi, pa))
```

```
## Error: object 'pg' not found
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)
```

```
## Error: object 'pg' not found
```

```r

# parameters <- c('mu')
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")
# parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia', 'Kag', 'Kai', 'sumG',
# 'sumI', 'sumA') parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia',
# 'Kag', 'Kai', 'sigmaG', 'sigmaI')
parametersToTrackWithDic <- c("pD", parametersToTrack)
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1),
# Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6  #3 #6
countIterations <- 100  #000

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: object 'jagsModel' not found
```

```r
dic
```

```
## Error: object 'dic' not found
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
```

```
## Error: object 'jagsModel' not found
```

```r
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## Error: object 'chains' not found
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Error: object 'chains' not found
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: object 'chains' not found
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 0.006 secs
```


### Cohort: 1983

```r
cohortYear <- 1983
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags


pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')
pathModel <- file.path(pathDirectory, "DiffusionOnly/DiffusionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/Data/SummaryBirthYearByTime.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
```

```
## Error: object 'ds' not found
```

```r
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.
```

```
## Error: object 'ds' not found
```

```r

pg <- ds$ProportionGoers
```

```
## Error: object 'ds' not found
```

```r
pi <- ds$ProportionIrregulars
```

```
## Error: object 'ds' not found
```

```r
pa <- ds$ProportionAbsentees
```

```
## Error: object 'ds' not found
```

```r

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
```

```
## Error: object 'pg' not found
```

```r
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'timeCount' not found
```

```r
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'pa' not found
```

```r
mean(c(pg, pi, pa))
```

```
## Error: object 'pg' not found
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)
```

```
## Error: object 'pg' not found
```

```r

# parameters <- c('mu')
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")
# parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia', 'Kag', 'Kai', 'sumG',
# 'sumI', 'sumA') parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia',
# 'Kag', 'Kai', 'sigmaG', 'sigmaI')
parametersToTrackWithDic <- c("pD", parametersToTrack)
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1),
# Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6  #3 #6
countIterations <- 100  #000

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: object 'jagsModel' not found
```

```r
dic
```

```
## Error: object 'dic' not found
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
```

```
## Error: object 'jagsModel' not found
```

```r
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## Error: object 'chains' not found
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Error: object 'chains' not found
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: object 'chains' not found
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 0.008001 secs
```



### Cohort: 1984

```r
cohortYear <- 1984
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags


pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')
pathModel <- file.path(pathDirectory, "DiffusionOnly/DiffusionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/Data/SummaryBirthYearByTime.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
```

```
## Error: object 'ds' not found
```

```r
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.
```

```
## Error: object 'ds' not found
```

```r

pg <- ds$ProportionGoers
```

```
## Error: object 'ds' not found
```

```r
pi <- ds$ProportionIrregulars
```

```
## Error: object 'ds' not found
```

```r
pa <- ds$ProportionAbsentees
```

```
## Error: object 'ds' not found
```

```r

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
```

```
## Error: object 'pg' not found
```

```r
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'timeCount' not found
```

```r
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
```

```
## Error: object 'pa' not found
```

```r
mean(c(pg, pi, pa))
```

```
## Error: object 'pg' not found
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)
```

```
## Error: object 'pg' not found
```

```r

# parameters <- c('mu')
parametersToTrack <- c("Kgi", "Kga", "Kig", "Kia", "Kag", "Kai", "sumG", "sumI")
# parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia', 'Kag', 'Kai', 'sumG',
# 'sumI', 'sumA') parametersToTrack <- c('Kgi', 'Kga', 'Kig', 'Kia',
# 'Kag', 'Kai', 'sigmaG', 'sigmaI')
parametersToTrackWithDic <- c("pD", parametersToTrack)
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1),
# Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6  #3 #6
countIterations <- 100  #000

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/DiffusionOnly/DiffusionOnly/DiffusionBeta.bugs"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: object 'jagsModel' not found
```

```r
dic
```

```
## Error: object 'dic' not found
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
```

```
## Error: object 'jagsModel' not found
```

```r
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## Error: object 'chains' not found
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Error: object 'chains' not found
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: object 'chains' not found
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 0.006 secs
```

