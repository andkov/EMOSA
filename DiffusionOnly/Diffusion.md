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
## Linked to JAGS 3.3.0
```

```
## Loaded modules: basemod,bugs
```

```r
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags
```

```
## module dic loaded
```

```r

{
    #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
    if (basename(getwd()) == "EMOSA") {
        #This clause executes when run from the *.R file.
        pathModel <- file.path(getwd(), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
    } else if (basename(getwd()) == "DiffusionOnly") {
        #This clause executes when run from the *.Rmd/Rnw file.
        pathModel <- file.path(dirname(getwd()), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")
    } else {
        stop(paste0("The working directory '", basename(getwd()), "' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
    }
}

# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')

# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))
```

```
## [1] 0.3333
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)

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
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 183
## 
## Initializing model
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
dic
```

```
## Mean deviance:  -96.7 
## penalty 8.27 
## Penalized deviance: -88.4
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## 
## Iterations = 1101:1200
## Thinning interval = 1 
## Number of chains = 6 
## Sample size per chain = 100 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##          Mean       SD Naive SE Time-series SE
## Kag  1.30e-01 6.64e-02  0.00271        0.00696
## Kai  9.54e-02 6.34e-02  0.00259        0.00643
## Kga  3.89e-01 2.09e-01  0.00853        0.01649
## Kgi  5.86e-01 2.23e-01  0.00912        0.02035
## Kia  2.40e-01 1.14e-01  0.00466        0.01313
## Kig  3.24e-01 9.14e-02  0.00373        0.01303
## sumG 1.96e+03 1.01e+03 41.12499       61.41710
## sumI 1.72e+03 8.39e+02 34.23734       50.30133
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  6.82e-03 7.92e-02 1.45e-01    0.183    0.239
## Kai  3.92e-03 4.38e-02 9.13e-02    0.132    0.229
## Kga  1.19e-01 2.34e-01 3.03e-01    0.573    0.853
## Kgi  2.78e-01 3.43e-01 5.84e-01    0.776    0.930
## Kia  4.52e-02 1.36e-01 2.48e-01    0.341    0.426
## Kig  2.02e-01 2.59e-01 3.00e-01    0.353    0.541
## sumG 5.14e+02 1.19e+03 1.83e+03 2509.426 4293.170
## sumI 5.44e+02 1.09e+03 1.55e+03 2176.845 3706.417
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        3.43       5.60
## Kai        3.15       5.22
## Kga        3.80       6.49
## Kgi        4.26       7.67
## Kia        2.61       4.32
## Kig        2.84       4.74
## sumG       1.11       1.26
## sumI       1.04       1.10
## 
## Multivariate psrf
## 
## 4.1
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  18.18  34.68  30.58  25.82  29.66  16.94 259.83 285.22
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

![plot of chunk GoDogGo](figure/Cohort1980GoDogGo1.png) 

```r
densityplot(chains)
```

![plot of chunk GoDogGo](figure/Cohort1980GoDogGo2.png) 

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 1.915 secs
```



### Cohort: 1981

```r
cohortYear <- 1981
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

{
    #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
    if (basename(getwd()) == "EMOSA") {
        #This clause executes when run from the *.R file.
        pathModel <- file.path(getwd(), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
    } else if (basename(getwd()) == "DiffusionOnly") {
        #This clause executes when run from the *.Rmd/Rnw file.
        pathModel <- file.path(dirname(getwd()), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")
    } else {
        stop(paste0("The working directory '", basename(getwd()), "' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
    }
}

# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')

# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))
```

```
## [1] 0.3333
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)

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
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 185
## 
## Initializing model
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
dic
```

```
## Mean deviance:  -101 
## penalty 6.74 
## Penalized deviance: -94.6
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## 
## Iterations = 1101:1200
## Thinning interval = 1 
## Number of chains = 6 
## Sample size per chain = 100 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##          Mean       SD Naive SE Time-series SE
## Kag  1.32e-01 2.46e-02  0.00100       4.34e-03
## Kai  4.09e-02 3.16e-02  0.00129       4.84e-03
## Kga  2.83e-01 1.29e-01  0.00525       1.03e-02
## Kgi  3.13e-01 9.09e-02  0.00371       1.48e-02
## Kia  1.59e-01 1.05e-01  0.00428       8.98e-03
## Kig  1.79e-01 8.43e-02  0.00344       1.06e-02
## sumG 3.30e+03 1.58e+03 64.63333       1.02e+02
## sumI 1.88e+03 8.27e+02 33.77644       5.48e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  8.20e-02 1.16e-01 1.33e-01 1.47e-01    0.182
## Kai  2.20e-03 1.65e-02 3.56e-02 5.91e-02    0.134
## Kga  1.38e-02 2.18e-01 2.92e-01 3.87e-01    0.478
## Kgi  1.17e-01 2.79e-01 3.33e-01 3.67e-01    0.470
## Kia  9.86e-03 6.90e-02 1.48e-01 2.30e-01    0.361
## Kig  4.91e-02 9.90e-02 1.72e-01 2.42e-01    0.327
## sumG 1.02e+03 2.15e+03 3.00e+03 4.17e+03 6970.579
## sumI 6.38e+02 1.25e+03 1.73e+03 2.39e+03 3725.059
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.32       1.73
## Kai        1.53       2.16
## Kga        3.40       5.43
## Kgi        2.14       3.85
## Kia        3.03       5.12
## Kig        2.88       4.73
## sumG       1.02       1.05
## sumI       1.03       1.07
## 
## Multivariate psrf
## 
## 3.65
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  26.09  32.28  26.56  32.44  47.48  22.59 342.20 261.93
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

![plot of chunk GoDogGo](figure/Cohort1981GoDogGo1.png) 

```r
densityplot(chains)
```

![plot of chunk GoDogGo](figure/Cohort1981GoDogGo2.png) 

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 1.913 secs
```


### Cohort: 1982

```r
cohortYear <- 1982
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

{
    #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
    if (basename(getwd()) == "EMOSA") {
        #This clause executes when run from the *.R file.
        pathModel <- file.path(getwd(), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
    } else if (basename(getwd()) == "DiffusionOnly") {
        #This clause executes when run from the *.Rmd/Rnw file.
        pathModel <- file.path(dirname(getwd()), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")
    } else {
        stop(paste0("The working directory '", basename(getwd()), "' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
    }
}

# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')

# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))
```

```
## [1] 0.3333
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)

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
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 185
## 
## Initializing model
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
dic
```

```
## Mean deviance:  -93.5 
## penalty 6.56 
## Penalized deviance: -87
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## 
## Iterations = 1101:1200
## Thinning interval = 1 
## Number of chains = 6 
## Sample size per chain = 100 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##          Mean       SD Naive SE Time-series SE
## Kag  8.95e-02 4.20e-02 1.71e-03        0.00508
## Kai  3.99e-02 2.31e-02 9.43e-04        0.00489
## Kga  1.69e-01 1.04e-01 4.26e-03        0.02251
## Kgi  6.53e-01 1.18e-01 4.82e-03        0.01481
## Kia  1.86e-01 1.11e-01 4.51e-03        0.00829
## Kig  4.35e-01 1.57e-01 6.40e-03        0.00904
## sumG 7.63e+02 3.63e+02 1.48e+01       23.16124
## sumI 2.88e+03 1.35e+03 5.53e+01       90.13211
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  1.33e-02 5.60e-02 9.28e-02 1.23e-01 1.61e-01
## Kai  3.09e-03 2.05e-02 3.92e-02 5.47e-02 9.44e-02
## Kga  1.38e-02 8.02e-02 1.57e-01 2.47e-01 3.99e-01
## Kgi  4.57e-01 5.60e-01 6.29e-01 7.13e-01 9.18e-01
## Kia  3.85e-02 1.10e-01 1.64e-01 2.14e-01 4.29e-01
## Kig  1.33e-01 3.73e-01 4.33e-01 5.63e-01 6.75e-01
## sumG 2.51e+02 5.02e+02 6.82e+02 9.58e+02 1.68e+03
## sumI 8.91e+02 1.88e+03 2.67e+03 3.66e+03 5.94e+03
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.82       2.71
## Kai        1.14       1.33
## Kga        1.30       1.74
## Kgi        2.73       4.33
## Kia        3.69       5.89
## Kig        4.97       8.60
## sumG       1.02       1.06
## sumI       1.05       1.12
## 
## Multivariate psrf
## 
## 4.86
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  45.31  20.85  26.60  21.73  32.04  28.25 260.13 247.40
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

![plot of chunk GoDogGo](figure/Cohort1982GoDogGo1.png) 

```r
densityplot(chains)
```

![plot of chunk GoDogGo](figure/Cohort1982GoDogGo2.png) 

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 1.953 secs
```


### Cohort: 1983

```r
cohortYear <- 1983
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

{
    #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
    if (basename(getwd()) == "EMOSA") {
        #This clause executes when run from the *.R file.
        pathModel <- file.path(getwd(), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
    } else if (basename(getwd()) == "DiffusionOnly") {
        #This clause executes when run from the *.Rmd/Rnw file.
        pathModel <- file.path(dirname(getwd()), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")
    } else {
        stop(paste0("The working directory '", basename(getwd()), "' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
    }
}

# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')

# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))
```

```
## [1] 0.3333
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)

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
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 183
## 
## Initializing model
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
dic
```

```
## Mean deviance:  -107 
## penalty 7.57 
## Penalized deviance: -99
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## 
## Iterations = 1101:1200
## Thinning interval = 1 
## Number of chains = 6 
## Sample size per chain = 100 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##          Mean       SD Naive SE Time-series SE
## Kag  7.71e-02 1.48e-02 6.05e-04       1.61e-03
## Kai  1.01e-01 6.13e-02 2.50e-03       1.19e-02
## Kga  8.43e-02 5.46e-02 2.23e-03       6.95e-03
## Kgi  2.87e-01 6.09e-02 2.49e-03       9.18e-03
## Kia  3.66e-01 1.13e-01 4.63e-03       2.15e-02
## Kig  7.68e-02 3.75e-02 1.53e-03       5.07e-03
## sumG 1.05e+04 4.96e+03 2.03e+02       3.47e+02
## sumI 9.18e+02 4.55e+02 1.86e+01       3.26e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  5.07e-02 6.75e-02 7.72e-02 8.43e-02 1.07e-01
## Kai  7.40e-03 5.30e-02 9.28e-02 1.47e-01 2.31e-01
## Kga  3.67e-03 4.19e-02 7.54e-02 1.21e-01 1.91e-01
## Kgi  1.54e-01 2.46e-01 2.97e-01 3.32e-01 3.83e-01
## Kia  1.76e-01 2.70e-01 3.63e-01 4.58e-01 5.64e-01
## Kig  6.07e-03 5.63e-02 7.33e-02 1.04e-01 1.45e-01
## sumG 3.25e+03 6.68e+03 9.66e+03 1.33e+04 2.20e+04
## sumI 2.91e+02 5.72e+02 8.30e+02 1.18e+03 1.95e+03
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        2.20       3.40
## Kai        1.47       2.07
## Kga        2.62       4.33
## Kgi        2.30       3.61
## Kia        1.31       1.73
## Kig        2.27       3.88
## sumG       1.06       1.15
## sumI       1.08       1.19
## 
## Multivariate psrf
## 
## 2.56
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  39.13  23.60  20.91  17.44  21.67  28.90 204.43 440.75
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

![plot of chunk GoDogGo](figure/Cohort1983GoDogGo1.png) 

```r
densityplot(chains)
```

![plot of chunk GoDogGo](figure/Cohort1983GoDogGo2.png) 

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 1.874 secs
```



### Cohort: 1984

```r
cohortYear <- 1984
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic")  # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

{
    #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
    if (basename(getwd()) == "EMOSA") {
        #This clause executes when run from the *.R file.
        pathModel <- file.path(getwd(), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
    } else if (basename(getwd()) == "DiffusionOnly") {
        #This clause executes when run from the *.Rmd/Rnw file.
        pathModel <- file.path(dirname(getwd()), "DiffusionOnly/DiffusionBeta.bugs")
        pathData <- file.path(dirname(getwd()), "Data/SummaryBirthYearByTime.csv")
    } else {
        stop(paste0("The working directory '", basename(getwd()), "' was not anticiapted.  If appropriate, please go near the top of the 'OchaReport1.R' code and add this new location."))
    }
}

# pathModel <- file.path(pathDirectory,
# 'DiffusionOnly/DiffusionGauss.bugs') pathModel <-
# file.path(pathDirectory, 'DiffusionOnly/DiffusionLogit.bugs')

# curve(dbeta(x, 1,1)) curve(dbeta(x, 10,10)) curve(dlogis(x, location =
# .25, scale = 1), xlim=c(-5, 5))


ds <- read.csv(pathData, stringsAsFactors = FALSE)
ds <- ds[ds$byear == cohortYear, ]  #Select only the desired cohort
ds <- ds[order(ds$time), ]  #Sort, just, to make sure values will be passed to JAGS in the correct order.

pg <- ds$ProportionGoers
pi <- ds$ProportionIrregulars
pa <- ds$ProportionAbsentees

# Proportion of Goers, of Irregulars, or Nongoers (or absentees) {Check
# these with data; I may have messed up the order} For the 1984 cohort pg
# <- c(0.401088929, 0.340290381, 0.249546279, 0.218693285, 0.180580762,
# 0.167876588, 0.157894737, 0.158802178, 0.161524501) pi <- c(0.233212341,
# 0.256805808, 0.288566243, 0.305807623, 0.27676951, 0.270417423,
# 0.229582577, 0.250453721, 0.237749546) pa <- c(0.36569873, 0.402903811,
# 0.461887477, 0.475499093, 0.542649728, 0.561705989, 0.612522686,
# 0.590744102, 0.600725953)
timeCount <- length(pg)
if (length(pi) != timeCount) stop("The proportions have a different number of time points.")
if (length(pa) != timeCount) stop("The proportions have a different number of time points.")
mean(c(pg, pi, pa))
```

```
## [1] 0.3333
```

```r

jagsData <- list(pg = pg, pi = pi, pa = pa, timeCount = timeCount)

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
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 185
## 
## Initializing model
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
dic
```

```
## Mean deviance:  -83.9 
## penalty 7.34 
## Penalized deviance: -76.6
```

```r
# mcarray <- jags.samples(model=jagsModel, c('mu'),
# n.iter=countIterations) #If I understand correctly, the following line
# is similar, but better
chains <- coda.samples(jagsModel, variable.names = parametersToTrack, n.iter = countIterations)  # updates the model, and coerces the output to a single mcmc.list object.
# chains <- coda.samples(jagsModel,
# variable.names=parametersToTrackWithDic, n.iter=countIterations)#
# updates the model, and coerces the output to a single mcmc.list object.
elapsed <- Sys.time() - startTime
(condensed <- summary(chains))
```

```
## 
## Iterations = 1101:1200
## Thinning interval = 1 
## Number of chains = 6 
## Sample size per chain = 100 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##          Mean       SD Naive SE Time-series SE
## Kag  4.76e-02   0.0216 8.82e-04        0.00311
## Kai  6.38e-02   0.0397 1.62e-03        0.00661
## Kga  9.14e-02   0.0565 2.31e-03        0.00557
## Kgi  2.09e-01   0.0527 2.15e-03        0.00656
## Kia  2.41e-01   0.0885 3.61e-03        0.01210
## Kig  6.17e-02   0.0477 1.95e-03        0.00646
## sumG 1.03e+03 480.9211 1.96e+01       29.87578
## sumI 6.80e+02 307.8533 1.26e+01       18.27033
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  5.30e-03   0.0335   0.0484 6.30e-02    0.092
## Kai  3.48e-03   0.0329   0.0600 9.04e-02    0.150
## Kga  4.64e-03   0.0451   0.0868 1.29e-01    0.207
## Kgi  1.04e-01   0.1760   0.2089 2.49e-01    0.308
## Kia  8.14e-02   0.1822   0.2373 2.92e-01    0.434
## Kig  2.61e-03   0.0243   0.0479 9.21e-02    0.174
## sumG 2.99e+02 644.8013 956.6461 1.36e+03 2083.150
## sumI 2.07e+02 458.3689 631.4558 8.69e+02 1414.697
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.19       1.46
## Kai        1.19       1.45
## Kga        1.51       2.12
## Kgi        1.50       2.07
## Kia        1.35       1.79
## Kig        1.47       2.00
## sumG       1.06       1.14
## sumI       1.02       1.05
## 
## Multivariate psrf
## 
## 1.54
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  77.19  36.40  83.93  48.27  37.66  48.54 276.47 295.45
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

![plot of chunk GoDogGo](figure/Cohort1984GoDogGo1.png) 

```r
densityplot(chains)
```

![plot of chunk GoDogGo](figure/Cohort1984GoDogGo2.png) 

```r
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 1.984 secs
```

