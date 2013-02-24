<!-- Specify the report's official name, goal & description. -->
# Diffusion results
**Report Goal**:  Provide a minimalistic report prototype for future reports.
**Report Description**: This is a prototype of a simple report.  It should represent the one side of the spectrum of MIECHV automated reports..

<!-- Point knitr to the underlying code file so it knows where to look for the chunks. -->



### Cohort: 1980
Additive Contagion

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
## Mean deviance:  -97.8 
## penalty 7.81 
## Penalized deviance: -90
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
## Kag  1.55e-01   0.0593  0.00242        0.00523
## Kai  8.56e-02   0.0414  0.00169        0.00592
## Kga  4.44e-01   0.3661  0.01495        0.01284
## Kgi  5.53e-01   0.2264  0.00924        0.01560
## Kia  2.39e-01   0.1319  0.00539        0.01229
## Kig  2.86e-01   0.0937  0.00383        0.00581
## sumG 2.01e+03 967.9528 39.51651       61.13011
## sumI 1.79e+03 857.0551 34.98913       54.06009
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  4.47e-02 1.20e-01 1.52e-01    0.207    0.249
## Kai  8.35e-03 5.38e-02 9.26e-02    0.115    0.165
## Kga  7.34e-03 1.13e-01 2.92e-01    0.854    0.995
## Kgi  1.11e-01 4.29e-01 6.11e-01    0.747    0.873
## Kia  1.58e-02 1.24e-01 2.43e-01    0.345    0.463
## Kig  1.27e-01 2.19e-01 2.51e-01    0.379    0.457
## sumG 5.17e+02 1.30e+03 1.88e+03 2616.541 4133.040
## sumI 5.16e+02 1.18e+03 1.65e+03 2221.780 3941.310
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        3.52       7.20
## Kai        1.77       2.69
## Kga        7.31      12.86
## Kgi        4.85       7.88
## Kia        3.14       5.13
## Kig        4.38       7.61
## sumG       1.01       1.02
## sumI       1.13       1.32
## 
## Multivariate psrf
## 
## 6.76
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  34.31  28.71  38.03  29.32  27.58  34.69 323.29 293.06
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 2.2 secs
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
## Mean deviance:  -100 
## penalty 7.75 
## Penalized deviance: -92.4
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
## Kag  1.24e-01 3.40e-02  0.00139       4.99e-03
## Kai  4.51e-02 3.54e-02  0.00145       5.70e-03
## Kga  2.34e-01 1.56e-01  0.00639       1.49e-02
## Kgi  3.08e-01 1.12e-01  0.00456       9.83e-03
## Kia  1.85e-01 1.25e-01  0.00511       1.13e-02
## Kig  1.54e-01 3.87e-02  0.00158       9.20e-03
## sumG 3.44e+03 1.61e+03 65.77279       1.14e+02
## sumI 1.93e+03 9.09e+02 37.12613       5.45e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  6.53e-02 1.02e-01 1.20e-01 1.46e-01    0.198
## Kai  2.35e-03 1.75e-02 3.57e-02 6.77e-02    0.125
## Kga  6.32e-03 8.25e-02 2.19e-01 3.72e-01    0.514
## Kgi  1.56e-01 2.11e-01 2.98e-01 3.89e-01    0.521
## Kia  2.15e-02 8.84e-02 1.47e-01 2.92e-01    0.453
## Kig  6.12e-02 1.35e-01 1.59e-01 1.80e-01    0.216
## sumG 1.07e+03 2.23e+03 3.17e+03 4.30e+03 7250.378
## sumI 5.49e+02 1.27e+03 1.82e+03 2.45e+03 4083.620
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.97       3.06
## Kai        1.51       2.21
## Kga        3.70       5.96
## Kgi        3.92       6.31
## Kia        3.21       5.45
## Kig        1.23       1.60
## sumG       1.04       1.11
## sumI       1.02       1.04
## 
## Multivariate psrf
## 
## 3.9
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  21.30  41.48  23.54  21.63  34.17  22.19 216.05 278.35
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 2.143 secs
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
## Mean deviance:  -93.1 
## penalty 6.67 
## Penalized deviance: -86.4
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
## Kag  1.00e-01 4.02e-02  0.00164        0.00836
## Kai  4.79e-02 2.61e-02  0.00107        0.00437
## Kga  1.64e-01 9.69e-02  0.00396        0.02351
## Kgi  6.29e-01 1.13e-01  0.00462        0.02140
## Kia  2.27e-01 7.61e-02  0.00311        0.01844
## Kig  3.91e-01 1.10e-01  0.00449        0.01849
## sumG 7.48e+02 3.79e+02 15.48729       23.82965
## sumI 2.85e+03 1.25e+03 51.06904       58.87807
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  4.10e-02 7.03e-02 9.49e-02 1.25e-01 1.95e-01
## Kai  4.33e-03 2.71e-02 4.94e-02 6.67e-02 9.93e-02
## Kga  8.94e-03 8.19e-02 1.60e-01 2.27e-01 3.82e-01
## Kgi  4.48e-01 5.34e-01 6.22e-01 7.02e-01 8.70e-01
## Kia  7.71e-02 1.69e-01 2.35e-01 2.76e-01 3.77e-01
## Kig  1.62e-01 3.01e-01 3.83e-01 4.91e-01 5.65e-01
## sumG 2.18e+02 4.58e+02 6.95e+02 9.61e+02 1.69e+03
## sumI 8.30e+02 1.85e+03 2.78e+03 3.67e+03 5.63e+03
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.21       1.55
## Kai        1.54       2.33
## Kga        1.21       1.56
## Kgi        2.16       3.49
## Kia        1.25       1.57
## Kig        2.48       4.09
## sumG       1.02       1.05
## sumI       1.01       1.02
## 
## Multivariate psrf
## 
## 2.42
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  48.28  40.74  29.62  18.60  14.74  16.06 280.00 486.45
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 2.18 secs
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
## Mean deviance:  -105 
## penalty 9.01 
## Penalized deviance: -96
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
## Kag  7.59e-02 1.84e-02 7.53e-04       1.84e-03
## Kai  8.05e-02 5.64e-02 2.30e-03       7.24e-03
## Kga  1.11e-01 8.82e-02 3.60e-03       8.27e-03
## Kgi  2.60e-01 9.21e-02 3.76e-03       7.73e-03
## Kia  3.00e-01 1.48e-01 6.06e-03       1.46e-02
## Kig  7.90e-02 4.10e-02 1.67e-03       5.16e-03
## sumG 1.06e+04 5.42e+03 2.21e+02       4.03e+02
## sumI 9.80e+02 5.14e+02 2.10e+01       3.48e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  3.70e-02 6.72e-02 7.77e-02 8.53e-02 1.11e-01
## Kai  3.66e-03 3.09e-02 6.41e-02 1.31e-01 1.95e-01
## Kga  1.10e-02 4.99e-02 8.25e-02 1.31e-01 3.49e-01
## Kgi  2.92e-02 2.25e-01 2.92e-01 3.26e-01 3.65e-01
## Kia  3.08e-02 1.88e-01 3.04e-01 4.13e-01 5.64e-01
## Kig  8.20e-03 5.72e-02 7.54e-02 9.48e-02 1.62e-01
## sumG 3.39e+03 6.69e+03 9.62e+03 1.37e+04 2.38e+04
## sumI 2.70e+02 6.05e+02 8.88e+02 1.26e+03 2.19e+03
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        2.51       3.93
## Kai        2.18       3.71
## Kga        3.59       7.90
## Kgi        4.13       7.07
## Kia        2.73       4.54
## Kig        2.66       4.39
## sumG       1.06       1.15
## sumI       1.14       1.34
## 
## Multivariate psrf
## 
## 4.66
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  33.60  30.97  25.40  28.61  32.49  29.37 168.95 277.71
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 2.104 secs
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
## Mean deviance:  -83.3 
## penalty 7.61 
## Penalized deviance: -75.7
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
## Kag  5.64e-02   0.0258  0.00105        0.00509
## Kai  9.65e-02   0.0481  0.00197        0.00787
## Kga  1.07e-01   0.0651  0.00266        0.01185
## Kgi  2.02e-01   0.0602  0.00246        0.00940
## Kia  3.08e-01   0.1149  0.00469        0.02343
## Kig  5.25e-02   0.0493  0.00201        0.00825
## sumG 1.01e+03 617.5344 25.21074       59.69117
## sumI 6.10e+02 300.0286 12.24862       19.31136
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Kag  4.87e-03   0.0381   0.0602 7.36e-02    0.105
## Kai  9.38e-03   0.0595   0.0980 1.29e-01    0.196
## Kga  9.03e-03   0.0579   0.0979 1.52e-01    0.257
## Kgi  8.58e-02   0.1559   0.2038 2.48e-01    0.301
## Kia  1.10e-01   0.2155   0.3043 4.00e-01    0.519
## Kig  1.65e-03   0.0150   0.0380 7.62e-02    0.197
## sumG 2.65e+02 578.6397 877.8486 1.28e+03 2516.116
## sumI 1.86e+02 383.5549 556.2687 7.78e+02 1324.366
```

```r

# windows() # dev.off()
gelman.diag(chains, autoburnin = FALSE)  #This is R-hat; the burnin period is manually specified above, so turn off the auto argument.
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Kag        1.06       1.13
## Kai        1.30       1.65
## Kga        1.09       1.23
## Kgi        1.26       1.58
## Kia        1.37       1.82
## Kig        1.44       2.11
## sumG       1.13       1.30
## sumI       1.04       1.09
## 
## Multivariate psrf
## 
## 1.52
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
##    Kag    Kai    Kga    Kgi    Kia    Kig   sumG   sumI 
##  35.67  28.57  36.35  35.67  26.79  46.51 175.61 286.61
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
densityplot(chains)
# gelman.plot(chains) print(rbind(paste('estimated mu: ',
# condensed$statistics['mu0', 'Mean']), paste('observed mean:', mean(y,
# na.rm=T))))
elapsed
```

```
## Time difference of 2.242 secs
```

