<!-- Specify the report's official name, goal & description. -->
# Contagion results
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
## Warning: there is no package called 'rjags'
```

```r



if (Sys.info()["nodename"] == "MICKEY") pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA"
# pathDirectory <-
# 'F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion'
if (Sys.info()["nodename"] == "MERKANEZ-PC") pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy"  #Change this directory location

# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
```

```
## Error: object 'pathDirectory' not found
```

```r
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
```

```
## Error: object 'pathDirectory' not found
```

```r


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Error: object 'pathData' not found
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1e+05

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Error: could not find function "jags.model"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: could not find function "dic.samples"
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
## Error: could not find function "coda.samples"
```

```r
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
## Error: could not find function "gelman.diag"
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: could not find function "effectiveSize"
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: could not find function "xyplot"
```

```r
densityplot(chains)
```

```
## Error: could not find function "densityplot"
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.014 secs
```



### Cohort: 1981

```r
cohortYear <- 1981
a <- 5
sqrt(a)
```

```
## [1] 2.236
```


```r
require(rjags)
```

```
## Loading required package: rjags
```

```
## Warning: there is no package called 'rjags'
```

```r



if (Sys.info()["nodename"] == "MICKEY") pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA"
# pathDirectory <-
# 'F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion'
if (Sys.info()["nodename"] == "MERKANEZ-PC") pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy"  #Change this directory location

# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
```

```
## Error: object 'pathDirectory' not found
```

```r
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
```

```
## Error: object 'pathDirectory' not found
```

```r


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Error: object 'pathData' not found
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1e+05

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Error: could not find function "jags.model"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: could not find function "dic.samples"
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
## Error: could not find function "coda.samples"
```

```r
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
## Error: could not find function "gelman.diag"
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: could not find function "effectiveSize"
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: could not find function "xyplot"
```

```r
densityplot(chains)
```

```
## Error: could not find function "densityplot"
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.014 secs
```


### Cohort: 1982

```r
cohortYear <- 1982
```


```r
require(rjags)
```

```
## Loading required package: rjags
```

```
## Warning: there is no package called 'rjags'
```

```r



if (Sys.info()["nodename"] == "MICKEY") pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA"
# pathDirectory <-
# 'F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion'
if (Sys.info()["nodename"] == "MERKANEZ-PC") pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy"  #Change this directory location

# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
```

```
## Error: object 'pathDirectory' not found
```

```r
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
```

```
## Error: object 'pathDirectory' not found
```

```r


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Error: object 'pathData' not found
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1e+05

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Error: could not find function "jags.model"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: could not find function "dic.samples"
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
## Error: could not find function "coda.samples"
```

```r
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
## Error: could not find function "gelman.diag"
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: could not find function "effectiveSize"
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: could not find function "xyplot"
```

```r
densityplot(chains)
```

```
## Error: could not find function "densityplot"
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.027 secs
```


### Cohort: 1983

```r
cohortYear <- 1983
```


```r
require(rjags)
```

```
## Loading required package: rjags
```

```
## Warning: there is no package called 'rjags'
```

```r



if (Sys.info()["nodename"] == "MICKEY") pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA"
# pathDirectory <-
# 'F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion'
if (Sys.info()["nodename"] == "MERKANEZ-PC") pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy"  #Change this directory location

# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
```

```
## Error: object 'pathDirectory' not found
```

```r
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
```

```
## Error: object 'pathDirectory' not found
```

```r


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Error: object 'pathData' not found
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1e+05

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Error: could not find function "jags.model"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: could not find function "dic.samples"
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
## Error: could not find function "coda.samples"
```

```r
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
## Error: could not find function "gelman.diag"
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: could not find function "effectiveSize"
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: could not find function "xyplot"
```

```r
densityplot(chains)
```

```
## Error: could not find function "densityplot"
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.024 secs
```



### Cohort: 1984

```r
cohortYear <- 1984
```


```r
require(rjags)
```

```
## Loading required package: rjags
```

```
## Warning: there is no package called 'rjags'
```

```r



if (Sys.info()["nodename"] == "MICKEY") pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA"
# pathDirectory <-
# 'F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/OneShot_Only1984Diffusion'
if (Sys.info()["nodename"] == "MERKANEZ-PC") pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy"  #Change this directory location

# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
```

```
## Error: object 'pathDirectory' not found
```

```r
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
```

```
## Error: object 'pathDirectory' not found
```

```r


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Error: object 'pathData' not found
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1e+05

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Error: could not find function "jags.model"
```

```r
# print(jagsModel) update(jagsModel, 1000) #modifies the original object
# and returns NULL
dic <- dic.samples(jagsModel, n.iter = countIterations)
```

```
## Error: could not find function "dic.samples"
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
## Error: could not find function "coda.samples"
```

```r
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
## Error: could not find function "gelman.diag"
```

```r
effectiveSize(chains)  #Sample size adjusted for autocorrelation
```

```
## Error: could not find function "effectiveSize"
```

```r

xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: could not find function "xyplot"
```

```r
densityplot(chains)
```

```
## Error: could not find function "densityplot"
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.015 secs
```


