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

model <- "Contagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/Data/SummaryBirthYearByTime.csv':
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1000  #00

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs"
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

xyplot <- xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
density <- densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.007 secs
```

```r

pathOutData <- file.path(pathDirectory, "ContagionOnly/figure")  # where to put images
modnum <- paste(model, as.character(cohortYear), ".png")
pathFileOut <- file.path(pathOutData, modnum)
png(filename = pathFileOut, width = 912, height = 960, units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/figure/Contagion
## 1980 .png' for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main = "some title")
```

```
## Error: 'expr' did not evaluate to an object of length 'n'
```

```r
dev.off()
```

```
## pdf 
##   2
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

model <- "Contagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/Data/SummaryBirthYearByTime.csv':
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1000  #00

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs"
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

xyplot <- xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
density <- densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.006 secs
```

```r

pathOutData <- file.path(pathDirectory, "ContagionOnly/figure")  # where to put images
modnum <- paste(model, as.character(cohortYear), ".png")
pathFileOut <- file.path(pathOutData, modnum)
png(filename = pathFileOut, width = 912, height = 960, units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/figure/Contagion
## 1981 .png' for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main = "some title")
```

```
## Error: 'expr' did not evaluate to an object of length 'n'
```

```r
dev.off()
```

```
## pdf 
##   2
```


### Cohort: 1982

```r
cohortYear <- 1982
```


```r
require(rjags)

model <- "Contagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/Data/SummaryBirthYearByTime.csv':
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1000  #00

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs"
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

xyplot <- xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
density <- densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.006 secs
```

```r

pathOutData <- file.path(pathDirectory, "ContagionOnly/figure")  # where to put images
modnum <- paste(model, as.character(cohortYear), ".png")
pathFileOut <- file.path(pathOutData, modnum)
png(filename = pathFileOut, width = 912, height = 960, units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/figure/Contagion
## 1982 .png' for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main = "some title")
```

```
## Error: 'expr' did not evaluate to an object of length 'n'
```

```r
dev.off()
```

```
## pdf 
##   2
```


### Cohort: 1983

```r
cohortYear <- 1983
```


```r
require(rjags)

model <- "Contagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/Data/SummaryBirthYearByTime.csv':
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1000  #00

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs"
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

xyplot <- xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
density <- densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.006001 secs
```

```r

pathOutData <- file.path(pathDirectory, "ContagionOnly/figure")  # where to put images
modnum <- paste(model, as.character(cohortYear), ".png")
pathFileOut <- file.path(pathOutData, modnum)
png(filename = pathFileOut, width = 912, height = 960, units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/figure/Contagion
## 1983 .png' for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main = "some title")
```

```
## Error: 'expr' did not evaluate to an object of length 'n'
```

```r
dev.off()
```

```
## pdf 
##   2
```



### Cohort: 1984

```r
cohortYear <- 1984
```


```r
require(rjags)

model <- "Contagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory,
# 'ContagionOnly/ContagionGauss.bugs')
pathModel <- file.path(pathDirectory, "ContagionOnly/ContagionBeta.bugs")
pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")


ds <- read.csv(pathData, stringsAsFactors = FALSE)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/Data/SummaryBirthYearByTime.csv':
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

parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sumG", "sumI")  #For Beta
# parametersToTrack <- c('Tgi', 'Tga', 'Tig', 'Tia', 'Tag', 'Tai',
# 'sigmaG', 'sigmaI') #For Gauss

countChains <- 6  #3 #6
countIterations <- 1000  #00

startTime <- Sys.time()

jagsModel <- jags.model(file = pathModel, data = jagsData, n.chains = countChains)  #, inits=inits)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs':
## No such file or directory
```

```
## Error: Cannot open model file
## "C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/ContagionBeta.bugs"
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

xyplot <- xyplot(chains)  #Needs at least two parameters; else throws an error.
```

```
## Error: object 'chains' not found
```

```r
density <- densityplot(chains)
```

```
## Error: object 'chains' not found
```

```r
# gelman.plot(chains)
elapsed
```

```
## Time difference of 0.007 secs
```

```r

pathOutData <- file.path(pathDirectory, "ContagionOnly/figure")  # where to put images
modnum <- paste(model, as.character(cohortYear), ".png")
pathFileOut <- file.path(pathOutData, modnum)
png(filename = pathFileOut, width = 912, height = 960, units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/ContagionOnly/ContagionOnly/figure/Contagion
## 1984 .png' for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main = "some title")
```

```
## Error: 'expr' did not evaluate to an object of length 'n'
```

```r
dev.off()
```

```
## pdf 
##   2
```


