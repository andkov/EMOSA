<!-- Specify the report's official name, goal & description. -->
# Hybrid results
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
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags
```

```
## module dic loaded
```

```r

model<-"AdditiveContagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AdditiveContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AdditiveContagionBeta.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AdditiveContagionBeta.bugs")
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
```

```
## [1] 0.3333
```

```r

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "kgi", "kga", "kig", "kia", "kag", "kai",                       
                       "sumG", "sumI"
                       # parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       #                        "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       #                        "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100#000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 364
## 
## Initializing model
```

```r
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
```

```
## Mean deviance:  -91.2 
## penalty 8.94 
## Penalized deviance: -82.2
```

```r
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
## Tag  9.98e-02 3.70e-02  0.00151        0.00638
## Tai  5.10e-01 2.91e-01  0.01189        0.01163
## Tga  2.80e-01 1.24e-01  0.00507        0.00474
## Tgi  1.26e-01 8.34e-02  0.00341        0.01360
## Tia  5.45e-01 1.13e-01  0.00459        0.01233
## Tig  1.31e-01 1.71e-01  0.00697        0.01015
## kag  4.34e-01 9.72e-02  0.00397        0.00991
## kai  6.41e-01 1.11e-01  0.00452        0.01980
## kga  4.66e-01 2.55e-01  0.01040        0.02965
## kgi  4.63e-01 1.91e-01  0.00781        0.01336
## kia  3.63e-01 2.20e-01  0.00898        0.02512
## kig  2.62e-01 1.01e-01  0.00413        0.01423
## sumG 2.01e+03 1.03e+03 41.85457       62.24672
## sumI 7.96e+02 3.80e+02 15.50502       25.21741
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  2.05e-02 7.57e-02 1.02e-01    0.129    0.162
## Tai  2.46e-02 2.50e-01 5.39e-01    0.754    0.972
## Tga  8.66e-02 2.08e-01 2.79e-01    0.298    0.540
## Tgi  2.00e-02 5.93e-02 1.05e-01    0.165    0.356
## Tia  3.52e-01 4.75e-01 5.50e-01    0.627    0.780
## Tig  2.75e-03 2.49e-02 5.84e-02    0.118    0.558
## kag  2.90e-01 3.86e-01 4.11e-01    0.437    0.660
## kai  4.72e-01 5.49e-01 6.38e-01    0.717    0.868
## kga  4.83e-02 1.92e-01 5.00e-01    0.650    0.871
## kgi  1.43e-01 3.25e-01 4.59e-01    0.634    0.759
## kia  3.61e-02 1.81e-01 3.50e-01    0.502    0.910
## kig  9.07e-02 1.90e-01 2.46e-01    0.318    0.480
## sumG 5.22e+02 1.24e+03 1.86e+03 2620.786 4207.381
## sumI 2.42e+02 5.20e+02 7.37e+02 1009.393 1722.865
```

```r
# head(chains, 20)

# windows() # dev.off()
rhat<-gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation
```

```
##    Tag    Tai    Tga    Tgi    Tia    Tig    kag    kai    kga    kgi 
##  22.75 633.04  30.34  20.17  24.74  35.66  26.69  43.44  16.26  25.18 
##    kia    kig   sumG   sumI 
##  28.88  14.61 311.87 228.48
```

```r

xyplot(chains) #Needs at least two parameters; else throws an error.
(density<-densityplot(chains))
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed
```

```
## Time difference of 3.56 secs
```

```r

particularity<-"none"
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
modnum<-paste(model,cohortYear,particularity,"png", sep='.') 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion//figure/AdditiveContagion.1980.none.png'
## for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main="some title")
dev.off()
```

```
## pdf 
##   2
```

```r


fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,"solution.txt"))
sink(fileSink)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion/RawOut/1980solution.txt':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
model
```

```
## [1] "AdditiveContagion"
```

```r
particularity
```

```
## [1] "none"
```

```r
cohortYear
```

```
## [1] 1980
```

```r
dic
```

```
## Mean deviance:  -91.2 
## penalty 8.94 
## Penalized deviance: -82.2
```

```r
condensed
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
## Tag  9.98e-02 3.70e-02  0.00151        0.00638
## Tai  5.10e-01 2.91e-01  0.01189        0.01163
## Tga  2.80e-01 1.24e-01  0.00507        0.00474
## Tgi  1.26e-01 8.34e-02  0.00341        0.01360
## Tia  5.45e-01 1.13e-01  0.00459        0.01233
## Tig  1.31e-01 1.71e-01  0.00697        0.01015
## kag  4.34e-01 9.72e-02  0.00397        0.00991
## kai  6.41e-01 1.11e-01  0.00452        0.01980
## kga  4.66e-01 2.55e-01  0.01040        0.02965
## kgi  4.63e-01 1.91e-01  0.00781        0.01336
## kia  3.63e-01 2.20e-01  0.00898        0.02512
## kig  2.62e-01 1.01e-01  0.00413        0.01423
## sumG 2.01e+03 1.03e+03 41.85457       62.24672
## sumI 7.96e+02 3.80e+02 15.50502       25.21741
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  2.05e-02 7.57e-02 1.02e-01    0.129    0.162
## Tai  2.46e-02 2.50e-01 5.39e-01    0.754    0.972
## Tga  8.66e-02 2.08e-01 2.79e-01    0.298    0.540
## Tgi  2.00e-02 5.93e-02 1.05e-01    0.165    0.356
## Tia  3.52e-01 4.75e-01 5.50e-01    0.627    0.780
## Tig  2.75e-03 2.49e-02 5.84e-02    0.118    0.558
## kag  2.90e-01 3.86e-01 4.11e-01    0.437    0.660
## kai  4.72e-01 5.49e-01 6.38e-01    0.717    0.868
## kga  4.83e-02 1.92e-01 5.00e-01    0.650    0.871
## kgi  1.43e-01 3.25e-01 4.59e-01    0.634    0.759
## kia  3.61e-02 1.81e-01 3.50e-01    0.502    0.910
## kig  9.07e-02 1.90e-01 2.46e-01    0.318    0.480
## sumG 5.22e+02 1.24e+03 1.86e+03 2620.786 4207.381
## sumI 2.42e+02 5.20e+02 7.37e+02 1009.393 1722.865
```

```r
rhat
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Tag        1.64       2.42
## Tai        1.00       1.01
## Tga        9.21      15.91
## Tgi        2.34       3.75
## Tia        3.60       5.94
## Tig        5.00       9.09
## kag        3.82       7.06
## kai        2.13       3.91
## kga        3.16       5.18
## kgi        4.21       7.10
## kia        2.73       5.37
## kig        2.58       4.03
## sumG       1.05       1.11
## sumI       1.12       1.28
## 
## Multivariate psrf
## 
## 10
```

```r
sink()
# THis file to be read and processed by "examining the results of the Addictive Contagion from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AdditiveContagion"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43
```



### Cohort: 1981

```r
cohortYear <- 1981
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

model<-"AdditiveContagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AdditiveContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AdditiveContagionBeta.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AdditiveContagionBeta.bugs")
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
```

```
## [1] 0.3333
```

```r

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "kgi", "kga", "kig", "kia", "kag", "kai",                       
                       "sumG", "sumI"
                       # parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       #                        "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       #                        "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100#000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 367
## 
## Initializing model
```

```r
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
```

```
## Mean deviance:  -90.1 
## penalty 10.2 
## Penalized deviance: -79.9
```

```r
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
## Tag     0.205    0.156  0.00635       6.12e-03
## Tai     0.514    0.289  0.01179       1.14e-02
## Tga     0.162    0.082  0.00335       6.57e-03
## Tgi     0.256    0.165  0.00675       1.32e-02
## Tia     0.554    0.141  0.00576       1.89e-02
## Tig     0.466    0.248  0.01012       1.94e-02
## kag     0.270    0.115  0.00467       1.27e-02
## kai     0.572    0.127  0.00519       2.51e-02
## kga     0.751    0.176  0.00717       1.73e-02
## kgi     0.503    0.267  0.01089       2.06e-02
## kia     0.451    0.312  0.01274       2.37e-02
## kig     0.317    0.233  0.00950       1.61e-02
## sumG 2705.677 1600.898 65.35639       1.22e+02
## sumI  651.238  395.016 16.12645       2.08e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  2.73e-02 8.60e-02    0.146    0.301    0.519
## Tai  3.01e-02 2.51e-01    0.524    0.763    0.988
## Tga  1.18e-02 8.78e-02    0.186    0.228    0.271
## Tgi  2.55e-02 1.11e-01    0.227    0.330    0.590
## Tia  3.56e-01 4.38e-01    0.521    0.645    0.880
## Tig  5.64e-02 2.40e-01    0.492    0.680    0.849
## kag  7.57e-02 1.51e-01    0.287    0.360    0.446
## kai  3.56e-01 4.67e-01    0.575    0.680    0.775
## kga  3.95e-01 5.98e-01    0.778    0.914    0.981
## kgi  1.21e-01 2.53e-01    0.477    0.803    0.907
## kia  8.84e-03 1.52e-01    0.445    0.729    0.962
## kig  7.80e-02 1.72e-01    0.237    0.351    0.894
## sumG 5.15e+02 1.51e+03 2319.856 3636.576 6092.369
## sumI 1.44e+02 3.55e+02  557.867  869.356 1676.952
```

```r
# head(chains, 20)

# windows() # dev.off()
rhat<-gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation
```

```
##    Tag    Tai    Tga    Tgi    Tia    Tig    kag    kai    kga    kgi 
##  24.59 664.94  15.47  32.31  15.46  19.46  17.75  14.09  26.31  22.09 
##    kia    kig   sumG   sumI 
##  28.37  22.56 179.23 334.03
```

```r

xyplot(chains) #Needs at least two parameters; else throws an error.
(density<-densityplot(chains))
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed
```

```
## Time difference of 3.566 secs
```

```r

particularity<-"none"
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
modnum<-paste(model,cohortYear,particularity,"png", sep='.') 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion//figure/AdditiveContagion.1981.none.png'
## for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main="some title")
dev.off()
```

```
## pdf 
##   2
```

```r


fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,"solution.txt"))
sink(fileSink)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion/RawOut/1981solution.txt':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
model
```

```
## [1] "AdditiveContagion"
```

```r
particularity
```

```
## [1] "none"
```

```r
cohortYear
```

```
## [1] 1981
```

```r
dic
```

```
## Mean deviance:  -90.1 
## penalty 10.2 
## Penalized deviance: -79.9
```

```r
condensed
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
## Tag     0.205    0.156  0.00635       6.12e-03
## Tai     0.514    0.289  0.01179       1.14e-02
## Tga     0.162    0.082  0.00335       6.57e-03
## Tgi     0.256    0.165  0.00675       1.32e-02
## Tia     0.554    0.141  0.00576       1.89e-02
## Tig     0.466    0.248  0.01012       1.94e-02
## kag     0.270    0.115  0.00467       1.27e-02
## kai     0.572    0.127  0.00519       2.51e-02
## kga     0.751    0.176  0.00717       1.73e-02
## kgi     0.503    0.267  0.01089       2.06e-02
## kia     0.451    0.312  0.01274       2.37e-02
## kig     0.317    0.233  0.00950       1.61e-02
## sumG 2705.677 1600.898 65.35639       1.22e+02
## sumI  651.238  395.016 16.12645       2.08e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  2.73e-02 8.60e-02    0.146    0.301    0.519
## Tai  3.01e-02 2.51e-01    0.524    0.763    0.988
## Tga  1.18e-02 8.78e-02    0.186    0.228    0.271
## Tgi  2.55e-02 1.11e-01    0.227    0.330    0.590
## Tia  3.56e-01 4.38e-01    0.521    0.645    0.880
## Tig  5.64e-02 2.40e-01    0.492    0.680    0.849
## kag  7.57e-02 1.51e-01    0.287    0.360    0.446
## kai  3.56e-01 4.67e-01    0.575    0.680    0.775
## kga  3.95e-01 5.98e-01    0.778    0.914    0.981
## kgi  1.21e-01 2.53e-01    0.477    0.803    0.907
## kia  8.84e-03 1.52e-01    0.445    0.729    0.962
## kig  7.80e-02 1.72e-01    0.237    0.351    0.894
## sumG 5.15e+02 1.51e+03 2319.856 3636.576 6092.369
## sumI 1.44e+02 3.55e+02  557.867  869.356 1676.952
```

```r
rhat
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Tag       9.113      15.50
## Tai       0.997       1.00
## Tga       4.746       7.75
## Tgi       5.103      11.07
## Tia       3.148       5.60
## Tig       4.864       8.74
## kag       3.876       7.33
## kai       2.030       3.41
## kga       3.455       5.64
## kgi       5.368       9.74
## kia       4.257       7.61
## kig       5.404       9.40
## sumG      1.193       1.47
## sumI      1.381       1.91
## 
## Multivariate psrf
## 
## 14.3
```

```r
sink()
# THis file to be read and processed by "examining the results of the Addictive Contagion from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AdditiveContagion"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43
```


### Cohort: 1982

```r
cohortYear <- 1982
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

model<-"AdditiveContagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AdditiveContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AdditiveContagionBeta.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AdditiveContagionBeta.bugs")
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
```

```
## [1] 0.3333
```

```r

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "kgi", "kga", "kig", "kia", "kag", "kai",                       
                       "sumG", "sumI"
                       # parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       #                        "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       #                        "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100#000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 367
## 
## Initializing model
```

```r
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
```

```
## Mean deviance:  -74.6 
## penalty 8.99 
## Penalized deviance: -65.6
```

```r
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
##         Mean       SD Naive SE Time-series SE
## Tag    0.161   0.1546  0.00631        0.01164
## Tai    0.492   0.2879  0.01176        0.01087
## Tga    0.363   0.1754  0.00716        0.01176
## Tgi    0.110   0.0905  0.00370        0.01472
## Tia    0.468   0.1315  0.00537        0.02312
## Tig    0.329   0.2802  0.01144        0.02228
## kag    0.504   0.1696  0.00693        0.00904
## kai    0.429   0.1048  0.00428        0.01545
## kga    0.456   0.3047  0.01244        0.04520
## kgi    0.567   0.2575  0.01051        0.02871
## kia    0.437   0.2366  0.00966        0.02665
## kig    0.196   0.1641  0.00670        0.02675
## sumG 583.572 304.0854 12.41424       18.44259
## sumI 406.405 335.9221 13.71396       24.23486
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%     75%    97.5%
## Tag  4.20e-03   0.0482   0.1104   0.182    0.493
## Tai  2.83e-02   0.2447   0.4849   0.734    0.973
## Tga  6.08e-02   0.1841   0.4366   0.504    0.587
## Tgi  4.34e-03   0.0347   0.0848   0.157    0.332
## Tia  2.19e-01   0.3580   0.4901   0.583    0.662
## Tig  7.23e-03   0.0852   0.2705   0.501    0.939
## kag  2.63e-01   0.3510   0.4679   0.650    0.804
## kai  2.49e-01   0.3409   0.4405   0.522    0.598
## kga  1.72e-02   0.2219   0.4139   0.652    0.972
## kgi  9.23e-02   0.3897   0.5739   0.794    0.938
## kia  2.85e-02   0.2635   0.4063   0.597    0.940
## kig  7.50e-03   0.0644   0.1547   0.299    0.601
## sumG 1.90e+02 354.1342 518.0862 749.290 1271.224
## sumI 5.91e+01 184.3389 304.9803 513.299 1399.244
```

```r
# head(chains, 20)

# windows() # dev.off()
rhat<-gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation
```

```
##    Tag    Tai    Tga    Tgi    Tia    Tig    kag    kai    kga    kgi 
##  28.18 750.64  29.09  32.44  15.29  29.81  27.53  22.94  34.70  21.63 
##    kia    kig   sumG   sumI 
##  23.10  33.54 285.97 186.85
```

```r

xyplot(chains) #Needs at least two parameters; else throws an error.
(density<-densityplot(chains))
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed
```

```
## Time difference of 3.803 secs
```

```r

particularity<-"none"
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
modnum<-paste(model,cohortYear,particularity,"png", sep='.') 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion//figure/AdditiveContagion.1982.none.png'
## for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main="some title")
dev.off()
```

```
## pdf 
##   2
```

```r


fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,"solution.txt"))
sink(fileSink)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion/RawOut/1982solution.txt':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
model
```

```
## [1] "AdditiveContagion"
```

```r
particularity
```

```
## [1] "none"
```

```r
cohortYear
```

```
## [1] 1982
```

```r
dic
```

```
## Mean deviance:  -74.6 
## penalty 8.99 
## Penalized deviance: -65.6
```

```r
condensed
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
##         Mean       SD Naive SE Time-series SE
## Tag    0.161   0.1546  0.00631        0.01164
## Tai    0.492   0.2879  0.01176        0.01087
## Tga    0.363   0.1754  0.00716        0.01176
## Tgi    0.110   0.0905  0.00370        0.01472
## Tia    0.468   0.1315  0.00537        0.02312
## Tig    0.329   0.2802  0.01144        0.02228
## kag    0.504   0.1696  0.00693        0.00904
## kai    0.429   0.1048  0.00428        0.01545
## kga    0.456   0.3047  0.01244        0.04520
## kgi    0.567   0.2575  0.01051        0.02871
## kia    0.437   0.2366  0.00966        0.02665
## kig    0.196   0.1641  0.00670        0.02675
## sumG 583.572 304.0854 12.41424       18.44259
## sumI 406.405 335.9221 13.71396       24.23486
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%     75%    97.5%
## Tag  4.20e-03   0.0482   0.1104   0.182    0.493
## Tai  2.83e-02   0.2447   0.4849   0.734    0.973
## Tga  6.08e-02   0.1841   0.4366   0.504    0.587
## Tgi  4.34e-03   0.0347   0.0848   0.157    0.332
## Tia  2.19e-01   0.3580   0.4901   0.583    0.662
## Tig  7.23e-03   0.0852   0.2705   0.501    0.939
## kag  2.63e-01   0.3510   0.4679   0.650    0.804
## kai  2.49e-01   0.3409   0.4405   0.522    0.598
## kga  1.72e-02   0.2219   0.4139   0.652    0.972
## kgi  9.23e-02   0.3897   0.5739   0.794    0.938
## kia  2.85e-02   0.2635   0.4063   0.597    0.940
## kig  7.50e-03   0.0644   0.1547   0.299    0.601
## sumG 1.90e+02 354.1342 518.0862 749.290 1271.224
## sumI 5.91e+01 184.3389 304.9803 513.299 1399.244
```

```r
rhat
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Tag        4.78      10.86
## Tai        1.00       1.01
## Tga        5.74      10.52
## Tgi        1.72       2.51
## Tia        2.34       3.87
## Tig        4.09       6.86
## kag        5.50       8.79
## kai        1.72       2.50
## kga        2.72       4.86
## kgi        3.17       5.47
## kia        2.35       3.62
## kig        2.62       4.67
## sumG       1.03       1.08
## sumI       1.60       2.77
## 
## Multivariate psrf
## 
## 7.94
```

```r
sink()
# THis file to be read and processed by "examining the results of the Addictive Contagion from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AdditiveContagion"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43
```


### Cohort: 1983

```r
cohortYear <- 1983
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

model<-"AdditiveContagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AdditiveContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AdditiveContagionBeta.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AdditiveContagionBeta.bugs")
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
```

```
## [1] 0.3333
```

```r

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "kgi", "kga", "kig", "kia", "kag", "kai",                       
                       "sumG", "sumI"
                       # parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       #                        "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       #                        "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100#000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 362
## 
## Initializing model
```

```r
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
```

```
## Mean deviance:  -92.6 
## penalty 6.93 
## Penalized deviance: -85.7
```

```r
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
## Tag  1.78e-01 9.63e-02 3.93e-03       5.08e-03
## Tai  5.08e-01 2.83e-01 1.16e-02       1.15e-02
## Tga  8.90e-02 5.63e-02 2.30e-03       3.69e-03
## Tgi  3.08e-02 2.58e-02 1.05e-03       5.72e-03
## Tia  5.52e-01 2.30e-01 9.39e-03       1.33e-02
## Tig  9.08e-02 4.75e-02 1.94e-03       4.53e-03
## kag  1.30e-01 5.25e-02 2.14e-03       2.34e-03
## kai  5.35e-01 2.21e-01 9.01e-03       1.14e-02
## kga  2.44e-01 1.05e-01 4.27e-03       4.46e-03
## kgi  3.90e-01 1.21e-01 4.95e-03       7.61e-03
## kia  3.25e-01 1.85e-01 7.55e-03       4.21e-02
## kig  4.86e-02 3.54e-02 1.45e-03       8.35e-03
## sumG 7.33e+03 3.96e+03 1.62e+02       2.89e+02
## sumI 2.81e+02 1.66e+02 6.77e+00       1.08e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  3.59e-03 1.47e-01 1.69e-01 2.26e-01 3.76e-01
## Tai  3.64e-02 2.73e-01 5.11e-01 7.44e-01 9.73e-01
## Tga  3.57e-03 3.88e-02 8.41e-02 1.48e-01 1.73e-01
## Tgi  1.67e-03 1.10e-02 2.51e-02 4.41e-02 1.12e-01
## Tia  7.28e-02 4.42e-01 6.20e-01 7.40e-01 8.14e-01
## Tig  4.29e-03 4.97e-02 1.01e-01 1.32e-01 1.65e-01
## kag  6.79e-02 8.66e-02 1.09e-01 1.93e-01 2.18e-01
## kai  1.25e-01 3.95e-01 5.62e-01 7.20e-01 8.91e-01
## kga  9.13e-03 2.00e-01 2.89e-01 3.11e-01 3.77e-01
## kgi  2.15e-01 2.65e-01 3.98e-01 4.98e-01 5.96e-01
## kia  3.44e-02 1.98e-01 2.79e-01 4.39e-01 7.55e-01
## kig  1.84e-03 2.02e-02 3.96e-02 7.19e-02 1.26e-01
## sumG 1.99e+03 4.37e+03 6.56e+03 9.30e+03 1.68e+04
## sumI 7.04e+01 1.59e+02 2.38e+02 3.60e+02 7.12e+02
```

```r
# head(chains, 20)

# windows() # dev.off()
rhat<-gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation
```

```
##    Tag    Tai    Tga    Tgi    Tia    Tig    kag    kai    kga    kgi 
##  30.91 607.72  23.52  26.44  29.43  28.83  29.68  46.33  40.82  24.70 
##    kia    kig   sumG   sumI 
##  20.22  25.78 188.07 223.71
```

```r

xyplot(chains) #Needs at least two parameters; else throws an error.
(density<-densityplot(chains))
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed
```

```
## Time difference of 3.441 secs
```

```r

particularity<-"none"
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
modnum<-paste(model,cohortYear,particularity,"png", sep='.') 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion//figure/AdditiveContagion.1983.none.png'
## for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main="some title")
dev.off()
```

```
## pdf 
##   2
```

```r


fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,"solution.txt"))
sink(fileSink)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion/RawOut/1983solution.txt':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
model
```

```
## [1] "AdditiveContagion"
```

```r
particularity
```

```
## [1] "none"
```

```r
cohortYear
```

```
## [1] 1983
```

```r
dic
```

```
## Mean deviance:  -92.6 
## penalty 6.93 
## Penalized deviance: -85.7
```

```r
condensed
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
## Tag  1.78e-01 9.63e-02 3.93e-03       5.08e-03
## Tai  5.08e-01 2.83e-01 1.16e-02       1.15e-02
## Tga  8.90e-02 5.63e-02 2.30e-03       3.69e-03
## Tgi  3.08e-02 2.58e-02 1.05e-03       5.72e-03
## Tia  5.52e-01 2.30e-01 9.39e-03       1.33e-02
## Tig  9.08e-02 4.75e-02 1.94e-03       4.53e-03
## kag  1.30e-01 5.25e-02 2.14e-03       2.34e-03
## kai  5.35e-01 2.21e-01 9.01e-03       1.14e-02
## kga  2.44e-01 1.05e-01 4.27e-03       4.46e-03
## kgi  3.90e-01 1.21e-01 4.95e-03       7.61e-03
## kia  3.25e-01 1.85e-01 7.55e-03       4.21e-02
## kig  4.86e-02 3.54e-02 1.45e-03       8.35e-03
## sumG 7.33e+03 3.96e+03 1.62e+02       2.89e+02
## sumI 2.81e+02 1.66e+02 6.77e+00       1.08e+01
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  3.59e-03 1.47e-01 1.69e-01 2.26e-01 3.76e-01
## Tai  3.64e-02 2.73e-01 5.11e-01 7.44e-01 9.73e-01
## Tga  3.57e-03 3.88e-02 8.41e-02 1.48e-01 1.73e-01
## Tgi  1.67e-03 1.10e-02 2.51e-02 4.41e-02 1.12e-01
## Tia  7.28e-02 4.42e-01 6.20e-01 7.40e-01 8.14e-01
## Tig  4.29e-03 4.97e-02 1.01e-01 1.32e-01 1.65e-01
## kag  6.79e-02 8.66e-02 1.09e-01 1.93e-01 2.18e-01
## kai  1.25e-01 3.95e-01 5.62e-01 7.20e-01 8.91e-01
## kga  9.13e-03 2.00e-01 2.89e-01 3.11e-01 3.77e-01
## kgi  2.15e-01 2.65e-01 3.98e-01 4.98e-01 5.96e-01
## kia  3.44e-02 1.98e-01 2.79e-01 4.39e-01 7.55e-01
## kig  1.84e-03 2.02e-02 3.96e-02 7.19e-02 1.26e-01
## sumG 1.99e+03 4.37e+03 6.56e+03 9.30e+03 1.68e+04
## sumI 7.04e+01 1.59e+02 2.38e+02 3.60e+02 7.12e+02
```

```r
rhat
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Tag       6.684     12.359
## Tai       0.996      0.997
## Tga       5.774     10.397
## Tgi       1.373      1.989
## Tia       5.447      9.293
## Tig       2.615      4.222
## kag       6.265     10.846
## kai       4.799      7.961
## kga       5.186      9.222
## kgi       4.721      8.040
## kia       1.175      1.416
## kig       1.315      1.769
## sumG      1.173      1.401
## sumI      1.213      1.519
## 
## Multivariate psrf
## 
## 9.31
```

```r
sink()
# THis file to be read and processed by "examining the results of the Addictive Contagion from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AdditiveContagion"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43
```



### Cohort: 1984

```r
cohortYear <- 1984
```


```r
require(rjags)
# require(coda)
rjags::load.module("dic") # load a few useful modules (JAGS is modular in design): https://sites.google.com/site/autocatalysis/bayesian-methods-using-jags

model<-"AdditiveContagion"

pathDirectory <- file.path(getwd())
# pathModel <- file.path(pathDirectory, paste(model),"AdditiveContagionBeta.bugs")
# pathData <- file.path(pathDirectory, "Data/SummaryBirthYearByTime.csv")
# pathOutChains <- file.path(pathDirectory, paste0("Data/ChainsHybrid", cohortYear, ".csv"))

{ #This bracket permits the 'else' clause (because it's located on the top layer of the code.)
  if( basename(getwd()) == "EMOSA" ) {#This clause executes when run from the *.R file.
    pathModel <- file.path(getwd(),paste(model),"AdditiveContagionBeta.bugs")
    pathData <- file.path(getwd(), "Data/SummaryBirthYearByTime.csv")
  }
  else if( basename(getwd()) == paste(model) ) { #This clause executes when run from the *.Rmd/Rnw file.
    pathModel <- file.path(dirname(getwd()), paste(model), "AdditiveContagionBeta.bugs")
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
```

```
## [1] 0.3333
```

```r

jagsData <- list("pg"=pg, "pi"=pi, "pa"=pa, "timeCount"=timeCount)


parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       "kgi", "kga", "kig", "kia", "kag", "kai",                       
                       "sumG", "sumI"
                       # parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", 
                       #                        "Cgi", "Cga", "Cig", "Cia", "Cag", "Cai",                       
                       #                        "sumG", "sumI"
) #For Beta
# parametersToTrack <- c("Tgi", "Tga", "Tig", "Tia", "Tag", "Tai", "sigmaG", "sigmaI") #For Gauss
#parametersToTrackWithDic <- c("pD", "deviance", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
parametersToTrackWithDic <- c("pD", parametersToTrack) #Must first execute 'rjags::load.module("dic")'
# inits <- function(){ list(Kgi=rnorm(1), Kga=rnorm(1), Kig=rnorm(1), Kia=rnorm(1), Kag=rnorm(1), Kai=rnorm(1)) }

countChains <- 6#3 #6
countIterations <- 100#000

startTime <- Sys.time()

jagsModel <- jags.model(file=pathModel, data=jagsData, n.chains=countChains)#, inits=inits)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 364
## 
## Initializing model
```

```r
#print(jagsModel)
#update(jagsModel, 1000) #modifies the original object and returns NULL
dic <- dic.samples(jagsModel, n.iter=countIterations) 
dic
```

```
## Mean deviance:  -77 
## penalty 12 
## Penalized deviance: -65
```

```r
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
## Tag  2.59e-01   0.1357  0.00554        0.01986
## Tai  5.07e-01   0.2921  0.01192        0.01205
## Tga  1.61e-01   0.0878  0.00358        0.00708
## Tgi  1.68e-01   0.0982  0.00401        0.01268
## Tia  6.90e-01   0.1337  0.00546        0.01322
## Tig  1.08e-01   0.1446  0.00591        0.01195
## kag  1.98e-01   0.0748  0.00305        0.00885
## kai  5.83e-01   0.2934  0.01198        0.02781
## kga  2.45e-01   0.1605  0.00655        0.01859
## kgi  2.96e-01   0.2022  0.00825        0.01328
## kia  1.39e-01   0.1059  0.00432        0.01467
## kig  3.34e-02   0.0318  0.00130        0.00592
## sumG 1.15e+03 570.3413 23.28409       35.98213
## sumI 2.60e+02 162.5637  6.63663       11.31106
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  5.10e-03   0.2125 2.78e-01 3.40e-01    0.535
## Tai  2.57e-02   0.2581 5.31e-01 7.55e-01    0.982
## Tga  1.44e-02   0.0875 1.45e-01 2.37e-01    0.306
## Tgi  3.07e-02   0.0889 1.47e-01 2.35e-01    0.382
## Tia  5.25e-01   0.5954 6.40e-01 7.32e-01    0.980
## Tig  1.92e-03   0.0200 4.90e-02 1.03e-01    0.480
## kag  3.26e-02   0.1660 2.06e-01 2.59e-01    0.305
## kai  1.33e-01   0.3364 5.04e-01 9.05e-01    0.993
## kga  7.81e-03   0.0781 2.59e-01 3.60e-01    0.532
## kgi  1.80e-02   0.1266 2.75e-01 4.20e-01    0.717
## kia  5.05e-03   0.0508 1.13e-01 2.05e-01    0.396
## kig  9.65e-04   0.0105 2.33e-02 4.77e-02    0.110
## sumG 3.38e+02 738.3246 1.03e+03 1.45e+03 2435.416
## sumI 4.33e+01 135.1751 2.30e+02 3.53e+02  653.938
```

```r
# head(chains, 20)

# windows() # dev.off()
rhat<-gelman.diag(chains, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
effectiveSize(chains) #Sample size adjusted for autocorrelation
```

```
##    Tag    Tai    Tga    Tgi    Tia    Tig    kag    kai    kga    kgi 
##  40.67 627.64  26.40  28.86  35.56  50.50  24.75  32.56  32.30  27.53 
##    kia    kig   sumG   sumI 
##  60.53  59.71 265.21 180.86
```

```r

xyplot(chains) #Needs at least two parameters; else throws an error.
(density<-densityplot(chains))
# gelman.plot(chains)
# print(rbind(paste("estimated mu: ", condensed$statistics["mu0", "Mean"]),
#             paste("observed mean:", mean(y, na.rm=T))))
# dsChains <- as.data.frame(chains)
# write.csv(chains, pathOutChains, row.names=FALSE)
elapsed
```

```
## Time difference of 3.686 secs
```

```r

particularity<-"none"
pathOutData <- file.path(pathDirectory,paste(model),"/figure") # where to put images
modnum<-paste(model,cohortYear,particularity,"png", sep='.') 
pathFileOut<-file.path(pathOutData,modnum)
png(filename = pathFileOut,
    width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
```

```
## Warning: Unable to open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion//figure/AdditiveContagion.1984.none.png'
## for writing
```

```
## Warning: opening device failed
```

```
## Error: unable to start png() device
```

```r
plot(density, main="some title")
dev.off()
```

```
## pdf 
##   2
```

```r


fileSink<-file.path(pathDirectory,paste(model),"RawOut",paste0(cohortYear,"solution.txt"))
sink(fileSink)
```

```
## Warning: cannot open file
## 'C:/Users/inspirion/Documents/GitHub/EMOSA/AdditiveContagion/AdditiveContagion/RawOut/1984solution.txt':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
model
```

```
## [1] "AdditiveContagion"
```

```r
particularity
```

```
## [1] "none"
```

```r
cohortYear
```

```
## [1] 1984
```

```r
dic
```

```
## Mean deviance:  -77 
## penalty 12 
## Penalized deviance: -65
```

```r
condensed
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
## Tag  2.59e-01   0.1357  0.00554        0.01986
## Tai  5.07e-01   0.2921  0.01192        0.01205
## Tga  1.61e-01   0.0878  0.00358        0.00708
## Tgi  1.68e-01   0.0982  0.00401        0.01268
## Tia  6.90e-01   0.1337  0.00546        0.01322
## Tig  1.08e-01   0.1446  0.00591        0.01195
## kag  1.98e-01   0.0748  0.00305        0.00885
## kai  5.83e-01   0.2934  0.01198        0.02781
## kga  2.45e-01   0.1605  0.00655        0.01859
## kgi  2.96e-01   0.2022  0.00825        0.01328
## kia  1.39e-01   0.1059  0.00432        0.01467
## kig  3.34e-02   0.0318  0.00130        0.00592
## sumG 1.15e+03 570.3413 23.28409       35.98213
## sumI 2.60e+02 162.5637  6.63663       11.31106
## 
## 2. Quantiles for each variable:
## 
##          2.5%      25%      50%      75%    97.5%
## Tag  5.10e-03   0.2125 2.78e-01 3.40e-01    0.535
## Tai  2.57e-02   0.2581 5.31e-01 7.55e-01    0.982
## Tga  1.44e-02   0.0875 1.45e-01 2.37e-01    0.306
## Tgi  3.07e-02   0.0889 1.47e-01 2.35e-01    0.382
## Tia  5.25e-01   0.5954 6.40e-01 7.32e-01    0.980
## Tig  1.92e-03   0.0200 4.90e-02 1.03e-01    0.480
## kag  3.26e-02   0.1660 2.06e-01 2.59e-01    0.305
## kai  1.33e-01   0.3364 5.04e-01 9.05e-01    0.993
## kga  7.81e-03   0.0781 2.59e-01 3.60e-01    0.532
## kgi  1.80e-02   0.1266 2.75e-01 4.20e-01    0.717
## kia  5.05e-03   0.0508 1.13e-01 2.05e-01    0.396
## kig  9.65e-04   0.0105 2.33e-02 4.77e-02    0.110
## sumG 3.38e+02 738.3246 1.03e+03 1.45e+03 2435.416
## sumI 4.33e+01 135.1751 2.30e+02 3.53e+02  653.938
```

```r
rhat
```

```
## Potential scale reduction factors:
## 
##      Point est. Upper C.I.
## Tag        3.09       7.66
## Tai        1.00       1.01
## Tga        4.36       7.80
## Tgi        2.18       3.51
## Tia        3.06       6.12
## Tig        4.04       9.59
## kag        2.64       4.47
## kai        3.78       6.78
## kga        2.58       4.46
## kgi        4.29       7.10
## kia        1.30       1.70
## kig        1.12       1.30
## sumG       1.05       1.11
## sumI       1.23       1.57
## 
## Multivariate psrf
## 
## 6.57
```

```r
sink()
# THis file to be read and processed by "examining the results of the Addictive Contagion from WINBUGS.xlsx"  
# located in the same folder "GitHub/EMOSA/AdditiveContagion"



#Coda & DIC on JAGS: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ea46dc43
```


