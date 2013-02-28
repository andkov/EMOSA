rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
cohortYear <- 1980 #1980, 1981, 1982, 1983, 1984

pathDirectory <- file.path(getwd())
pathResults <- file.path(pathDirectory,"Results")
pathDiffusionIn<-file.path(pathResults,"diffusion.csv")
pathContagionIn<-file.path(pathResults,"contagion.csv")
pathHybridIn<-file.path(pathResults,"hybrid.csv")
pathPrevalences <-file.path(pathResults,"prevalences.csv")
pathTransitions <-file.path(pathResults,"transitions.csv")
pathProportions <-file.path(pathResults,"proportions.csv")

dsD <- read.csv(pathDiffusionIn, stringsAsFactors=FALSE)
dsC <- read.csv(pathContagionIn, stringsAsFactors=FALSE)
dsH <- read.csv(pathHybridIn, stringsAsFactors=FALSE)

dsPs <- read.csv(pathPrevalences, stringsAsFactors=FALSE)
dsTs <- read.csv(pathTransitions, stringsAsFactors=FALSE)
dsPPs <- read.csv(pathProportions, stringsAsFactors=FALSE)

dsD <- merge(dsD, dsPPs, by="cohort")
dsC <- merge(dsC, dsPPs, by="cohort")
dsH <- merge(dsH, dsPPs, by="cohort")

# Step Prediction for diffusion
dsH$g01p<-dsH$g00 + (dsH$Tig * (dsH$i00^(1-dsH$Cig)) * (dsH$g00^dsH$Cig))
                 + (dsH$Tag * (dsH$a00^(1-dsH$Cag)) * (dsH$g00^dsH$Cag))
                 - (dsH$Tgi * (dsH$g00^(1-dsH$Cgi)) * (dsH$i00^dsH$Cgi))
                 - (dsH$Tga * (dsH$g00^(1-dsH$Cga)) * (dsH$a00^dsH$Cga))

dsH$g01 
dsH$g01p
0.174887892  0.292600897	0.532511211		0.216535433	0.306102362	0.477362205		0.226345083	0.304267161	0.469387755		0.26642984	0.292184725	0.441385435		0.340290381
0.174073664  0.271331447	0.554594889		0.216202548	0.296611991	0.487185462		0.219800981	0.293481113	0.486717906		0.262310877	0.273947128	0.463741995		0.24915482
