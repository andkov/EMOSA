rm(list=ls(all=TRUE)) #Clear out variables from previous runs.
# update.packages("ask=F")
require(colorspace)
require(lme4)
require(Matrix)
require(lattice)
require(car)
require(MASS)
require(nnet)
require(ggplot2)
require(plyr)
require(reshape2)
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
# NOTE: you must have  "NLSY-97_Religiosity" repository in your local GitHub folder


####### Section 1 ---------------------Read, clean, melt, cast-------------------begin-----------------
pathGitHub <- file.path("C:/Users/inspirion/Documents/GitHub") # locate the "GitHub" folder on your computer
# Read the source file from NLS Web Investigator and prepare the parent dataset "dsSource"
#                           GitHub          Repository               NN Script
pathReadClean <- file.path(pathGitHub,"NLSY-97_Religiosity","01_reading_and_cleaning.R")
source(pathReadClean)
rm(list=setdiff(ls(), c("dsSource","pathGitHub")))

# Select desired groups of (timeseries) variables from dsSource into wide datasets 
# melt  wides into longs, summarize longs, widen summaries
# One pair of wide and long forms timeseries variable (dsWattendResp) and (dsLcatatrans)

pathMeltCast <- file.path(pathGitHub,"NLSY-97_Religiosity","02_melting_and_casting.R")
source(pathMeltCast)

# DATASET NAMES
#   dsSource      - the clean and processed NLSY file from the source (08032013 = MMDDYYYY)
#
#   dsWattendResp  -d(ata)s(et)W(ide that included)attend(ence of the original)Resp(ponse to the quiestionnaire item
#                   "In the past year, how frequently have you attended worship service?")
#                   subset of variables in dsSource: (variables/attend.png) years 2000-2010
#   dsWcatatrans  - d(ata)s(et)W(ide that includ)cat(egories of) a(ttendance and)trans(itions between these categories)
#                   subset(dsSource): att(ttendance)cat(egories): Gr(Goers), Ir(regulars), Ab(senties)
#   dsSWcatatrans  - summarized  Counts and  Proportions of catatrans
#   dsSWcounts      - subset(dsSWcatatrans) vars   of att(endence catagory) (G-I-A) and Transitions (gg-...-aa)
#   dsSWprops       - subset(dsSWcatatrans) vars  of att(endence catagory) (G-I-A) and Transitions (gg-...-aa)
# 
#   dsOBScounts    - observed counts in ready for modeling
#   dsOBSprops     - observed proportions ready for modeling
#   --L---          - eLongated form of the respective dataset: replace "W" by "L" in any dataset name

# Choose dataset(s) to keep
# rm(list=setdiff(ls(), c("dsOBSprops")))
# str(dsOBSprops)
####### Section 1 ---------------------Read, clean, melt, cast-------------------end-----------------


####### Section 2 ------------------- Bring in model solutions -----------------begin----------------
# DATASET NAMES
#   dsModelCo      - dsModel=Contains the complete output from WinBUGS run
#                       Co = C(ontagion)o(riginal)
#                       Do = D(iffusion)o(riginal)
#                       Cs = C(ontagion)s(caled)
#                       Ds = D(iffusion)s(caled)
#   dsModels       - complete output  for all models
#   dsModelCoPars  - Contains only parameter values for each model (Co,Do,Cs,Ds)
#   dsModelsPars   - only parameter values  for all models
#   dsModelsParsLong
#   dsDIC          - model performance indices for all models: 
#                    mean difference (md), penalty (p), and DIC (DIC)
#   dsDICLong

# Bring in model solutions
pathCollectSolutions <- file.path(getwd(),"Results","collect_model_solutions.R")
source(pathCollectSolutions)

# Disable when working with "EMOSA_models.R"
# Choose dataset(s) to keep 
rm(list=setdiff(ls(),c("dsSource", #  original  NLSY97 datasource
                       "dsSWprops", # observed prevalences and transitions S-summarized, W-Wide, props- proportions
                       "dsSLprops", # elongated with time as ID and prevalence as RESPONS 
                       "dsSWcatatrans", # both counts and proportions - for both prevalences and transitions
                       "dsModelCoPars", # ds with Contagion original Parameters
                       "dsModelDoPars", # ds with Contagion original Parameters
                       "dsModelCsPars", # ds with Contagion scaled Parameters
                       "dsModelDsPars", # ds with Contagion scaled Parameters
                       "dsDIC"       # mean difference, penalty, and DIC of 4 models
)))










