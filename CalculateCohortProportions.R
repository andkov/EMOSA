rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(plyr)
require(reshape2)

if( Sys.info()["nodename"] == "MICKEY" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EMOSA/Data"
  #pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EmosaFork/EMOSA/Data"
if( Sys.info()["nodename"] == "MERKANEZ-PC" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy" #Change this directory location

pathInData <- file.path(pathDirectory, "subject_data_emosa_nonmiss.csv")

dsWide <- read.csv(pathInData, stringsAsFactors=FALSE)

times <- 0:8
years <- 1980:1984 #sort(unique(dsWide$byear))

#Include only records with a valid birth year
dsWide <- dsWide[dsWide$byear %in% years, ]
dsWide$byear <- as.integer(dsWide$byear)

#Include only records with a valid ID
dsWide <- dsWide[dsWide$id != "V", ]
dsWide$id <- as.integer(dsWide$id)

#Drop the birth month variable
dsWide <- dsWide[, colnames(dsWide) != "bmonth"]

#Inspect the resulting dataset
summary(dsWide)

#Transform the wide dataset into a long dataset
dsLong <- melt(dsWide, id.vars=c("id", "byear"))

#Convert the year variable from a character to a number
dsLong$variable <- gsub(pattern="att", replacement="", x=dsLong$variable) #Strip off the 'att' prefix
dsLong$variable <- as.integer(dsLong$variable) #Convert to a number.
dsLong <- plyr::rename(dsLong, replace=c(variable="time", value="attendence"))
summary(dsLong)

#Create a function to summarize each byear*time cell
SummarizeBYearTime <- function( df ) {#df stands for 'data.frame'
  observationCount <- 
  dsResult <- data.frame(
  )
  return( dsResult)
}


plyr::ddply(dsWide, .variables=c("byear", "time")

dsSummarized <- data.frame(
  Time=times, 
  TotalGoers=NA_real_, TotalIrregulars=NA_real_, TotalAbsentees=NA_real_,
  ProportionGoers=NA_real_, ProportionIrregulars=NA_real_, ProportionAbsentees=NA_real_
)

# for( t in dsSummarized$Time ) {
# # for( t in 7 ) {
#   rawColumnName <- paste0("att0", t)
#   #print(rawColumnName)
#   count <- sum(!is.na(dsRaw[, rawColumnName]))
#   dsSummarized[t+1, 'TotalGoers'] <- sum(dsRaw[, rawColumnName] %in% c(1,2))
#   dsSummarized[t+1, 'TotalIrregulars'] <- sum(dsRaw[, rawColumnName] %in% c(3,4,5))
#   dsSummarized[t+1, 'TotalAbsentees'] <- sum(dsRaw[, rawColumnName] %in% c(6,7,8))
# }
# yearCount <- apply(dsSummarized[, c('TotalGoers', 'TotalIrregulars', 'TotalAbsentees')], 1, sum)
# dsSummarized[, 'ProportionGoers'] <- dsSummarized[, 'TotalGoers'] / yearCount
# dsSummarized[, 'ProportionIrregulars'] <- dsSummarized[, 'TotalIrregulars'] / yearCount
# dsSummarized[, 'ProportionAbsentees'] <- dsSummarized[, 'TotalAbsentees'] / yearCount

