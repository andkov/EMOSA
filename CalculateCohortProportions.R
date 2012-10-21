rm(list=ls(all=TRUE)) #Clear all the variables from previous runs

if( Sys.info()["nodename"] == "MICKEY" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/Consulting/EmosaMcmc/Dev/EmosaFork/EMOSA/Data"
if( Sys.info()["nodename"] == "MERKANEZ-PC" ) 
  pathDirectory <- "F:/Users/wibeasley/Documents/SSuccess/InterimStudy" #Change this directory location

pathInData <- file.path(pathDirectory, "subject_data_emosa_nonmiss.csv")

times <- 0:8
years <- sort(unique(dsRaw$byear))

dsRaw <- read.csv(pathInData, stringsAsFactors=FALSE)
dsSummarized <- data.frame(
  Time=times, 
  TotalGoers=NA_real_, TotalIrregulars=NA_real_, TotalAbsentees=NA_real_,
  ProportionGoers=NA_real_, ProportionIrregulars=NA_real_, ProportionAbsentees=NA_real_
  )

for( t in dsSummarized$Time ) {
# for( t in 7 ) {
  rawColumnName <- paste0("att0", t)
  print(rawColumnName)
  count <- sum(!is.na(dsRaw[, rawColumnName]))
  dsSummarized[t+1, 'TotalGoers'] <- sum(dsRaw[, rawColumnName] %in% c(1,2))
  dsSummarized[t+1, 'TotalIrregulars'] <- sum(dsRaw[, rawColumnName] %in% c(3,4,5))
  dsSummarized[t+1, 'TotalAbsentees'] <- sum(dsRaw[, rawColumnName] %in% c(6,7,8))
}
yearCount <- apply(dsSummarized[, c('TotalGoers', 'TotalIrregulars', 'TotalAbsentees')], 1, sum)
dsSummarized[, 'ProportionGoers'] <- dsSummarized[, 'TotalGoers'] / yearCount
dsSummarized[, 'ProportionIrregulars'] <- dsSummarized[, 'TotalIrregulars'] / yearCount
dsSummarized[, 'ProportionAbsentees'] <- dsSummarized[, 'TotalAbsentees'] / yearCount

