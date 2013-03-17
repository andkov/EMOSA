rm(list=ls(all=TRUE)) #Clear out variables from previous runs.

# create a data frame 
 cohort <- c(1980,1981,1982)
 A00 <- c(.15, .2,.4)
 B00 <- c(.25, .3, .4) 
 C00 <-c(.6, .5,.2)
 Tab<-c(.6,.5,.4)
 Tac<-c(.2,.25,.35)
 ds <- data.frame(cohort,A00,B00,C00,Tab,Tac)
 print (ds)

#create a function to forecast the future values of A, B, C
forecast<- function( df ) {
  dsResult <- data.frame(
    cohort= df$cohort,
    A01   = df$A00 -df$Tab +df$Tac ,    
    B01   = df$B00 -df$Tab +df$Tac,    
    C01  =  df$C00 -df$Tab +df$Tac    
    
  )
  dsResult<- merge(df,dsResult,by="cohort")
  return( dsResult)
}

# take the initial values and parameter estimates 
# and forecast the future values into a new dataset
predicted<-forecast(ds)
print(predicted)

# QUESTION: how to make this iterate through arbitrary number of timepoints? 