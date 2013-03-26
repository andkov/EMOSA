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

# Initially I'd like to make two suggestions that might make the problem easier 
# to code. First, revise the data schema so that each year is a unique row, and 
# each group is a unique column. Second, since the cohorts are treated 
# mathematically independent of each other, keep them separate for now, 
# at least until the code's kernel is built. Put a loop around this later
# that cycles through them. In the first block of code, there are two matrices,
# one with observed data, and one that will collect the predicted data

yearCount <- 7 #Declare the number of time points.
groupCount <- 3 #Declare the number of groups.

#Create fake data that sum to 1 across rows/times.
ob <- matrix(runif(yearCount*groupCount), ncol=groupCount)
ob <- ob / apply(ob, 1, function( x ){ return( sum(x) )})

#Establish a container to hold the predicted values.
pred <- matrix(NA_real_, ncol=groupCount, nrow=yearCount)

t12<-.5; t13<-.2; t11<-1-t12-t13 #Transition parameters from group 1
t21<-.9; t23<-.4; t22<-1-t21-t23 #Transition parameters from group 2
t31<-.2; t32<-.7; t33<-1-t31-t32 #Transition parameters from group 3

for( i in 2:yearCount ) {
  pred[i, 1] <- ob[i-1, 1]*t11 + ob[i-1, 2]*t21 + ob[i-1, 3]*t31
  pred[i, 2] <- ob[i-1, 1]*t12 + ob[i-1, 2]*t22 + ob[i-1, 3]*t32
  pred[i, 3] <- ob[i-1, 1]*t13 + ob[i-1, 2]*t23 + ob[i-1, 3]*t33
}

#Calculate the squared errors
ss <- (pred[-1, ] - ob[-1, ])^2 #Ignore the first year of data
print(ss)
sum(ss)

#Create transition parameters that sum to 1 across rows/groups.
tt <-  matrix(runif(groupCount*groupCount), ncol=groupCount)
tt <- tt / apply(tt, 1, function( x ){ return( sum(x) )})











