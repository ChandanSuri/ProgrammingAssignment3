#Finding the best hospital in a state
best <-function(state,outcome)
{
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  #Check the validity of state
  if(!state %in% unique(data[,7])) 
  {
    #7th column is having the abbreviations for states
    stop("invalid state")
  }
  #check the validity of outcome needed as inputted by the user
  switch(outcome,'heart attack' ={
    col=11
    #set the column to 11 as for heart attacks
  }, 'heart failure' = {
    col=17
    #set the column to 17 as for heart failures
  }, 'pneumonia' = {
    col=23;
    #set the column to 23 as for pneumonia patients
  }, stop("invalid outcome"))
  #return the columns with death rates and hospital names
  colData <- c(2,col)
  #extract data for a particular state
  stateDeathRateData <- data[data$State == state, colData]
  #extract the first state having minimum death rate for 30 days as in the 2nd column of data extracted
  #this is returned
  stateDeathRateData[which.min(stateDeathRateData[,2]),1]
}