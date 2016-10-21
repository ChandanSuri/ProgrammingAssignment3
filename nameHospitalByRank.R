#ranking hospitals by outcome in a state
rankHospital <- function(state,outcome, num ="best"){
  
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
  
  #get that outcome's column data
  data[ ,col] <- as.numeric(data[ ,col])
  #get hospital names and death rates for a particular state
  stateDeathRateData <- data[data[,7] ==state, c(2,col)]
  #na values to be excluded
  stateDeathRateData <- na.omit(stateDeathRateData)
  nHospitals <- nrow(stateDeathRateData)
  switch (num,
    best = {
      num=1
    },
    worst = {
      num = nHospitals
    }
  )
  if(num>nHospitals){
    return(NA)
  }
  #order according to the death rates(2nd column), the first column(hospital names)
  orderData <- order(stateDeathRateData[ ,2],stateDeathRateData[ ,1])
  #return the name of the hospital having the rank as stated by num according to the data ordered
  stateDeathRateData[orderData,][num,1]
}

