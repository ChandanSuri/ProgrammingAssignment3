#Rank all the hospitals
rankAll <- function(outcome, num="best")
{
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  states <- unique(data[,7])
  
  #Check the validity of state
  switch(outcome, 'heart attack' ={
    col=11
  }, 'heart failure' = {
    col=17
  }, 'pneumonia' = {
    col=23
  }, stop("invalid outcome"))
  
  #get that outcome's column data
  data[ ,col] <- as.numeric(data[ ,col])
  
  #only extract name, state and death rate for 30 days
  data <- data[, c(2,7,col)]
  data <- na.omit(data)
  
  #to get hospitals according to order by death rates in a particular state
  rankInStates <- function(state)
  {
    #get hospital names and death rates for a particular state
    stateDeathRateData <- data[data[,2]==state, ]
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
    #order according to the death rates(3rd column), the first column(hospital names)
    orderData <- order(stateDeathRateData[,3],stateDeathRateData[,1])
    result <- stateDeathRateData[orderData, ][num,1]
    c(result,state)
  }
  #to get the order of hospitals for all states and merge them
  finalResult <- do.call(rbind, lapply(states, rankInStates))
  #return in specified format
  finalResult <- finalResult[order(finalResult[,2]), ]
  rownames(finalResult) <- finalResult[,2]
  colnames(finalResult) <- c("hospital","state")
  data.frame(finalResult)
}