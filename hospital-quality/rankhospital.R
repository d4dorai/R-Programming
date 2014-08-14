rankhospital <- function(state, out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  if (!(state %in% outcome$State)) {stop("invalid state")}
  
  if (out == "heart attack") {
    
    
    d <-  data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcome$Hospital.Name)
    
    
    
  } else if (out == "heart failure") {
    
    
    d <-  data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ), outcome$Hospital.Name)
    
    
    
  } else if (out == "pneumonia"){
    
    d <- data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcome$Hospital.Name)  
    
    
    
  } else { stop("invalid outcome")}
  
  
  colnames(d) <- c("st" ,"out", "name")
  
  d <- d[d$st == state,  ]  
  
  d$st <- NULL
  

  
  d <- d[order(d$out,d$name, na.last = NA),]
  
  if (num == "best") {
    as.character(d[1,2])
    
  } else if ( num == "worst") {
    as.character(d[nrow(d),2])
    
  } else if (num > nrow(d)) {NA
  
  } else { as.character(d[num,2]) }

}
