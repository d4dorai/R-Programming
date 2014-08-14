best <- function(state, out) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## outcome: heart attack", \heart failure", or \pneumonia
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
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
    

d <- d[!is.na(d$out), ]

d <- d[d$out == min(d$out),]

d <- d[order(d$name),]

as.character(d[1,2])


}



