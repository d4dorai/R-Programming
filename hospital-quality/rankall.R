rankall <- function(out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  
  if (out == "heart attack") {
    
    
    d <-  data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcome$Hospital.Name)
    
    
    
  } else if (out == "heart failure") {
    
    
    d <-  data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ), outcome$Hospital.Name)
    
    
    
  } else if (out == "pneumonia"){
    
    d <- data.frame(outcome$State, as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcome$Hospital.Name)  
    
    
    
  } else { stop("invalid outcome")}
  
  
  colnames(d) <- c("state" ,"out", "hospital")
  
  thestates <- unique(outcome$State)
  
  d <- split(d, d$state)
 
  #initialize newdata
  state <- c("GOOO")
  hospital <- c("haa")
  newdata <- data.frame( hospital , state, stringsAsFactors=FALSE)
  
  
  for (i in thestates){
    b <-  paste("d$", i, sep="")
    ex <- parse(text = b)
    tempd <- eval(ex)[order(eval(ex)$out, eval(ex)$hospital, na.last = NA),]  
    
    
    
    if (num == "best") {
     newrow <- c( as.character(tempd[1,3]) , i ) 
      
    } else if ( num == "worst") {
      newrow <- c(as.character(tempd[nrow(tempd),3]) , i )
      
    } else if (num > nrow(tempd)) {newrow <- c( NA , i)
                               
    } else { newrow <- c( as.character(tempd[num,3]) ,i) }
    
    
    newdata <- rbind(newdata, newrow)
  }
  
  newdata <- newdata[-1,]

}
