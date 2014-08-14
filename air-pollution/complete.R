complete <- function(directory, idd = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  id <- c(0)
  nobs <- c(0)
  dd <- data.frame(id,nobs)
  for (i in idd){
    
    if (i < 10){ a <- paste("00",toString(i),".csv", sep="")}
    else if (i < 100) {a <- paste("0",toString(i),".csv", sep="")}
    else {a <- paste(toString(i),".csv", sep="")}
    
    data <- read.csv(a)
   
    nr <- nrow(data)
    
      count <- 0 
      for (j in 1:nr){
        if (!is.na(data$sulfate[j]) && !is.na(data$nitrate[j])) {
          count <- count + 1}
      }
    id <- i
    nobs <- count
    ddd <- data.frame(id, nobs)
    dd <- rbind(dd, ddd)
    
  }
  dd[-1,] ##deleting the first null row that i added to initialize
  
  
}
