pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
  vec <- 0
  
  b <- paste("data$", pollutant, sep="")
  
  for (i in id ) {
    
    if (i < 10){ a <- paste("00",toString(i),".csv", sep="")}
    else if (i < 100) {a <- paste("0",toString(i),".csv", sep="")}
    else {a <- paste(toString(i),".csv", sep="")}
 
    data <- read.csv(a)
    
    vec <- c(vec, eval(parse(text = b)) )
    
  }
  
  themean <- mean(vec[2:length(vec)], na.rm = TRUE)
  themean
}
