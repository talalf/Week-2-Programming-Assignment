complete <- function(directory, id = 1:332) {
  nobs <- c()
  for(i in id){
    if(i >= 100){
      filepath <- paste(directory, "/", i, ".csv", sep = "")
      }
    else if(i >= 10) {
      filepath <- paste(directory, "/", "0", i, ".csv", sep = "")
      }
    else {
      filepath <- paste(directory, "/", "00", i, ".csv", sep = "")
      }
  obs <- sum(complete.cases(read.csv(filepath)))
  nobs <- append(nobs, obs)
  }
  outputframe <- data.frame(id, nobs)
  outputframe
  }