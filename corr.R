corr <- function(directory, threshold = 0) {
  n <- length(list.files(directory))
  outputvar <- c()
  for(i in 1:n){
    
    #this is needed for getting the individual filenames
    if(i >= 100){
      filepath <- paste(directory, "/", i, ".csv", sep = "")
        }
    else if(i >= 10) {
      filepath <- paste(directory, "/", "0", i, ".csv", sep = "")
        }
    else {
      filepath <- paste(directory, "/", "00", i, ".csv", sep = "")
        }
    
    #this is for reading the individual files
    table <- read.csv(filepath)
    complete <- sum(complete.cases(table)) #returns no. of complete cases for that .csv file
    
    if(complete > threshold) {
        complete_monitors <- 1
        var0 <- cor(x = table$sulfate, y = table$nitrate, use = "complete.obs")
        
        outputvar <- append(outputvar, var0)
    }
    
    else {
        complete_monitors <- 0
    }
  }
  outputvar
}