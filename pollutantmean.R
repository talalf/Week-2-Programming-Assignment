pollutantmean <- function(directory, pollutant, id = 1:332){
  pollutant_sum <- 0
  length_total <- 0
  for(i in id) {
    if(i >= 100){
      filepath <- paste(directory, "/", i, ".csv", sep = "")
      print(filepath)
      }
    else if(i >= 10) {
      filepath <- paste(directory, "/", "0", i, ".csv", sep = "")
    }
    else {
      filepath <- paste(directory, "/", "00", i, ".csv", sep = "")
      }
  poldata <- read.csv(filepath)
  pollutant_1 <- sum(poldata[ ,pollutant], na.rm = TRUE)
  pollutant_sum <- pollutant_sum + pollutant_1
  length_1 <- length(poldata[ , pollutant]) - sum(is.na(poldata[ , pollutant]))
  length_total <- length_total + length_1
  }
  mean_pollutant <- pollutant_sum / length_total
  mean_pollutant
}