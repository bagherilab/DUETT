load_data <- function(filename) {
  
  # read the file
  raw_data = read.csv(filename)
  
  raw_data = as.matrix(raw_data)
  
  return(raw_data)
  
}