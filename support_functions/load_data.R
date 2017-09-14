load_data <- function(filename) {
  
  # read the file
  nc = max(count.fields(filename, sep="\t"))
  raw_data = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")

  raw_data = as.matrix(raw_data)
  
  # if last column is all na, remove it
  if (sum(is.na(raw_data[,ncol(raw_data)])) == ncol(raw_data)) {
    raw_data = raw_data[, -ncol(raw_data), drop = F]
  }
  
  return(raw_data)
  
}