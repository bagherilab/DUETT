load_data <- function(filename) {
  
  # read the file
  nc = max(count.fields(filename, sep="\t"))
  raw_data = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  
  raw_data = as.matrix(raw_data)
  
  # browser()
  # # if last column is all na, remove it
  # if (sum(is.na(raw_data[,ncol(raw_data)])) == ncol(raw_data)) {
  #   raw_data = raw_data[, -ncol(raw_data), drop = F]
  # }
  # 
  # # if last row is all na, remove it
  # if (sum(is.na(raw_data[nrow(raw_data), ])) == nrow(raw_data)) {
  #   raw_data = raw_data[-nrow(raw_data), , drop = F]
  # }
  
  #### HACK remove first 14 non-NAs of each column
  # for (n_col in 1:ncol(raw_data)) {
  #   is_NA = which(is.na(raw_data[,n_col]))
  #   if (length(is_NA) > 0) {
  #     first_non_NA = is_NA[[length(is_NA)]] + 1
  #     raw_data[first_non_NA:min((first_non_NA + 14 - 1), nrow(raw_data)), n_col] = NA
  #   } else {
  #     raw_data[1:14, n_col] = NA
  #   }
  # }
  # raw_data = apply(raw_data, 1, function(i) i / mean(i, na.rm = T))
  return(raw_data)
  
}