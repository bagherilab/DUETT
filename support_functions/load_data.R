load_data <- function(filename) {
  
  raw_data_list = vector("list", length(filename))
  for (n_file in 1:length(filename)) {
    # read the file
    nc = max(count.fields(filename[[n_file]], sep="\t"))
    raw_data_list[[n_file]] = read.table(filename[[n_file]], sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
    
    raw_data_list[[n_file]] = as.matrix(raw_data_list[[n_file]])
  }
  
  data_dim = sapply(raw_data_list, function(i) dim(i))
  num_unique = sapply(1:2, function(i) length(unique(data_dim[i,])))
  
  if (any(num_unique != 1)) {
    warning("Data input files not all same dimension; only using first input file")
    for (n_file in 1:length(filename)) {
      print(paste(filename[[n_file]], dim(raw_data_list[[n_file]])))
    }
    return(raw_data_list[[1]])
  }
  
  # # if last column is all na, remove it
  # if (sum(is.na(raw_data[,ncol(raw_data)])) == ncol(raw_data)) {
  #   raw_data = raw_data[, -ncol(raw_data), drop = F]
  # }
  # 
  # # if last row is all na, remove it
  # if (sum(is.na(raw_data[nrow(raw_data), ])) == nrow(raw_data)) {
  #   raw_data = raw_data[-nrow(raw_data), , drop = F]
  # }
  
  return(raw_data_list)
  
}