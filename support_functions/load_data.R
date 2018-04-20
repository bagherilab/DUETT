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
  
  
  #### HACK
  # for (n_file in 1:length(filename)) {
  #   raw_data_list[[n_file]][is.na(raw_data_list[[n_file]])] = 0
  #   
  #   # retain only first 100 columns
  #   # data_mat = data_mat[,1:100]
  #   
  #   # transpose so samples become variables
  #   # data_mat = t(data_mat)
  #   
  #   cluster_obj = hclust(dist(data_mat))
  #   
  #   plot(cluster_obj)
  #   cluster_cut = cutree(cluster_obj, 10)
  #   
  #   # reorder columns according to clusters
  #   data_mat = data_mat[,order(cluster_cut)]
  # }
  #### hack
  
  return(raw_data_list)
  
}