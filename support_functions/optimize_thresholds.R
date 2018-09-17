optimize_thresholds <- function(data_mat, optimize_parameters, outfile_optimize) {
  
  num_files = length(data_mat)
  print("Optimizing thresholds")
  start_time = Sys.time()
  source("support_functions/utility_functions.R")
  source("support_functions/merge_replicates.R")
  source("support_functions/find_events_optimize.R")
  source("support_functions/find_events.R")
  
  # record parameters
  window_size = optimize_parameters$window_size_optimize
  I_length = window_size
  P_scan = seq(optimize_parameters$P_start, optimize_parameters$P_end, optimize_parameters$P_interval)
  I_scan = seq(optimize_parameters$I_start, optimize_parameters$I_end, optimize_parameters$I_interval)
<<<<<<< HEAD
  R_scan = seq(optimize_parameters$R_start, optimize_parameters$R_end, optimize_parameters$R_interval)
=======
  D_scan = seq(optimize_parameters$D_start, optimize_parameters$D_end, optimize_parameters$D_interval)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  
  # variables to cache events
  P_events = vector("list", length(P_scan))
  names(P_events) = as.character(P_scan)
  P_events = lapply(1:num_files, function(i) P_events)
  
  I_events = vector("list", length(I_scan))
  names(I_events) = as.character(I_scan)
  I_events = lapply(1:num_files, function(i) I_events)
  
<<<<<<< HEAD
  R_events = vector("list", length(R_scan))
  names(R_events) = as.character(R_scan)
  R_events = lapply(1:num_files, function(i) R_events)
=======
  D_events = vector("list", length(D_scan))
  names(D_events) = as.character(D_scan)
  D_events = lapply(1:num_files, function(i) D_events)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  
  # placeholder
  concurrent_events = matrix(NA, nrow = 0, ncol = 4)
  ###################################################
<<<<<<< HEAD
  # calculate PIR
  if (window_size == 0) {
    P_values = lapply(data_mat, function(i) i * 0)
  } else {
    P_values = lapply(data_mat, function(i) diff_data(i, window_size))
=======
  # calculate PID
  if (window_size == 0) {
    D_values = lapply(data_mat, function(i) i * 0)
  } else {
    D_values = lapply(data_mat, function(i) diff_data(i, window_size))
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  }
  
  num_row = nrow(data_mat[[1]])
  num_col = ncol(data_mat[[1]])
<<<<<<< HEAD
  I_values = lapply(1:length(P_values), function(i) matrix(NA, nrow = num_row, ncol = num_col))
  if (I_length > num_row) {return(I_values)}
  
  for (n_file in 1:length(P_values)) {
    for (n_col in 1:num_col) {
      I_values[[n_file]][(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(P_values[[n_file]][i:(i+I_length),n_col])) / I_length
    }
  }
  R_values = lapply(1:num_files, function(i) P_values[[i]] / abs(data_mat[[i]] - P_values[[i]]))
=======
  I_values = lapply(1:length(D_values), function(i) matrix(NA, nrow = num_row, ncol = num_col))
  if (I_length > num_row) {return(I_values)}
  
  for (n_file in 1:length(D_values)) {
    for (n_col in 1:num_col) {
      I_values[[n_file]][(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(D_values[[n_file]][i:(i+I_length),n_col])) / I_length
    }
  }
  P_values = lapply(1:num_files, function(i) D_values[[i]] / abs(data_mat[[i]] - D_values[[i]]))
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  
  ################################################
  # do the actual optimization
  
<<<<<<< HEAD
  event_scan = vector("list", length(P_scan) * length(I_scan) * length(R_scan))
  dim(event_scan) = c(length(P_scan), length(I_scan), length(R_scan))
  dimnames(event_scan) = list(P_scan, I_scan, R_scan)
  
  set_event_scan <- function(n_P, n_I, n_R, event_scan_arg, event_obj) {
    event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_R)]] = event_obj
    return(event_scan_arg)
  }
  get_event_scan <- function(n_P, n_I, n_R, event_scan_arg) {
    return(event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_R)]])
=======
  event_scan = vector("list", length(P_scan) * length(I_scan) * length(D_scan))
  dim(event_scan) = c(length(P_scan), length(I_scan), length(D_scan))
  dimnames(event_scan) = list(P_scan, I_scan, D_scan)
  
  set_event_scan <- function(n_P, n_I, n_D, event_scan_arg, event_obj) {
    event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_D)]] = event_obj
    return(event_scan_arg)
  }
  get_event_scan <- function(n_P, n_I, n_D, event_scan_arg) {
    return(event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_D)]])
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  }
  
  for (n_P in P_scan) {
    for (n_I in I_scan) {
<<<<<<< HEAD
      for (n_R in R_scan) {
=======
      for (n_D in D_scan) {
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
        event_storage = vector("list", num_files)
        
        # time1 = Sys.time()
        for (n_file in 1:num_files) {
          return_list = find_events(P_values[[n_file]], 
                                    I_values[[n_file]], 
                                    D_values[[n_file]], 
                                    NULL,
                                    data_mat[[n_file]], 
                                    window_size = window_size, 
<<<<<<< HEAD
                                    cutoffs = list(P=n_P, I=n_I, R=n_R), 
=======
                                    cutoffs = list(P=n_P, I=n_I, D=n_D), 
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
                                    I_length = I_length)
          
          event_storage[[n_file]] = return_list$event_locations
          
<<<<<<< HEAD
=======
          # cache the events detected for PID
          # P_events[[n_file]][[as.character(n_P)]] = return_list$P_events
          # I_events[[n_file]][[as.character(n_I)]] = return_list$I_events
          # D_events[[n_file]][[as.character(n_D)]] = return_list$D_events
          
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
          # clean events
          event_storage[[n_file]] = clean_events(event_storage[[n_file]], 3, 1)
        }
        
<<<<<<< HEAD
=======
        # time2 = Sys.time()
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
        # store the results
        temp = lapply(1:num_files, function(i) list(event_storage[[i]], concurrent_events))
        event_storage = merge_replicates(temp, num_files)[[1]]
        
<<<<<<< HEAD
        event_storage = find_runs(event_storage)
        event_scan = set_event_scan(n_P, n_I, n_R, event_scan, event_storage)
=======
        # time3 = Sys.time()
        event_storage = find_runs(event_storage)
        # time4 = Sys.time()
        event_scan = set_event_scan(n_P, n_I, n_D, event_scan, event_storage)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
      }
    }
  }
  
  # count number of events
<<<<<<< HEAD
  num_events = matrix(NA, nrow = length(P_scan), ncol = length(I_scan) * length(R_scan))
  dim(num_events) = c(length(P_scan), length(I_scan), length(R_scan))
  dimnames(num_events) = dimnames(event_scan)
  for (n_P in 1:length(P_scan)) {
    for (n_I in 1:length(I_scan)) {
      for (n_R in 1:length(R_scan)) {
        num_events[[n_P, n_I, n_R]] = sum(event_scan[[n_P,n_I,n_R]][, "end"] - event_scan[[n_P,n_I,n_R]][, "start"] + 1)
=======
  num_events = matrix(NA, nrow = length(P_scan), ncol = length(I_scan) * length(D_scan))
  dim(num_events) = c(length(P_scan), length(I_scan), length(D_scan))
  dimnames(num_events) = dimnames(event_scan)
  for (n_P in 1:length(P_scan)) {
    for (n_I in 1:length(I_scan)) {
      for (n_D in 1:length(D_scan)) {
        num_events[[n_P, n_I, n_D]] = sum(event_scan[[n_P,n_I,n_D]][, "end"] - event_scan[[n_P,n_I,n_D]][, "start"] + 1)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
      }
    }
  }
  
  # calculate distance
  threshold_normalized = lapply(dimnames(num_events), function(i) as.numeric(i) / max(as.numeric(i)))
  PI_outer = outer(threshold_normalized[[1]], threshold_normalized[[2]], function(i,j) i^2 + j^2)
  threshold_distance = num_events * NA
<<<<<<< HEAD
  for (i in 1:length(R_scan)) {
    threshold_distance[,,i] = PI_outer + threshold_normalized[[3]][[i]] ^ 2
  }
  
=======
  for (i in 1:length(D_scan)) {
    threshold_distance[,,i] = PI_outer + threshold_normalized[[3]][[i]] ^ 2
  }
  
  # normalize sum of threshold distance between 0 and 1
  # threshold_distance = threshold_distance / max(threshold_distance)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  # distance in terms of number of events
  event_distance = (num_events / max(num_events)) ^ 2
  # calculate euclidean distance from origin
  total_distance = threshold_distance + event_distance
  # find shortest distance winner
  winner = which(total_distance == min(total_distance), arr.ind = T)
  
  end_time = Sys.time()
<<<<<<< HEAD
  save(data_mat, num_events, threshold_distance, event_distance, total_distance, winner, event_scan, D_scan, I_scan, R_scan, start_time, end_time, file = paste(outfile_optimize, ".RData", sep = ""))
  
  event_locations = expand_runs(event_scan[[winner[[1]], winner[[2]], winner[[3]]]], data_mat[[1]]) # convert to event_locations format
  
  print("Optimizing thresholds is complete")
  print(paste( "Results were saved to ", outfile_optimize, ".RData", sep = ""))
=======
  save(data_mat, num_events, threshold_distance, event_distance, total_distance, winner, event_scan, D_scan, I_scan, P_scan, start_time, end_time, file = paste(outfile_optimize, ".RData", sep = ""))
  
  event_locations = expand_runs(event_scan[[winner[[1]], winner[[2]], winner[[3]]]], data_mat[[1]]) # convert to event_locations format
  
  stop("everything is fine")
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
}