optimize_thresholds <- function(data_mat, optimize_parameters, outfile_optimize) {
  
  num_files = length(data_mat)
  print("Optimizing thresholds")
  start_time = Sys.time()
  source("support_functions/utility_functions.R")
  source("support_functions/merge_replicates.R")
  source("support_functions/find_events_optimize.R")
  source("support_functions/find_events.R")
  source("support_functions/find_ND.R")
  
  # record parameters
  window_size = optimize_parameters$window_size_optimize
  I_length = window_size
  P_scan = seq(optimize_parameters$P_start, optimize_parameters$P_end, optimize_parameters$P_interval)
  I_scan = seq(optimize_parameters$I_start, optimize_parameters$I_end, optimize_parameters$I_interval)
  R_scan = seq(optimize_parameters$R_start, optimize_parameters$R_end, optimize_parameters$R_interval)
  
  # variables to cache events
  P_events = vector("list", length(P_scan))
  names(P_events) = as.character(P_scan)
  P_events = lapply(1:num_files, function(i) P_events)
  
  I_events = vector("list", length(I_scan))
  names(I_events) = as.character(I_scan)
  I_events = lapply(1:num_files, function(i) I_events)
  
  R_events = vector("list", length(R_scan))
  names(R_events) = as.character(R_scan)
  R_events = lapply(1:num_files, function(i) R_events)
  
  # placeholder
  concurrent_events = matrix(NA, nrow = 0, ncol = 4)
  ###################################################
  # calculate PIR
  if (window_size == 0) {
    P_values = lapply(data_mat, function(i) i * 0)
  } else {
    P_values = lapply(data_mat, function(i) diff_data(i, window_size))
  }
  
  num_row = nrow(data_mat[[1]])
  num_col = ncol(data_mat[[1]])
  I_values = lapply(1:length(P_values), function(i) matrix(NA, nrow = num_row, ncol = num_col))
  if (I_length > num_row) {return(I_values)}
  
  for (n_file in 1:length(P_values)) {
    for (n_col in 1:num_col) {
      I_values[[n_file]][(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(P_values[[n_file]][i:(i+I_length),n_col])) / I_length
    }
  }
  R_values = lapply(1:num_files, function(i) P_values[[i]] / abs(data_mat[[i]] - P_values[[i]]))
  
  ################################################
  # do the actual optimization
  
  event_scan = vector("list", length(P_scan) * length(I_scan) * length(R_scan))
  dim(event_scan) = c(length(P_scan), length(I_scan), length(R_scan))
  dimnames(event_scan) = list(P_scan, I_scan, R_scan)
  
  set_event_scan <- function(n_P, n_I, n_R, event_scan_arg, event_obj) {
    event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_R)]] = event_obj
    return(event_scan_arg)
  }
  get_event_scan <- function(n_P, n_I, n_R, event_scan_arg) {
    return(event_scan_arg[[as.character(n_P), as.character(n_I), as.character(n_R)]])
  }
  
  for (n_P in P_scan) {
    for (n_I in I_scan) {
      for (n_R in R_scan) {
        event_storage = vector("list", num_files)
        
        # time1 = Sys.time()
        for (n_file in 1:num_files) {
          return_list = find_events(P_values[[n_file]], 
                                    I_values[[n_file]], 
                                    R_values[[n_file]], 
                                    NULL,
                                    data_mat[[n_file]], 
                                    window_size = window_size, 
                                    cutoffs = list(P=n_P, I=n_I, R=n_R), 
                                    I_length = I_length)
          
          event_storage[[n_file]] = return_list$event_locations
          
          # clean events
          event_storage[[n_file]] = clean_events(event_storage[[n_file]], 3, 1)
        }
        
        # store the results
        temp = lapply(1:num_files, function(i) list(event_storage[[i]], concurrent_events))
        event_storage = merge_replicates(temp, num_files)[[1]]
        
        event_storage = find_runs(event_storage)
        event_scan = set_event_scan(n_P, n_I, n_R, event_scan, event_storage)
      }
    }
  }
  
  # count number of events
  num_events = matrix(NA, nrow = length(P_scan), ncol = length(I_scan) * length(R_scan))
  dim(num_events) = c(length(P_scan), length(I_scan), length(R_scan))
  dimnames(num_events) = dimnames(event_scan)
  for (n_P in 1:length(P_scan)) {
    for (n_I in 1:length(I_scan)) {
      for (n_R in 1:length(R_scan)) {
        num_events[[n_P, n_I, n_R]] = sum(event_scan[[n_P,n_I,n_R]][, "end"] - event_scan[[n_P,n_I,n_R]][, "start"] + 1)
      }
    }
  }
  
  # calculate distance
  threshold_normalized = lapply(dimnames(num_events), function(i) as.numeric(i) / max(as.numeric(i)))
  PI_outer = outer(threshold_normalized[[1]], threshold_normalized[[2]], function(i,j) i^2 + j^2)
  threshold_distance = num_events * NA
  for (i in 1:length(R_scan)) {
    threshold_distance[,,i] = PI_outer + threshold_normalized[[3]][[i]] ^ 2
  }
  
  # distance in terms of number of events
  event_distance = (num_events / max(num_events)) ^ 2
  # calculate euclidean distance from origin
  total_distance = threshold_distance + event_distance
  # find shortest distance winner
  winner = which(total_distance == min(total_distance), arr.ind = T)
  
  end_time = Sys.time()
  save(data_mat, num_events, threshold_distance, event_distance, total_distance, winner, event_scan, P_scan, I_scan, R_scan, start_time, end_time, file = paste(outfile_optimize, ".RData", sep = ""))
  
  event_locations = expand_runs(event_scan[[winner[[1]], winner[[2]], winner[[3]]]], data_mat[[1]]) # convert to event_locations format
  
  ################################## output
  
  pdf(paste( "Results were saved to ", outfile_optimize, ".pdf", sep = ""))
  plot(0:100, 0:100, type = "n", xaxt = "n", yaxt = "n", main = "", xlab = "", ylab = "", bty = "n")
  text(0, 100, "Optimized thresholds", cex = 1.5, pos = 4)
  text(0, 95, paste("P=", P_scan[[winner[[1]]]]), pos = 4)
  text(0, 90, paste("I=", I_scan[[winner[[2]]]]), pos = 4)
  text(0, 85, paste("R=", R_scan[[winner[[3]]]]), pos = 4)
  text(0, 80, paste("Number of events = ", num_events[[winner[[1]], winner[[2]], winner[[3]]]], sep = ""), pos = 4)
  
  text(100, 100, "Other parameters", cex = 1.5, pos = 2)
  text(rep(100, 10), seq(95, 50, -5), paste(names(optimize_parameters), optimize_parameters, sep = "="), pos = 2)
  
  text(0, 50, paste("Computational time:", round(end_time-start_time, 2), "seconds"), pos = 4)
  
  # draw the pareto curve
  temp = cbind(matrix(threshold_distance/max(threshold_distance), ncol = 1), matrix(event_distance, ncol = 1))
  ND = find_ND(temp)
  # par(mar = c(8,8,8,8))
  plot(x=temp[ND,1], y=temp[ND,2], xlab = "Normalized threshold sum", ylab = "Normalized event number", xlim = c(0,1), ylim = c(0,1), col = "grey50", pch = 20, cex = 2, main = "Elbow criterion for threshold selection")
  par(new = T)
  # plot the winner
  plot(x=threshold_distance[winner]/max(threshold_distance), y=event_distance[winner], xaxt = "n", yaxt = "n", xlim=c(0,1), ylim=c(0,1), xlab = "", ylab = "", bty = "n", pch = 20, col = "red", cex = 3)
  lines(x = c(0,threshold_distance[winner]/max(threshold_distance)), y = c(0, event_distance[winner]), lty = 2)
  
  dev.off()
  ################################## 
  
  print("Optimizing thresholds is complete")
  print(paste( "Results were saved to ", outfile_optimize, ".RData", sep = ""))
  print(paste( "Results were saved to ", outfile_optimize, ".pdf", sep = ""))
  
}