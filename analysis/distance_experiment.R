
rm(list=ls())
setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")


distance_experiment <- function(orig_file, new_file) {
  load(orig_file)
  
  # calculate distance
  threshold_normalized = lapply(dimnames(num_events), function(i) as.numeric(i) / max(as.numeric(i)))
  PI_outer = outer(threshold_normalized[[1]], threshold_normalized[[2]], function(i,j) i^2 + j^2)
  threshold_distance = num_events * NA
  for (i in 1:length(D_scan)) {
    threshold_distance[,,i] = PI_outer + threshold_normalized[[3]][[i]] ^ 2
  }
  
  # distance in terms of number of events
  event_distance = (num_events / max(num_events)) ^ 2
  # calculate euclidean distance from origin
  total_distance = threshold_distance + event_distance
  # find shortest distance winner
  winner = which(total_distance == min(total_distance), arr.ind = T)
  
  save(data_mat, num_events, threshold_distance, event_distance, total_distance, winner, event_scan, D_scan, I_scan, P_scan, start_time, end_time, file = paste(new_file, sep = ""))
}

distance_experiment("optimized_thresholds_SRP.RData", "optimized_thresholds_SRP_2.RData")
distance_experiment("optimized_thresholds_F_0mM.RData", "optimized_thresholds_F_0mM_2.RData")
distance_experiment("optimized_thresholds_F_10mM.RData", "optimized_thresholds_F_10mM_2.RData")

