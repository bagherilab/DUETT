
rm(list=ls())
setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/utility_functions.R")
source("support_functions/color_to_hex.R")

load("optimize_thresholds_merged_reps.RData")
# average data
data_mat = (data_mat[[1]] + data_mat[[2]] + data_mat[[3]]) / 3

# load originally labeled stuff
original_events = read.csv("analysis/SRP/replicates/SRP.csv")
# convert ramps to swings
original_events[original_events == -3] = -1
original_events[original_events == -1.5] = 1
original_events[original_events == 3] = 1
original_events[original_events == 1.5] = 1
original_events[original_events == 2] = 0
original_events[original_events == -2] = 0

# put NAs back in
original_events[is.na(data_mat)] = NA

num_events = matrix(NA, nrow = length(D_scan), ncol = length(I_scan) * length(P_scan))
dim(num_events) = c(length(D_scan), length(I_scan), length(P_scan))
dimnames(num_events) = dimnames(event_scan)

for (n_D in 1:length(D_scan)) {
  for (n_I in 1:length(I_scan)) {
    for (n_P in 1:length(P_scan)) {
      num_events[[n_D, n_I, n_P]] = sum(event_scan[[n_D,n_I,n_P]][, "end"] - event_scan[[n_D,n_I,n_P]][, "start"] + 1)
    }
  }
}

# calculate distance
threshold_normalized = lapply(dimnames(num_events), function(i) as.numeric(i) / max(as.numeric(i)))
DI_outer = outer(threshold_normalized[[1]], threshold_normalized[[2]], function(i,j) i^2 + j^2)
threshold_distance = num_events * NA
for (i in 1:length(P_scan)) {
  threshold_distance[,,i] = DI_outer + threshold_normalized[[3]][[i]] ^ 2
}

# normalize sum of threshold distance between 0 and 1
threshold_distance = threshold_distance / 3
# distance in terms of number of events
event_distance = (num_events / max(num_events)) ^ 2
# calculate euclidean distance from origin
total_distance = threshold_distance + event_distance
# sort distances
sorted_distance = sort(total_distance, decreasing = F)
indices = sapply(sorted_distance, function(i) which(total_distance == i, arr.ind = T))

# find pareto front
source("../../SAMDI/functions/find_ND.R")
temp = cbind(matrix(threshold_distance, ncol = 1), matrix(event_distance, ncol = 1))
ND = find_ND(temp)

###################################################
# doesn't work
if (F) {
  TP = c()
  FP = c()
  FN = c()
  TN = c()
  for (n in 1:length(indices)) {
    event_locations = expand_runs(event_scan[[indices[[n]][[1]], indices[[n]][[1]], indices[[n]][[1]]]], data_mat)
    TP = c(TP, sum((event_locations == original_events) & (event_locations != 0) & (original_events != 0), na.rm = T))
    FP = c(FP, sum((event_locations != original_events) & (original_events == 0), na.rm = T))
    FN = c(FN, sum((event_locations != original_events) & (event_locations == 0), na.rm = T))
    TN = c(TN, sum((original_events == 0) & (event_locations == 0), na.rm = T))
  }
  
  TPR = TP / (TP + FN)
  FPR = FN / (FP + TN)
}

###################################################
# try with monotonically increasinging thresholds
TP = c()
FP = c()
FN = c()
TN = c()
for (n in 1:min(dim(event_scan))) {
  event_locations = expand_runs(event_scan[[n,n,n]], data_mat)
  TP = c(TP, sum((event_locations == original_events) & (event_locations != 0) & (original_events != 0), na.rm = T))
  FP = c(FP, sum((event_locations != original_events) & (original_events == 0), na.rm = T))
  FN = c(FN, sum((event_locations != original_events) & (event_locations == 0), na.rm = T))
  TN = c(TN, sum((original_events == 0) & (event_locations == 0), na.rm = T))
}

TPR = TP / (TP + FN)
FPR = FP / (FP + TN)

plot(FPR, TPR)

get_TPR <- function(indices_arg) {
  event_locations = expand_runs(event_scan[[indices_arg[[1]], indices_arg[[2]], indices_arg[[3]]]], data_mat)
  TP = sum((event_locations == original_events) & (event_locations != 0) & (original_events != 0), na.rm = T)
  FN = sum((event_locations != original_events) & (event_locations == 0), na.rm = T)
  
  return(TP / (TP + FN))
}

get_TPR(c(4,4,4))


