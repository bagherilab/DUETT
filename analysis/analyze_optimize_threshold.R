
rm(list=ls())
setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/plotting/make_visual.R")
source("support_functions/utility_functions.R")
source("support_functions/color_to_hex.R")
source("support_functions/find_ND.R")

compare_optimized <- function(optimized_file, original_file, outfile_name = "outfile") {
  load(optimized_file)
  # average data
  data_mat = Reduce("+", data_mat)
  
  # load originally labeled stuff
  original_events = read.csv(original_file)
  # convert ramps to swings
  original_events[original_events == -3] = -1
  original_events[original_events == -1.5] = 1
  original_events[original_events == 3] = 1
  original_events[original_events == 1.5] = 1
  original_events[original_events == 2] = 0
  original_events[original_events == -2] = 0
  
  # placeholder
  concurrent_events = matrix(NA, nrow = 0, ncol = 4)
  
  ###################################################
  # compare to original events
  event_locations = expand_runs(event_scan[winner][[1]], data_mat)
  # convert NA to 0's
  event_locations[is.na(event_locations)] = 0
  
  agreement = which((event_locations == original_events) & (event_locations != 0) & (original_events != 0), arr.ind = T)
  disagreement_FP = which((event_locations != original_events) & (original_events == 0), arr.ind = T)
  disagreement_FN = which((event_locations != original_events) & (event_locations == 0), arr.ind = T)
  disagreement_TN = which((original_events == 0) & (event_locations == 0), arr.ind = T)
  
  # make visualization
  pdf(paste(outfile_name, ".pdf", sep = ""), width = 14, height = 14, useDingbats = F)
  temp = data_mat * 0
  temp[agreement] = event_locations[agreement]
  make_visual(data_mat, temp, concurrent_events)
  
  par(new = T)
  event_colors = list(upswing = color_to_hex("purple", 0.1), downswing = color_to_hex("purple", 0.1))
  temp = data_mat * 0
  temp[disagreement_FP] = event_locations[disagreement_FP]
  make_visual_PID(temp, event_draw = c(-1,1, -1.5,1.5, -3,3), ramp_draw = c(-1,1), event_colors = event_colors, box_resize = 1, lwd = 2)
  
  par(new = T)
  event_colors = list(upswing = color_to_hex("forestgreen", 0.1), downswing = color_to_hex("forestgreen", 0.1))
  temp = data_mat * 0
  temp[disagreement_FN] = original_events[disagreement_FN]
  make_visual_PID(temp, event_draw = c(-1,1, -1.5,1.5, -3,3), ramp_draw = c(-1,1), event_colors = event_colors, box_resize = 1, lwd = 2)
  
  par(new = F)
  make_visual(data_mat, original_events, concurrent_events)
  title("Original")
  
  make_visual(data_mat, event_locations, concurrent_events)
  title("Optimized")
  
  # draw the pareto curve
  temp = cbind(matrix(threshold_distance/max(threshold_distance), ncol = 1), matrix(event_distance, ncol = 1))
  ND = find_ND(temp)
  par(mar = c(8,8,8,8))
  plot(x=temp[ND,1], y=temp[ND,2], xlab = "Normalized threshold sum", ylab = "Normalized event number", xlim = c(0,1), ylim = c(0,1), col = "grey50", pch = 20, cex = 2)
  # plot(x=temp[,1], y=temp[,2], xlab = "Normalized threshold sum", ylab = "Normalized event number", xlim = c(0,1), ylim = c(0,1), col = "grey50", pch = 20, cex = 1)
  par(new = T)
  # plot the winner
  plot(x=threshold_distance[winner]/max(threshold_distance), y=event_distance[winner], xaxt = "n", yaxt = "n", xlim=c(0,1), ylim=c(0,1), xlab = "", ylab = "", bty = "n", pch = 20, col = "red", cex = 3)
  lines(x = c(0,threshold_distance[winner]/max(threshold_distance)), y = c(0, event_distance[winner]), lty = 2)
  
  dev.off()
  
  # calculate FP, TP, FN
  TP = nrow(agreement)
  FP = nrow(disagreement_FP)
  FN = nrow(disagreement_FN)
  TN = nrow(disagreement_TN)
  
  TPR = TP / (TP + FN)
  FPR = FP / (FP + TN)
  precision = TP / (TP + FP)
  
  return(list(TPR = TPR, FPR = FPR, precision = precision))
}

# compare_optimized("optimized_thresholds_SRP.RData", "analysis/SRP/replicates/SRP.csv", "optimized_comparison_SRP")
# compare_optimized("optimized_thresholds_F_0mM.RData", "analysis/F/replicates/F_0mM.csv", "optimized_comparison_F_0mM")
# compare_optimized("optimized_thresholds_F_10mM.RData", "analysis/F/replicates/F_10mM.csv", "optimized_comparison_F_10mM")



compare_optimized("optimized_thresholds_SRP_2.RData", "analysis/SRP/replicates/SRP.csv", "optimized_comparison_SRP_2")
compare_optimized("optimized_thresholds_F_0mM_2.RData", "analysis/F/replicates/F_0mM.csv", "optimized_comparison_F_0mM_2")
compare_optimized("optimized_thresholds_F_10mM_2.RData", "analysis/F/replicates/F_10mM.csv", "optimized_comparison_F_10mM_2")