# PID examples
set.seed(12345)

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")
# setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")

rm(list=ls())

source("helper_functions.R")
source("../../support_functions/utility_functions.R")

make_pair_plot <- function(window_size, start_level, end_level, event_length, buffer_length = 20, buffer_plot = 10, rnorm_sd = 0.1, ylim = c(0, end_level+1), max_PID = 0.5, y_shift = 0, back_down = F, top_length = 1) {
  
  y = make_values(start_level = start_level, end_level = end_level, event_length = event_length, buffer_length = buffer_length, rnorm_sd = rnorm_sd, back_down = back_down, top_length = top_length)
  x = 1:length(y)
  
  # partially remove buffer
  x_plot = x[(buffer_length - buffer_plot + 1):(length(x) - buffer_length + buffer_plot + 1)]
  x_plot = 1:length(x_plot)
  y_plot = y[(buffer_length - buffer_plot + 1):(length(y) - buffer_length + buffer_plot + 1)]
  
  par(plt = c(0.1171429, 0.9400000, 0.0850000, 0.9316667))
  plot(x_plot, y_plot, pch = 23, cex = 2, ylim = ylim, xlab = "", ylab = "")
  if (back_down) {
    grey_x = c(0, 0, 2*event_length+top_length, 2*event_length+top_length) + buffer_length + 0.5
  } else {
    grey_x = c(0, 0, event_length, event_length) + buffer_length + 0.5
  }
  grey_y = ylim[c(1,2,2,1)]
  grey_y[c(1,4)] = grey_y[c(1,4)] - 0.25
  grey_y[c(2,3)] = grey_y[c(2,3)] + 0.25
  grey_color = "#CCCCCCB2"
  polygon(x = grey_x - (buffer_length - buffer_plot), y = grey_y, col = grey_color, border = NA)
  
  PID_values = calculate_PID(y, window_size)
  
  grey_region = seq(ceiling(grey_x[2]), floor(grey_x[3]))
  storage = c(max(PID_values$P[grey_region], na.rm = T), max(PID_values$I[grey_region], na.rm = T), max(PID_values$D[grey_region], na.rm = T))
  # browser()
  
  # make barplots
  par(plt = c(0.16,0.2, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  
  names(storage) = c("P", "I", "D")
  storage[1:2][storage[1:2] > max_PID] = max_PID
  barplot(storage[1], ylim = c(0, max_PID), col = "blue", yaxt = "n", main = "")
  axis(2, at = c(0,0.25,0.5), labels = c(0,0.25,">0.5"), tick = F, line = -1)
  
  x_offset = 0.08
  par(plt = c(0.16+x_offset*1,0.2+x_offset*1, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  barplot(storage[2], ylim = c(0, max_PID), col = "forestgreen", yaxt = "n", main = "")
  axis(2, at = c(0,0.25,0.5), labels = c(0,0.25,">0.5"), tick = F, line = -1)
  
  par(plt = c(0.16+x_offset*2,0.2+x_offset*2, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  max_D = 1
  storage[[3]][storage[[3]] > max_D] = max_D
  barplot(storage[3], ylim = c(0, max_D), col = "red", yaxt = "n", main = "")
  axis(2, at = c(0,0.5,1), labels = c(0,0.5,">1"), tick = F, line = -1)
  
  return(PID_values)
}

ylim = c(0, 20)
window_size = 10
event_length = 5

PID_values = vector("list", 4)

pdf("PID_examples.pdf", height = 8, width = 5)
par(mfrow = c(4,1))
par(new = F)
PID_values[[1]] = make_pair_plot(window_size = window_size, start_level = 1, end_level = 15, event_length = event_length, ylim = ylim, y_shift = 0)

PID_values[[2]] = make_pair_plot(window_size = window_size, start_level = 0.1, end_level = 0.1, event_length = event_length, ylim = ylim, y_shift = 0, rnorm_sd = 0.1)

PID_values[[3]] = make_pair_plot(window_size = window_size, start_level = 17, end_level = 17, event_length = event_length, ylim = ylim, y_shift = 0.3, rnorm_sd = 0.7)

PID_values[[4]] = make_pair_plot(window_size = window_size, start_level = 2, end_level = 3, event_length = 0, buffer_plot = 12, ylim = ylim, y_shift = 0, back_down = T, top_length = 1)

dev.off()
