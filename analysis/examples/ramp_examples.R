# PID examples
set.seed(1234)

# setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")
setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")

rm(list=ls())

source("helper_functions.R")
source("../../support_functions/utility_functions.R")

make_pair_plot <- function(ramp_length, start_level, end_level, event_length, buffer_length = 10, buffer_plot = buffer_length, rnorm_sd = 0.1, ylim = c(0, end_level+1), max_PID = 0.5, y_shift = 0, back_down = F, top_length = 1) {
  
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
  print(grey_x)
  polygon(x = grey_x, y = grey_y, col = grey_color, border = NA)
  
  par(plt = c(0.16,0.33, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  
  grey_region = seq(ceiling(grey_x[2]), floor(grey_x[3]))
  ramp_values = calculate_ramp(y[grey_region], ramp_length)
  storage = c(max(ramp_values$p_values[grey_region], na.rm = T), max(ramp_values$betas[grey_region], na.rm = T), max(ramp_values$dws[grey_region], na.rm = T))
  
  # browser()
  # storage = c(mean(ramp_values$P[grey_region], na.rm = T), mean(ramp_values$I[grey_region], na.rm = T), mean(ramp_values$D[grey_region], na.rm = T))
  names(storage) = c("p_value", "beta", "dws")
  storage[[3]] = storage[[3]] / 2
  storage[storage > max_PID] = max_PID
  barplot(storage, ylim = c(0, max_PID), col = c("blue", "forestgreen", "red"), yaxt = "n", main = "")
  # title(main = "Max value", line = 0.5)
  axis(2, at = c(0,0.25,0.5), labels = c(0,0.5,">1"), tick = F, line = -1)
  axis(4, at = c(0,0.25,0.5), labels = c(0,1,">2"), tick = F, line = -1)
  # return(storage)
}

ylim = c(0, 10)
ramp_length = 10
event_length = 20
start_level = 0.5

pdf("ramp_examples.pdf", height = 8, width = 5)
par(mfrow = c(4,1))
par(new = F)
make_pair_plot(ramp_length = ramp_length, start_level = start_level, end_level = 10, event_length = event_length, ylim = ylim, y_shift = 0)

make_pair_plot(ramp_length = ramp_length, start_level = 3, end_level = 8, event_length = event_length, ylim = ylim, y_shift = 0, rnorm_sd = 3)

make_pair_plot(ramp_length = ramp_length, start_level = 2, end_level = 2, event_length = event_length, ylim = ylim, y_shift = 0.3, rnorm_sd = 1)

# make_pair_plot(ramp_length = ramp_length, start_level = start_level, end_level = 8, event_length = 0, ylim = ylim, y_shift = 0, back_down = T, top_length = 1)

dev.off()
