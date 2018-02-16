# PID examples
set.seed(12345)

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")
# setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/analysis/examples/")

rm(list=ls())

source("helper_functions.R")
source("../../support_functions/utility_functions.R")

make_pair_plot <- function(ramp_length, start_level, end_level, event_length, buffer_length = 10, buffer_plot = buffer_length, rnorm_sd = 0.1, ylim = c(0, end_level+1), max_PID = 0.5, y_shift = 0) {
  
  y = make_values(start_level = start_level, end_level = end_level, event_length = event_length, buffer_length = buffer_length, rnorm_sd = rnorm_sd)
  x = 1:length(y)
  
  # partially remove buffer
  x_plot = x[(buffer_length - buffer_plot + 1):(length(x) - buffer_length + buffer_plot + 1)]
  x_plot = 1:length(x_plot)
  y_plot = y[(buffer_length - buffer_plot + 1):(length(y) - buffer_length + buffer_plot + 1)]
  
  par(plt = c(0.1171429, 0.9400000, 0.0850000, 0.9316667))
  plot(x_plot, y_plot, pch = 23, cex = 2, ylim = ylim, xlab = "", ylab = "")
  
  line_region = seq(ceiling(buffer_length + 0.5), floor(event_length + buffer_length + 0.5))
  if (ramp_length > length(line_region)) {
    difference = ramp_length - length(line_region)
    line_region = c(seq(line_region[[1]] - floor(difference/2), line_region[[1]] - 1), line_region, seq(ceiling(max(line_region)) + 1, ceiling(max(line_region)) + ceiling(difference/2)))
  }
  
  # draw fitted line
  fitted_line = lm.fit(x = matrix(1:length(line_region), ncol = 1), y = matrix(y[line_region], ncol = 1))
  lines(x = line_region, y = fitted_line$fitted.values)
  
  ramp_values = calculate_ramp(y[line_region], ramp_length)
  storage = c(max(ramp_values$p_values, na.rm = T), max(ramp_values$betas, na.rm = T), max(ramp_values$dws, na.rm = T))
  
  # make barplots
  par(plt = c(0.16,0.2, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  
  names(storage) = c("p_value", "beta", "dws")
  storage[[1]] = -log10(storage[[1]])
  max_p_value = 4
  storage[[1]][storage[[1]] > max_p_value] = max_p_value
  barplot(storage[1], ylim = c(0, max_p_value), col = "blue", yaxt = "n", main = "")
  axis(2, at = c(0,2,4), labels = c("10^0","10^-2", "<10^-4"), tick = F, line = -1)
  
  x_offset = 0.08
  par(plt = c(0.16+x_offset*1,0.2+x_offset*1, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  max_beta = 1
  storage[[2]][storage[[2]] > max_beta] = max_beta
  barplot(storage[2], ylim = c(0, max_beta), col = "forestgreen", yaxt = "n", main = "")
  axis(2, at = c(0,0.5,1), labels = c(0,0.5,">1"), tick = F, line = -1)
  
  par(plt = c(0.16+x_offset*2,0.2+x_offset*2, 0.5 - y_shift,0.85 - y_shift))
  par(new = T)
  max_dws = 2
  storage[[3]][storage[[3]] > max_dws] = max_dws
  barplot(storage[3], ylim = c(0, max_dws), col = "red", yaxt = "n", main = "")
  axis(2, at = c(0,1,2), labels = c(0,1,">2"), tick = F, line = -1)
  # return(storage)
}

ylim = c(0, 21)
ramp_length = 20
event_length = 20

pdf("ramp_examples.pdf", height = 8, width = 5)
par(mfrow = c(4,1))
par(new = F)
make_pair_plot(ramp_length = ramp_length, start_level = 0, end_level = 20, buffer_length = 10, event_length = event_length, ylim = ylim, y_shift = 0)

make_pair_plot(ramp_length = ramp_length, start_level = 0, end_level = 25, buffer_length = 10, event_length = event_length, ylim = c(0, 50), y_shift = 0, rnorm_sd = 10)

make_pair_plot(ramp_length = ramp_length, start_level = 1, end_level = 5, buffer_length = 10, event_length = event_length, ylim = ylim, rnorm_sd = 1)

make_pair_plot(ramp_length = ramp_length, start_level = 0, end_level = 20, buffer_length = 18, event_length = 4, ylim = ylim)

dev.off()
