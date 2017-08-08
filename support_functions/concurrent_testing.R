rm(list=ls())

browser()
setwd("~/Dropbox/Albert Xue/Research/Shape_Seq/testing/PID/")

load("PID_events.RData")
load("../../data/SRPECLI_BZCN_0001.RData")
source("../../bubble_chart/make_visual.R")
source("../../bubble_chart/make_visual_PID.R")
source("utility_functions.R")
source("find_concurrent_events.R")

# set colors
source("../../../SAMDI/functions/color_to_hex.R")
event_colors = c(color_to_hex("red", 0.1), downswing_color = color_to_hex("blue", 0.1))
clear_color = color_to_hex("white", 1)
grey_color = color_to_hex("grey", 0.3)
line_color = color_to_hex("forestgreen", 0.6)
library(RColorBrewer)
grey_color_ramp = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)

############################################################################

make_lines <- function(concurrent_events_arg) {
  row_format <- function(row_pair) {
    ifelse(row_pair[[1]]>row_pair[[2]], yes = row_pair[[1]]<-row_pair[[1]]-1, no = row_pair[[2]]<-row_pair[[2]]-1)
    return(row_pair)
  }
  
  if (nrow(concurrent_events_arg) > 0) {
    for (n_row in 1:nrow(concurrent_events_arg)) {
      col_order = order(concurrent_events_arg[n_row, c(2,4)], decreasing = F) * 2
      x1 = min(row_format(concurrent_events_arg[n_row, c(2,4)]))
      x2 = max(row_format(concurrent_events_arg[n_row, c(2,4)]))
      y1 = 96 - concurrent_events_arg[n_row, col_order[[1]] - 1] + 1
      y2 = 96 - concurrent_events_arg[n_row, col_order[[2]] - 1] + 1
      
      lines(x = c(x1, x2, x2 + 1), y = c(y1, y1, y2), lwd = 3, col = line_color)
    }
  }
}

make_plot <- function(orientation = 1) {
  event_draw_list = list(c(-3,-1,1,3), NULL, c(-3,-1,1,3))
  ramp_draw_list = list(NULL, c(-1,1), c(-1,1))
  
  for (n_set in 1:3) {
    for (n_ori in orientation) {
      ifelse(n_ori == 1, event_colors_temp <- event_colors, event_colors_temp <- rev(event_colors))
      make_visual_PID(event_locations = event_locations[[n_ori]], event_draw = event_draw_list[[n_set]], ramp_draw = ramp_draw_list[[n_set]], event_colors = event_colors_temp, label_axes = T)
      par(new = T)
      make_lines(concurrent_events[[n_set]][[n_ori]])
      par(new = T)
    }
    par(new = F)
  }
}

max_concurrent_distance = 3
comparison_point = "start"
concurrent_events = list(
  find_concurrent_events(max_concurrent_distance, comparison_point, event_types = c(-3,-1,1,3), merge_3 = 1),
  find_concurrent_events(max_concurrent_distance, comparison_point, event_types = c(-3,-2,2,3), merge_3 = 2),
  find_concurrent_events(max_concurrent_distance, comparison_point, event_types = c(-3,-2,-1,1,2,3), merge_3 = 2))

############################################################################
# plotting

block_lwd = 1

pdf("concurrent_events.pdf", width = 24, height = 8)

par(mfrow = c(1,3))
resize = 2
xlim = c(0, ncol(data_storage$reactivity)+1)
ylim = c(-1, nrow(data_storage$reactivity))

# forward only
make_plot(orientation = 1)

# backwards only
make_plot(orientation = 2)

# both directions
make_plot(orientation = 1:2)

dev.off()
