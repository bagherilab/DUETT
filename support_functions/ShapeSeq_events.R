#######

list.of.packages <- "RColorBrewer"
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#######

ShapeSeq_events <- function(data_mat, window_size = 9, I_length = 9, ramp_window = 40, noise_length = 2, event_gap = 1, cutoffs = list(P = 1/4, I = 0.8, D = 0.8, p_value = 0.01 / (96*124), b_min = 0.15, dwp = 0.01)) {
  
  source("support_functions/plotting/make_visual.R")
  source("support_functions/plotting/make_visual_PID.R")
  source("support_functions/plotting/plot_dump_PID.R")
  source("support_functions/find_events.R")
  
  #############################################
  grow_window = F
  
  return_list = find_events(data_mat, window_size, cutoffs, I_length = I_length, ramp_window = ramp_window, grow_window = grow_window)
  
  event_locations = return_list$event_locations
  event_details = return_list[2:10]
  
  flip_indices = nrow(data_mat):1
  # return_list = find_events(data_mat[flip_indices,], window_size, cutoffs, I_length = I_length, ramp_window = ramp_window, grow_window = grow_window)
  
  # event_locations = list(event_locations, return_list$event_locations[flip_indices,])
  # event_details = list(event_details, lapply(return_list[2:10], function(i) i[flip_indices,]))
  event_locations = list(event_locations, list())
  event_details = list(event_details, list())
  
  # clean event object
  event_locations[[1]] = clean_events(event_locations[[1]], noise_length = noise_length, event_gap = event_gap, event_types = c(-3,-1,1,3))
  # event_locations[[2]] = clean_events(event_locations[[2]], noise_length = noise_length, event_gap = event_gap, event_types = c(-3,-1,1,3))
  
  event_locations[[1]] = clean_events(event_locations[[1]], noise_length = 0, event_gap = event_gap, event_types = c(-3,-2,2,3))
  # event_locations[[2]] = clean_events(event_locations[[2]], noise_length = 0, event_gap = event_gap, event_types = c(-3,-2,2,3))
  
  return(list(event_locations, event_details))
}

##########################################
# make heatmap with events

heatmap_plot <- function(data_mat, event_locations) {
  
  source("support_functions/utility_functions.R")
  source("support_functions/color_to_hex.R")
  
  # set colors
  # event_colors = list(upswing = color_to_hex("red", 0.1), downswing = color_to_hex("blue", 0.1)) # transparent
  event_colors = list(upswing = color_to_hex("red", 0), downswing = color_to_hex("blue", 0)) # not transparent
  clear_color = color_to_hex("white", 1)
  library(RColorBrewer)
  grey_color = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)
  
  make_triple_plot <- function(orientation = 1) {
    
    make_visual(data_mat, color_scale = grey_color, lwd = lwd, fg = clear_color)
    par(new = T)
    for (n_ori in orientation) {
      ifelse(n_ori == 1, event_colors_temp <- event_colors, event_colors_temp <- rev(event_colors))
      make_visual_PID(event_locations[[n_ori]], event_draw = c(-1,1, -3,3), ramp_draw = c(-1,1), event_colors = event_colors_temp, label_axes = F)
    }
    par(new = F)
  }
  
  lwd = 0.01
  
  make_triple_plot(orientation = 1)
}

