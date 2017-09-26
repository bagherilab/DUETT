make_visual <- function(data_mat, event_locations, concurrent_events, color_scale = NULL, numbering = T, numbering_interval = 5, lwd = 0.01, fg = "black", circle_size_override = NULL) {
  
  source("support_functions/utility_functions.R")
  source("support_functions/color_to_hex.R")
  source("support_functions/plotting/make_boxes.R")
  source("support_functions/plotting/axis_labels.R")
  source("support_functions/plotting/make_visual_concurrent.R")
  
  # set colors
  event_colors = list(upswing = color_to_hex("red", 0.1), downswing = color_to_hex("blue", 0.1)) # transparent
  # event_colors = list(upswing = color_to_hex("red", 0), downswing = color_to_hex("blue", 0)) # not transparent
  clear_color = color_to_hex("white", 1)
  library(RColorBrewer)
  grey_color = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)
  
  # make grey boxes for the data
  make_boxes(data_mat = data_mat, color_scale = grey_color, lwd = lwd, fg = clear_color)
  if (numbering) {axis_labels(n_row = nrow(data_mat), n_col = ncol(data_mat), numbering_interval = numbering_interval)}
  
  # want -1.5 to be upswing events, and 1.5 to be downswing events.  swap them temporarily
  event_1.5 = event_locations == 1.5
  event_n1.5 = event_locations == -1.5
  event_locations[event_1.5] = -1.5
  event_locations[event_n1.5] = 1.5
  
  # add swing and ramp events
  par(new = T)
  make_visual_PID(event_locations, event_draw = c(-1,1, -1.5,1.5, -3,3), ramp_draw = c(-1,1), event_colors = event_colors)
  par(new = T)
  
  # swap back
  event_locations[event_1.5] = 1.5
  event_locations[event_n1.5] = -1.5
  
  # add concurent events
  num_rows = nrow(event_locations)
  make_visual_concurrent(concurrent_events, num_rows)
  par(new = F)
}