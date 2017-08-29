make_visual <- function(data_mat, event_locations, color_scale = NULL, numbering = T, numbering_interval = 5, lwd = 0.01, fg = "black", circle_size_override = NULL) {
  
  source("support_functions/utility_functions.R")
  source("support_functions/color_to_hex.R")
  source("support_functions/plotting/make_boxes.R")
  source("support_functions/plotting/axis_labels.R")
  
  # set colors
  # event_colors = list(upswing = color_to_hex("red", 0.1), downswing = color_to_hex("blue", 0.1)) # transparent
  event_colors = list(upswing = color_to_hex("red", 0), downswing = color_to_hex("blue", 0)) # not transparent
  clear_color = color_to_hex("white", 1)
  library(RColorBrewer)
  grey_color = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)
  
  make_boxes(data_mat = data_mat, color_scale = grey_color, lwd = lwd, fg = clear_color)
  if (numbering) {axis_labels(n_row = nrow(data_mat), n_col = ncol(data_mat), numbering_interval = numbering_interval)}
  
  par(new = T)
  make_visual_PID(event_locations, event_draw = c(-1,1, -3,3), ramp_draw = c(-1,1), event_colors = event_colors)
  par(new = F)
  
}