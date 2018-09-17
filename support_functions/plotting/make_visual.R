make_visual <- function(data_mat, event_locations, concurrent_events, log_colors = F, color_scale = NULL, numbering = T, numbering_offset = 0, numbering_interval = 5, axis_label_resize = 1, lwd = 0.01, fg = "black", circle_size_override = NULL, box_resize = 1, diverging = F, event_colors = NULL) {
  
  source("support_functions/utility_functions.R")
  source("support_functions/color_to_hex.R")
  source("support_functions/plotting/make_boxes.R")
  source("support_functions/plotting/axis_labels.R")
  source("support_functions/plotting/make_visual_PIR.R")
  source("support_functions/plotting/make_visual_concurrent.R")
  
  if (log_colors) {
    negative_values = data_mat < 0 & !is.na(data_mat)
    positive_values = data_mat > 0 & !is.na(data_mat)
    data_mat[negative_values] = -log2(abs(data_mat[negative_values]) + 1)
    data_mat[positive_values] = log2(data_mat[positive_values] + 1)
  }
  
  # set colors
  if (is.null(event_colors)) {
    event_colors = list(upswing = color_to_hex("red", 0.1), downswing = color_to_hex("blue", 0.1)) # transparent
    # event_colors = list(upswing = color_to_hex("red", 0), downswing = color_to_hex("blue", 0)) # not transparent
  }
  clear_color = color_to_hex("white", 1)
  library(RColorBrewer)
  if (diverging) {
    library(grDevices)
    base_color = colorRamp(c("lightslateblue", "white", "lightcoral")) ((1:100)/100)
    # convert to hexcode
    base_color = apply(base_color, 1, function(i) do.call(rgb, as.list(i / 255)))
  } else {
    base_color = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)
  }
  
  # make grey boxes for the data
  make_boxes(data_mat = data_mat, color_scale = base_color, lwd = lwd, fg = clear_color, box_resize = box_resize, diverging = diverging)
  if (numbering) {axis_labels(n_row = nrow(data_mat), n_col = ncol(data_mat), numbering_offset = numbering_offset, numbering_interval = numbering_interval, axis_label_resize = axis_label_resize)}
  
  # add swing and ramp events
  par(new = T)
  make_visual_PIR(event_locations, event_draw = c(-1,1, -1.5,1.5, -3,3), ramp_draw = c(-1,1), event_colors = event_colors, box_resize = box_resize, lwd = 2)
  par(new = T)
  
  # add concurent events
  num_rows = nrow(event_locations)
  make_visual_concurrent(concurrent_events, num_rows, lwd = 3)
  par(new = F)
}