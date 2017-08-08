make_visual <- function(data_mat, color_scale = NULL, label_axes = T, lwd = 0.2, fg = "black", circle_size_override = NULL) {
  source("support_functions/plotting/bubble_chart.R")
  source("support_functions/plotting/axis_labels.R")
  if (is.null(color_scale)) {
    library(RColorBrewer)
    color_scale = colorRampPalette(brewer.pal(n = 7, name ="Reds"))(100)
  }
  
  if (!is.null(circle_size_override)) {
    circle_sizes = circle_size_override
  } else {
    circle_sizes = data_mat
  }
  circle_colors = data_mat / max(data_mat, na.rm = T)
  if (min(data_mat, na.rm = T)) {
    data_mat = data_mat + 0.00001
  }
  
  bubble_chart(circle_colors = circle_colors, circle_sizes = circle_sizes, color_scale = color_scale, lwd = lwd, shape = "square", fg = fg)
  
  if (label_axes) {axis_labels(n_row = nrow(data_mat), n_col = ncol(data_mat))}
}