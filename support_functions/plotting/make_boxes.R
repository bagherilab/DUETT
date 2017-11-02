make_boxes <- function(data_mat, color_scale = NULL, lwd = 0.01, fg = "black", circle_size_override = NULL, box_resize = 1) {
  
  source("support_functions/plotting/bubble_chart.R")
  
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
  
  # attempt to make max circle size aesthetic
  circle_sizes = (circle_sizes / max(circle_sizes, na.rm = T)) * 25 * box_resize
  
  bubble_chart(circle_colors = circle_colors, circle_sizes = circle_sizes, color_scale = color_scale, lwd = lwd, shape = "square", fg = fg)
  
}