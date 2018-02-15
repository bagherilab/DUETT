make_visual_concurrent <- function(concurrent_events, num_rows, lwd = 4) {
  row_format <- function(row_pair) {
    ifelse(row_pair[[1]]>row_pair[[2]], yes = row_pair[[1]]<-row_pair[[1]]-1, no = row_pair[[2]]<-row_pair[[2]]-1)
    return(row_pair)
  }
  
  source("support_functions/color_to_hex.R")
  line_color = color_to_hex("forestgreen", 0.4)
  # line_color = color_to_hex("orange", 0.4)
  
  if (nrow(concurrent_events) > 0) {
    for (n_row in 1:nrow(concurrent_events)) {
      col_order = order(concurrent_events[n_row, c(2,4)], decreasing = F) * 2
      x1 = min(row_format(concurrent_events[n_row, c(2,4)]))
      x2 = max(row_format(concurrent_events[n_row, c(2,4)]))
      y1 = num_rows - concurrent_events[n_row, col_order[[1]] - 1] + 1
      y2 = num_rows - concurrent_events[n_row, col_order[[2]] - 1] + 1
      
      lines(x = c(x1, x2, x2 + 1), y = c(y1, y1, y2), lwd = lwd, col = line_color)
    }
  }
}