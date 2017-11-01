axis_labels <- function(n_row, n_col, numbering_interval = 5, axis_label_resize = 1) {
  make_sequence <- function(start_value, end_value, interval_arg = numbering_interval) {
    seq_var = c(start_value, seq(start_value + (abs(interval_arg) - 1) * sign(interval_arg), end_value, by = interval_arg))
    while(any(seq_var >= 100)) {
      seq_var[seq_var >= 100] = seq_var[seq_var >= 100] - 100
    }
    return(seq_var)
  }
  
  if (axis_label_resize != 0) {
    # left labels
    text(rep(1, floor(n_row/numbering_interval)), c(n_row, seq(n_row - (numbering_interval-1), 1, -numbering_interval)), labels = 20+make_sequence(1, n_row), cex = 1.5 * axis_label_resize, pos = 2)
    # right labels
    text(rep(n_col, floor(n_row/numbering_interval)), c(n_row, seq(n_row - (numbering_interval-1), 1, -numbering_interval)), labels = 20+make_sequence(1, n_row), cex = 1.5 * axis_label_resize, pos = 4)
    # top labels
    text(c(1, seq(numbering_interval, n_col, by = numbering_interval)), rep(n_row, floor(n_col / numbering_interval)), make_sequence(1, n_col), cex = 1.5 * axis_label_resize, pos = 3)
    # bottom labels
    text(c(1, seq(numbering_interval, n_col, by = numbering_interval)), rep(1, floor(n_col / numbering_interval)), make_sequence(1, n_col), cex = 1.5 * axis_label_resize, pos = 1)
  }
}