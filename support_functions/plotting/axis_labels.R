axis_labels <- function(n_row, n_col, numbering_interval = 5, cex = 1.5) {
  make_sequence <- function(start_value, end_value, interval_arg = numbering_interval) {
    seq_var = c(start_value, seq(start_value + (abs(interval_arg) - 1) * sign(interval_arg), end_value, by = interval_arg))
    while(any(seq_var >= 100)) {
      seq_var[seq_var >= 100] = seq_var[seq_var >= 100] - 100
    }
    return(seq_var)
  }
  
  # left labels
  text(rep(1, floor(n_row/numbering_interval)), make_sequence(n_row, 1, -numbering_interval), labels = make_sequence(1, n_row), cex = cex, pos = 2)
  # right labels
  text(rep(n_col, floor(n_row/numbering_interval)), make_sequence(n_row, 1, -numbering_interval), labels = make_sequence(1, n_row), cex = cex, pos = 4)
  # top labels
  text(c(1, seq(numbering_interval, n_col, by = numbering_interval)), rep(n_row, floor(n_col / numbering_interval)), make_sequence(1, n_col), cex = cex, pos = 3)
  # bottom labels
  text(c(1, seq(numbering_interval, n_col, by = numbering_interval)), rep(1, floor(n_col / numbering_interval)), make_sequence(1, n_col), cex = cex, pos = 1)
  
  # diagonal labels
  # text(29 + c(1, seq(numbering_interval, n_row, by=numbering_interval)), c(n_row, seq(92, 1, by=-numbering_interval)), labels = c(1, seq(numbering_interval, n_row, by=numbering_interval)), cex = cex)
}