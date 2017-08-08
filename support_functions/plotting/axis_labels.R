axis_labels <- function(n_row, n_col, interval = 5, cex = 1.5) {
  make_sequence <- function(start_value, end_value, interval_arg = interval) {
    seq_var = c(start_value, seq(start_value + (abs(interval_arg) - 1) * sign(interval_arg), end_value, by = interval_arg))
    while(any(seq_var >= 100)) {
      seq_var[seq_var >= 100] = seq_var[seq_var >= 100] - 100
    }
    return(seq_var)
  }
  
  # left labels
  text(rep(-2, floor(n_row/interval)), make_sequence(n_row, 1, -interval), labels = make_sequence(1, n_row), cex = cex)
  # top labels
  text(c(1, seq(interval, n_col, by = interval)), rep(n_row + 2, floor(n_col / interval)), make_sequence(1, n_col), cex = cex)
  # bottom labels
  text(c(1, seq(interval, n_col, by = interval)), rep(-1, floor(n_col / interval)), make_sequence(1, n_col), cex = cex)
  # diagonal labels
  text(29 + c(1, seq(interval, n_row, by=interval)), c(n_row, seq(92, 1, by=-interval)), labels = c(1, seq(interval, n_row, by=interval)), cex = cex)
}