make_values <- function(start_level = 1, end_level = 2, event_length = 2, buffer_length = 40, rnorm_sd = 0.3, min_value = 0, back_down = F, top_length = 1) {
  
  if (event_length == 0) {
    ramp = c()
  } else {
    ramp = seq(start_level, end_level, length.out = event_length + 2)[2:(event_length + 1)]
  }
  
  if (back_down) {
    values = c(rep(start_level, buffer_length), ramp, rep(end_level, top_length), rev(ramp), rep(start_level, buffer_length))
  } else {
    values = c(rep(start_level, buffer_length), ramp, rep(end_level, buffer_length))
  }
  
  values = matrix(values, ncol = 1) + rnorm(length(values), sd = rnorm_sd)
  
  values[values < min_value] = min_value
  return(values)  
}



grid_values <- function(start_levels = c(0.01, 1,2,4,8), end_level_change = c(1,2,4,6), event_lengths = c(0,4,8,16)) {
  
  storage = list()
  for (n_start in start_levels) {
    for (n_end_change in end_level_change) {
      for (n_length in event_lengths) {
        storage = c(storage, list(make_values(start_level = n_start, end_level = n_start + n_end_change, event_length = n_length)))
      }
    }
  }
  
  return(storage)
  
}


calculate_PID <- function(values, window_size, I_length = window_size) {
  
  if (window_size == 0) {D_values = values * 0}
  D_values = diff_data(matrix(values, ncol = 1), window_size)
  
  num_row = length(values)
  num_col = 1
  I_values = matrix(NA, nrow = num_row, ncol = num_col)
  for (n_col in 1:ncol(D_values)) {
    # I_values[(I_length+1):(num_row-I_length),n_col] = sapply((I_length+1):(num_row-I_length), function(i) sum(D_values[i:(i+I_length),n_col])) / I_length
    # I_values[(I_length+1):(num_row-I_length),n_col] = sapply((I_length+1):(num_row-I_length), function(i) sum(D_values[i:(i+I_length),n_col])) / I_length
    I_values[(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(D_values[i:(i+I_length),n_col])) / I_length
  }
  
  P_values = D_values / abs(values - D_values)
  
  return(list(P=P_values, I=I_values, D=D_values))
}




calculate_ramp <- function(values, ramp_length) {
  library(lmtest)
  p_values = matrix(values * NA, ncol = 1)
  betas = p_values
  dws = p_values
  
  for (n_window in 1:max((nrow(p_values) - ramp_length - 1), 1)) {
    x = 1:ramp_length
    y = values[n_window:(n_window + ramp_length - 1)]
    
    if (!any(is.na(y))) {
      lm_summary = summary(lm(y ~ x))
      
      p_values[[n_window + ramp_length - 1, 1]] = lm_summary$coefficients[[2,4]]
      betas[[n_window + ramp_length - 1, 1]] = lm_summary$coefficients[[2,1]]
      
      # check that y is not all the same (dwtest doesn't like that)
      if (length(unique(y)) == 1) {
        dws[[n_window + ramp_length - 1, 1]] = Inf
      } else {
        # dws[[n_window + ramp_length - 1, 1]] = dwtest(y ~ x)$p.value
        dws[[n_window + ramp_length - 1, 1]] = dwtest(y ~ x)$statistic
      }
    }
  }
  return(list(p_values = p_values, betas = betas, dws = dws))
}