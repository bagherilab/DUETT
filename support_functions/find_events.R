find_events <- function(data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, D=1.333, p_value=0.001, b_min=0.1, dwp=0.001), I_length = 8, ramp_window = 10, grow_window = T) {
  
  if(!("lmtest" %in% installed.packages())) install.packages("lmtest")
  library(lmtest)
  source("support_functions/utility_functions.R")
  
  num_col = ncol(data_mat)
  num_row = nrow(data_mat)
  
  # gather all model predictions
  I_values = matrix(NA, nrow = num_row, ncol = num_col)
  D_values = I_values
  for (n_col in 1:num_col) {
    data_vector = data_mat[,n_col]
    
    D_values[,n_col] = diff_data(data_vector, window_size, grow_window = grow_window)
    I_values[(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(D_values[i:(i+I_length),n_col])) / I_length
  }
  P_values = D_values / (data_mat - D_values)
  
  P_events = P_values * 0
  P_events[P_values >= cutoffs$P] = 1
  P_events[P_values <= -(1-(1 / (1+cutoffs$P)))] = -1
  I_events = I_values * 0
  I_events[I_values >= cutoffs$I] = 1
  I_events[I_values <= -cutoffs$I] = -1
  D_events = D_values * 0
  D_events[D_values >= cutoffs$D] = 1
  D_events[D_values <= -cutoffs$D] = -1
  
  # process to find events
  event_locations = data_mat * 0
  
  event_locations[(P_events == 1) & (D_events == 1) & (I_events == 1)] = 1
  event_locations[(P_events == -1) & (D_events == -1) & (I_events == -1)] = -1
  
  #################################################################
  
  p_values = data_mat * NA
  betas = p_values
  dwp = p_values
  for (n_col in 1:num_col) {
    for (n_window in 1:(num_row - ramp_window - 1)) {
      x = 1:ramp_window
      y = data_mat[n_window:(n_window + ramp_window - 1), n_col]
      
      if (!any(is.na(y))) {
        lm_obj = lm(y ~ x)
        lm_summary = summary(lm_obj)
        
        p_values[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,4]]
        betas[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,1]]
        dwp[[n_window + ramp_window - 1, n_col]] = dwtest(y ~ x)$p.value
      }
    }
    
    # upramp
    upramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] >= cutoffs$b_min & dwp[,n_col] >= cutoffs$dwp)
    
    # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
    upramp_e1_indices = upramp_indices[event_locations[upramp_indices, n_col] == 1]
    upramp_indices = unique(unlist(sapply(upramp_indices, function(i) i:max(1, i - ramp_window + 1), simplify = F))) # extend ramp events to length of ramp
    event_locations[upramp_indices, n_col] = 2
    event_locations[upramp_e1_indices, n_col] = 3
    
    # downramp
    downramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] <= -cutoffs$b_min & dwp[,n_col] >= cutoffs$dwp)
    
    # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
    downramp_e1_indices = downramp_indices[event_locations[downramp_indices, n_col] == -1]
    downramp_indices = unique(unlist(sapply(downramp_indices, function(i) i:max(1, i - ramp_window + 1), simplify = F))) # extend ramp events to length of ramp
    event_locations[downramp_indices, n_col] = -2
    event_locations[downramp_e1_indices, n_col] = -3
    
  }
  
  return(list(event_locations = event_locations, P_values = P_values, P_events = P_events, I_values = I_values, I_events = I_events, D_values = D_values, D_events = D_events, p_values = p_values, betas = betas, dwp = dwp))
}
