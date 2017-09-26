find_events <- function(P_values, I_values, D_values, linear_values, data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, D=1.333, p_value=0.001, b_min=0.1, dwp=0.001), I_length = 8, ramp_window = 10, grow_window = T) {
  
  library(lmtest)
  
  num_col = ncol(data_mat)
  num_row = nrow(data_mat)
  
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
  
  p_values = linear_values$p_values
  betas = linear_values$betas
  dwp = linear_values$dwp
  for (n_col in 1:num_col) {
    # upramp
    upramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] >= cutoffs$b_min & dwp[,n_col] >= cutoffs$dwp)
    
    # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
    upramp_upswing_indices = upramp_indices[event_locations[upramp_indices, n_col] == 1]
    # check that event -1 exists (rare) and flag for event 1.5
    upramp_downswing_indices = upramp_indices[event_locations[upramp_indices, n_col] == -1]
    # reformat upramps
    upramp_indices = unique(unlist(sapply(upramp_indices, function(i) i:max(1, i - ramp_window + 1), simplify = F))) # extend ramp events to length of ramp
    event_locations[upramp_indices, n_col] = 2
    event_locations[upramp_upswing_indices, n_col] = 3
    event_locations[upramp_downswing_indices, n_col] = 1.5
    
    # downramp
    downramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] <= -cutoffs$b_min & dwp[,n_col] >= cutoffs$dwp)
    
    # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
    downramp_downswing_indices = downramp_indices[event_locations[downramp_indices, n_col] == -1]
    # check that event +1 exists (rare) and flag for event -1.5
    downramp_upswing_indices = downramp_indices[event_locations[downramp_indices, n_col] == 1]
    # reformat all upramps
    downramp_indices = unique(unlist(sapply(downramp_indices, function(i) i:max(1, i - ramp_window + 1), simplify = F))) # extend ramp events to length of ramp
    event_locations[downramp_indices, n_col] = -2
    event_locations[downramp_downswing_indices, n_col] = -3
    event_locations[downramp_upswing_indices, n_col] = -1.5
    
  }
  
  return(list(event_locations = event_locations, P_values = P_values, P_events = P_events, I_values = I_values, I_events = I_events, D_values = D_values, D_events = D_events, p_values = p_values, betas = betas, dwp = dwp))
}
