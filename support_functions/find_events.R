find_events <- function(P_values, I_values, R_values, linear_values, data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, R=1.333, p_value=0.001, linear_coeff=0.1, dws=0.001), I_length = 8, ramp_length = 10, grow_window = T) {
  
  library(lmtest)
  
  num_col = ncol(data_mat)
  num_row = nrow(data_mat)
  
  P_events = P_values * 0
  P_events[P_values >= cutoffs$P] = 1
  P_events[P_values <= -cutoffs$P] = -1
  I_events = I_values * 0
  I_events[I_values >= cutoffs$I] = 1
  I_events[I_values <= -cutoffs$I] = -1
  R_events = R_values * 0
  R_events[R_values >= cutoffs$R] = 1
  R_events[R_values <= -(1-(1 / (1+cutoffs$R)))] = -1
  
  # process to find events
  event_locations = data_mat * 0
  
  event_locations[(P_events == 1) & (I_events == 1) & (R_events == 1)] = 1
  event_locations[(P_events == -1) & (I_events == -1) & (R_events == -1)] = -1
  
  #################################################################
  
  if (is.null(linear_values)) { # not run during optimize_thresholds
    p_values = NULL
    betas = NULL
    dws = NULL
  } else {
    p_values = linear_values$R_values
    betas = linear_values$betas
    dws = linear_values$dws
    for (n_col in 1:num_col) {
      # upramp
      upramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] >= cutoffs$linear_coeff & dws[,n_col] >= cutoffs$dws)
      # reformat upramps
      upramp_indices = unique(unlist(sapply(upramp_indices, function(i) i:max(1, i - ramp_length + 1), simplify = F))) # extend ramp events to length of ramp
      
      # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
      upramp_upswing_indices = upramp_indices[upramp_indices %in% which(event_locations[, n_col] == 1)]
      # check that event -1 exists (rare) and flag for event 1.5
      upramp_downswing_indices = upramp_indices[upramp_indices %in% which(event_locations[, n_col] == -1)]
      
      event_locations[upramp_indices, n_col] = 2
      event_locations[upramp_upswing_indices, n_col] = 3
      event_locations[upramp_downswing_indices, n_col] = 1.5
      
      # downramp
      downramp_indices = which(p_values[,n_col] <= cutoffs$p_value & betas[,n_col] <= -cutoffs$linear_coeff & dws[,n_col] >= cutoffs$dws)
      # reformat all upramps
      downramp_indices = unique(unlist(sapply(downramp_indices, function(i) i:max(1, i - ramp_length + 1), simplify = F))) # extend ramp events to length of ramp
      
      # check that event 1 exists and flag for event 3.  Otherwise mark as event 2
      downramp_downswing_indices = downramp_indices[downramp_indices %in% which(event_locations[, n_col] == -1)]
      # check that event +1 exists (rare) and flag for event -1.5
      downramp_upswing_indices = downramp_indices[downramp_indices %in% which(event_locations[, n_col] == 1)]
      
      event_locations[downramp_indices, n_col] = -2
      event_locations[downramp_downswing_indices, n_col] = -3
      event_locations[downramp_upswing_indices, n_col] = -1.5
      
    }
  }
  
  return(list(event_locations = event_locations, R_values = R_values, R_events = R_events, I_values = I_values, I_events = I_events, P_values = P_values, P_events = P_events, p_values = p_values, betas = betas, dws = dws))
}
