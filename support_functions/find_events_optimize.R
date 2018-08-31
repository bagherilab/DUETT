find_events_optimize <- function(P_values, I_values, D_values, data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, D=1.333), I_length = 8, P_events = NULL, I_events = NULL, D_events = NULL) {
  
  if (is.null(P_events)) {
    
    P_events_plus = which(P_values >= cutoffs$P, arr.ind = T)
    if (length(P_events_plus) != 0)P_events_plus = cbind(P_events_plus, rep(1, nrow(P_events_plus))) # add column of +1
    P_events_minus = which(P_values <= -(1-(1 / (1+cutoffs$P))), arr.ind = T)
    if (length(P_events_minus) != 0) P_events_minus = cbind(P_events_minus, rep(-1, nrow(P_events_minus))) # add column of -1
    P_events = rbind(P_events_plus, P_events_minus)
  }
  
  if (is.null(I_events)) {
    
    I_events_plus = which(I_values >= cutoffs$I, arr.ind = T)
    if (length(I_events_plus) != 0) I_events_plus = cbind(I_events_plus, rep(1, nrow(I_events_plus))) # add column of +1
    I_events_minus = which(I_values <= cutoffs$I, arr.ind = T)
    if (length(I_events_minus) != 0) I_events_minus = cbind(I_events_minus, rep(-1, nrow(I_events_minus))) # add column of +1
    I_events = rbind(I_events_plus, I_events_minus)
  }
  
  if (is.null(D_events)) {
    # D_events = D_values * 0
    # D_events[D_values >= cutoffs$D] = 1
    # D_events[D_values <= -cutoffs$D] = -1
    
    D_events_plus = which(D_values >= cutoffs$D, arr.ind = T)
    if (length(D_events_plus) != 0) D_events_plus = cbind(D_events_plus, rep(1, nrow(D_events_plus))) # add column of +1
    D_events_minus = which(D_values <= cutoffs$D, arr.ind = T)
    if (length(D_events_minus) != 0) D_events_minus = cbind(D_events_minus, rep(-1, nrow(D_events_minus))) # add column of +1
    D_events = rbind(D_events_plus, D_events_minus)
  }
  
  # process to find events
  event_locations = data_mat * 0
  
  # add 1/3 or subtract 1/3 for each PID
  event_locations[P_events[P_events[,3] == 1,1:2]] = event_locations[P_events[P_events[,3] == 1,1:2]] + 1/3
  event_locations[P_events[P_events[,3] == -1,1:2]] = event_locations[P_events[P_events[,3] == -1,1:2]] - 1/3
  event_locations[I_events[I_events[,3] == 1,1:2]] = event_locations[I_events[I_events[,3] == 1,1:2]] + 1/3
  event_locations[I_events[I_events[,3] == -1,1:2]] = event_locations[I_events[I_events[,3] == -1,1:2]] - 1/3
  event_locations[D_events[D_events[,3] == 1,1:2]] = event_locations[D_events[D_events[,3] == 1,1:2]] + 1/3
  event_locations[D_events[D_events[,3] == -1,1:2]] = event_locations[D_events[D_events[,3] == -1,1:2]] - 1/3
  
  # remove anything with magnitude less than 1 (turn to 0)
  event_locations[abs(event_locations) < 1] = 0
  
  return(list(event_locations = event_locations, P_events = P_events, I_events = I_events, D_events = D_events))
}
