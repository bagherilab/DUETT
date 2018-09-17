<<<<<<< HEAD
finP_events_optimize <- function(P_values, I_values, R_values, data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, R=1.333), I_length = 8, R_events = NULL, I_events = NULL, P_events = NULL) {
  
  if (is.null(P_events)) {
    P_events_plus = which(R_values >= cutoffs$P, arr.ind = T)
    if (length(P_events_plus) != 0) P_events_plus = cbind(P_events_plus, rep(1, nrow(P_events_plus))) # add column of +1
    P_events_minus = which(R_values <= cutoffs$P, arr.ind = T)
    if (length(P_events_minus) != 0) P_events_minus = cbind(P_events_minus, rep(-1, nrow(P_events_minus))) # add column of +1
=======
find_events_optimize <- function(P_values, I_values, D_values, data_mat, window_size = 10, cutoffs = list(P=0.6, I=1, D=1.333), I_length = 8, P_events = NULL, I_events = NULL, D_events = NULL) {
  
  if (is.null(P_events)) {
    
    P_events_plus = which(P_values >= cutoffs$P, arr.ind = T)
    if (length(P_events_plus) != 0)P_events_plus = cbind(P_events_plus, rep(1, nrow(P_events_plus))) # add column of +1
    P_events_minus = which(P_values <= -(1-(1 / (1+cutoffs$P))), arr.ind = T)
    if (length(P_events_minus) != 0) P_events_minus = cbind(P_events_minus, rep(-1, nrow(P_events_minus))) # add column of -1
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
    P_events = rbind(P_events_plus, P_events_minus)
  }
  
  if (is.null(I_events)) {
    
    I_events_plus = which(I_values >= cutoffs$I, arr.ind = T)
    if (length(I_events_plus) != 0) I_events_plus = cbind(I_events_plus, rep(1, nrow(I_events_plus))) # add column of +1
    I_events_minus = which(I_values <= cutoffs$I, arr.ind = T)
    if (length(I_events_minus) != 0) I_events_minus = cbind(I_events_minus, rep(-1, nrow(I_events_minus))) # add column of +1
    I_events = rbind(I_events_plus, I_events_minus)
  }
  
<<<<<<< HEAD
  if (is.null(R_events)) {
    
    R_events_plus = which(P_values >= cutoffs$R, arr.ind = T)
    if (length(R_events_plus) != 0)R_events_plus = cbind(R_events_plus, rep(1, nrow(R_events_plus))) # add column of +1
    R_events_minus = which(P_values <= -(1-(1 / (1+cutoffs$R))), arr.ind = T)
    if (length(R_events_minus) != 0) R_events_minus = cbind(R_events_minus, rep(-1, nrow(R_events_minus))) # add column of -1
    R_events = rbind(R_events_plus, R_events_minus)
=======
  if (is.null(D_events)) {
    # D_events = D_values * 0
    # D_events[D_values >= cutoffs$D] = 1
    # D_events[D_values <= -cutoffs$D] = -1
    
    D_events_plus = which(D_values >= cutoffs$D, arr.ind = T)
    if (length(D_events_plus) != 0) D_events_plus = cbind(D_events_plus, rep(1, nrow(D_events_plus))) # add column of +1
    D_events_minus = which(D_values <= cutoffs$D, arr.ind = T)
    if (length(D_events_minus) != 0) D_events_minus = cbind(D_events_minus, rep(-1, nrow(D_events_minus))) # add column of +1
    D_events = rbind(D_events_plus, D_events_minus)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  }
  
  # process to find events
  event_locations = data_mat * 0
  
<<<<<<< HEAD
  # add 1/3 or subtract 1/3 for each PIR
=======
  # add 1/3 or subtract 1/3 for each PID
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  event_locations[P_events[P_events[,3] == 1,1:2]] = event_locations[P_events[P_events[,3] == 1,1:2]] + 1/3
  event_locations[P_events[P_events[,3] == -1,1:2]] = event_locations[P_events[P_events[,3] == -1,1:2]] - 1/3
  event_locations[I_events[I_events[,3] == 1,1:2]] = event_locations[I_events[I_events[,3] == 1,1:2]] + 1/3
  event_locations[I_events[I_events[,3] == -1,1:2]] = event_locations[I_events[I_events[,3] == -1,1:2]] - 1/3
<<<<<<< HEAD
  event_locations[R_events[R_events[,3] == 1,1:2]] = event_locations[R_events[R_events[,3] == 1,1:2]] + 1/3
  event_locations[R_events[R_events[,3] == -1,1:2]] = event_locations[R_events[R_events[,3] == -1,1:2]] - 1/3
=======
  event_locations[D_events[D_events[,3] == 1,1:2]] = event_locations[D_events[D_events[,3] == 1,1:2]] + 1/3
  event_locations[D_events[D_events[,3] == -1,1:2]] = event_locations[D_events[D_events[,3] == -1,1:2]] - 1/3
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
  
  # remove anything with magnitude less than 1 (turn to 0)
  event_locations[abs(event_locations) < 1] = 0
  
<<<<<<< HEAD
  return(list(event_locations = event_locations, P_events = P_events, I_events = I_events, R_events = R_events))
=======
  return(list(event_locations = event_locations, P_events = P_events, I_events = I_events, D_events = D_events))
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
}
