#######

list.of.packages <- c("RColorBrewer", "lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#######

DUETT_events <- function(P_values, I_values, R_values, linear_values, data_mat, window_size = 9, I_length = 9, ramp_length = 40, duration = 2, event_gap = 1, cutoffs = list(P = 1/4, I = 0.8, R = 0.8, p_value = 0.0001, linear_coeff = 0.15, dws = 0.01), numbering = T, numbering_interval = 5) {
  
  source("support_functions/utility_functions.R")
  source("support_functions/find_events.R")
  
  #############################################
  grow_window = F
  
  return_list = find_events(P_values, I_values, R_values, linear_values, data_mat, window_size, cutoffs, I_length = I_length, ramp_length = ramp_length, grow_window = grow_window)
  
  event_locations = return_list$event_locations
  event_details = return_list[2:10]
  
  flip_indices = nrow(data_mat):1
  
  # clean events
  event_locations = clean_events(event_locations, duration = duration, event_gap = event_gap)
  
  return(list(event_locations, event_details))
}


