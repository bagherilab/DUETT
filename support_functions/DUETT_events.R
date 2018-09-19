#######

list.of.packages <- c("RColorBrewer", "lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#######

DUETT_events <- function(P_values, I_values, R_values, linear_values, data_mat, window_size = 9, I_length = 9, ramp_length = 40, duration = 2, event_gap = 1, cutoffs = list(P = 1/4, I = 0.8, R = 0.8, p_value = 0.0001, linear_coeff = 0.15, dws = 0.01), concurrent_distance = 2, conc_event_types) {
  
  num_files = length(P_values)
  event_storage = vector("list", num_files)
  concurrent_events = vector("list", num_files)
  
  source("support_functions/utility_functions.R")
  source("support_functions/find_events.R")
  
  for (n_file in 1:num_files) {
    
    return_list = find_events(P_values[[n_file]], I_values[[n_file]], R_values[[n_file]], linear_values[[n_file]], data_mat[[n_file]], window_size, cutoffs, I_length = I_length, ramp_length = ramp_length, grow_window = grow_window)
    
    event_locations = return_list$event_locations
    event_details = return_list[2:10]
    
    flip_indices = nrow(data_mat[[n_file]]):1
    
    # clean events
    event_locations = clean_events(event_locations, duration = duration, event_gap = event_gap)
    
    event_storage[[n_file]] = list(event_locations, event_details)
    
    concurrent_events[[n_file]] = find_concurrent_events(event_storage[[n_file]][[1]], concurrent_distance = concurrent_distance, comparison_point = "start", event_types = conc_event_types)
  }
  return_list = list(event_storage, concurrent_events)
}


