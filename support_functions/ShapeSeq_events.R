#######

list.of.packages <- c("RColorBrewer", "lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#######

ShapeSeq_events <- function(P_values, I_values, D_values, linear_values, data_mat, window_size = 9, I_length = 9, ramp_length = 40, noise_length = 2, event_gap = 1, cutoffs = list(P = 1/4, I = 0.8, D = 0.8, p_value = 0.0001, linear_coeff = 0.15, dwp = 0.01), numbering = T, numbering_interval = 5, diverging = F) {
  
  source("support_functions/plotting/make_visual.R")
  source("support_functions/plotting/make_visual_PID.R")
  source("support_functions/plotting/plot_dump_PID.R")
  source("support_functions/find_events.R")
  
  #############################################
  grow_window = F
  
  return_list = find_events(P_values, I_values, D_values, linear_values, data_mat, window_size, cutoffs, I_length = I_length, ramp_length = ramp_length, grow_window = grow_window, diverging = diverging)
  
  event_locations = return_list$event_locations
  event_details = return_list[2:10]
  
  flip_indices = nrow(data_mat):1
  
  # clean events
  event_locations = clean_events(event_locations, noise_length = noise_length, event_gap = event_gap)
  
  return(list(event_locations, event_details))
}


