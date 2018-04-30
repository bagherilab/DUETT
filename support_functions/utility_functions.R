# dump of utility functions

############################ differentiate ############################
diff_data <- function(data_mat, window_size = 10, grow_window = F) {
  
  num_points = nrow(data_mat)
  diffed_data = data_mat * NA
  
  # edge cases if window_size = 0 or 1
  if (window_size == 0) {
    return(diffed_data)
  } else if (window_size == 1) {
    diffed_data = data_mat[2:num_points, ] - data_mat[1:(num_points-1), ]
    diffed_data = rbind(diffed_data[1, ] * NA, diffed_data)
    return(diffed_data)
  } else if (window_size > num_points) {
    warning("Swing window_size is larger than number of rows!  Swing events ignored!")
    return(diffed_data)
  }
  
  for (n_col in 1:ncol(data_mat)) {
    data_vector = data_mat[, n_col]
    
    diffed_data_temp = sapply(1:(num_points-window_size), function(i) data_vector[i+window_size] - mean(data_vector[i:(i+window_size-1)]))
    
    if (grow_window) {
      diffed_start = sapply(1:window_size, function(i) (mean(data_vector[1:i]) - data_vector[[i+1]]))
      diffed_data_temp = c(diffed_start, diffed_data_temp)
    } else {
      diffed_data_temp = c(rep(NA, window_size), diffed_data_temp)
    }
    
    diffed_data[,n_col] = diffed_data_temp
  }
  return(diffed_data)
}

############################ find runs ############################
find_runs <- function(location_swing, event_types = c(-3,-1.5,-1,1,1.5,3)) {
  
  event_points_new = matrix(NA, ncol = 5, nrow = 0, dimnames = list(NULL, c("col", "start", "middle", "end", "type")))
  for (n_col in 1:ncol(location_swing)) {
    temp = rle(location_swing[,n_col])
    relevant_indices = which(temp$values %in% event_types)
    
    for (index in relevant_indices) {
      # edge case when index=1
      if (index == 1) {
        start_point = 1
        end_point = start_point + temp$lengths[[index]] - 1
      } else {
        start_point = sum(temp$lengths[1:(index-1)]) + 1
        end_point = start_point + temp$lengths[[index]] - 1
      }
      
      # assign event type (jump=1, ramp=2)
      event_type = temp$values[[index]]
      
      event_points_new = rbind(event_points_new, c(n_col, start_point, floor(median(c(start_point, end_point))), end_point, event_type))
    }
  }
  return(event_points_new)
}

############################ clean events ############################

clean_events <- function(event_locations, noise_length = 1, event_gap = 1) {
  
  event_runs = find_runs(event_locations)
  num_events = nrow(event_runs)
  
  #### merge close events ####
  if (num_events > 0) {
    for (n_row in 1:(num_events - 1)) {
      
      # if only one event run, then break
      if (num_events == 1) {
        break
      }
      
      # find other events in same column
      same_column_row = which(event_runs[(n_row+1):nrow(event_runs),1] == event_runs[[n_row,1]])
      same_column_row = ((n_row+1):nrow(event_runs))[same_column_row] # reference the indices in event_runs
      
      for (n_event in same_column_row) {
        upstream_distance = abs(event_runs[[n_event, "end"]] - event_runs[[n_row, "start"]])
        downstream_distance = abs(event_runs[[n_event, "start"]] - event_runs[[n_row, "end"]])
        
        # if upstream or downstream distance is within event_gap, then merge events
        if ((upstream_distance <= (event_gap + 1)) | (downstream_distance <= (event_gap + 1))) {
          if (event_runs[[n_event, "type"]] == event_runs[[n_row, "type"]]) { # check that they are the same type of event, then merge
            event_locations[event_runs[[n_event, "end"]]:event_runs[[n_row, "start"]], event_runs[[n_event, 1]]] = event_runs[[n_event, "type"]]
            event_locations[event_runs[[n_row, "end"]]:event_runs[[n_event, "start"]], event_runs[[n_event, 1]]] = event_runs[[n_event, "type"]]
          }
        }
      }
    }
  }
  
  # temporarily merge event3 into event1
  event_3 = event_locations == 3
  event_n3 = event_locations == -3
  event_locations[event_3] = 1
  event_locations[event_n3] = -1
  # temporarily merge event1.5 into event1
  event_1.5 = event_locations == 1.5
  event_n1.5 = event_locations == -1.5
  event_locations[event_1.5] = 1
  event_locations[event_n1.5] = -1
  
  # redo event_runs
  event_runs = find_runs(event_locations)
  num_events = nrow(event_runs)
  
  #### drop short events ####
  if (num_events > 0) {
    distances = sapply(1:num_events, function(i) abs(event_runs[i, "start"] - event_runs[i, "end"]))
    remove_event_run_row = which(distances <= (noise_length - 1))
    
    # find events to remove
    for (n_remove in remove_event_run_row) {
      col_remove = event_runs[n_remove, "col"]
      start_remove = event_runs[n_remove, "start"]
      end_remove = event_runs[n_remove, "end"]
      
      event_locations[start_remove:end_remove, col_remove] = 0
    }
    
    # separate out event 3 (if still intact)
    event_locations[event_3 & event_locations == 1] = 3
    event_locations[event_n3 & event_locations == -1] = -3
    # separate out event 1.5 (if still intact)
    event_locations[event_1.5 & event_locations == 1] = 1.5
    event_locations[event_n1.5 & event_locations == -1] = -1.5
    # if not intact, turn into event2
    event_locations[(event_3 | event_1.5) & event_locations == 0] = 2
    event_locations[(event_n3 | event_n1.5) & event_locations == 0] = -2
    
  }
  return(event_locations)
}

############################  ############################

