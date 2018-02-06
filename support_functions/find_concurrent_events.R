find_concurrent_events <- function(event_locations, concurrent_distance = 2, comparison_point = "start", event_types = c(-2,-1,1,2)) {
  
  source("support_functions/utility_functions.R")
  
  concurrent_events = matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("row1", "col1", "row2", "col2")))
  
  if (length(event_types) == 0) {
    return(concurrent_events)
  }
  
  event_points = matrix(NA, nrow = 0, ncol = 5, dimnames = list(NULL, c("col", "start", "middle", "end", "type")))
  for (merge_3 in 1:2) {
    event_locations_temp = event_locations
    if (merge_3 %in% event_types) {
      
      # merge 3 into 2 or 1
      event_locations_temp[event_locations_temp == 3] = merge_3
      event_locations_temp[event_locations_temp == -3] = -merge_3
      # merge 1.5 into 2 or 1
      event_locations_temp[event_locations_temp == 1.5] = merge_3
      event_locations_temp[event_locations_temp == -1.5] = -merge_3
      
      event_points = rbind(event_points, find_runs(event_locations_temp, event_types = event_types))
    }
  }
  
  # remove duplicate events (can happen if a ramp and a swing event start at same spot)
    unique_event_points = !duplicated(apply(event_points, 1, function(i) paste(i, collapse = "_")))
  event_points = event_points[unique_event_points, ]
  
  # check for one or no events
  if (is.null(nrow(event_points))) {
    return(concurrent_events)
  } else if (nrow(event_points) %in% 0:1) {
    return(concurrent_events)
  }
  
  for (n_row1 in 1:(nrow(event_points)-1)) {
    for (n_row2 in (n_row1+1):nrow(event_points)) {
      distance = abs(event_points[n_row1, comparison_point] - event_points[n_row2, comparison_point])
      
      col_diff = abs(event_points[n_row1, "col"] - event_points[n_row2, "col"]) # record difference in col (to make sure not concurrent event is not in same column)
      
      if ((distance <= concurrent_distance) & (col_diff > 0)) {
        concurrent_events = rbind(concurrent_events, c(event_points[n_row1, c(comparison_point, "col")], event_points[n_row2, c(comparison_point, "col")]))
      }
    }
  }
  return(concurrent_events)
}