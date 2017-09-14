find_concurrent_events <- function(event_locations, concurrent_distance = 2, comparison_point = "start", event_types = c(-3,-2,-1,1,2,3), merge_3 = 2) {
  
  concurrent_events = matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("row1", "col1", "row2", "col2")))
  event_points = matrix(NA, ncol = 4, nrow = 0, dimnames = list(NULL, c("col", "start", "middle", "end")))
  
  # merge 3 into 2 or 1
  event_locations[event_locations == 3] = merge_3
  event_locations[event_locations == -3] = -merge_3
  
  event_points = find_runs(event_locations, event_types = event_types)
  
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