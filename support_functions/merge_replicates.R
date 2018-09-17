merge_replicates <- function(event_storage, agreement) {

  # function to find all of a certain event in event_locations
  get_event_group <- function(event_locations_temp, event_group) {
    return(sapply(1:ncol(event_locations_temp), function(i) event_locations_temp[,i] %in% event_group))
  }
  
  if (class(try(Reduce("+", lapply(event_storage, function(i) get_event_group(i[[1]], c(2,3,1.5)))) >= agreement)) == "try-error") {browser()}
  shared_upramps = Reduce("+", lapply(event_storage, function(i) get_event_group(i[[1]], c(2,3,1.5)))) >= agreement
  shared_downramps = Reduce("+", lapply(event_storage, function(i) get_event_group(i[[1]], -c(2,3,1.5)))) >= agreement
  shared_upswings = Reduce("+", lapply(event_storage, function(i) get_event_group(i[[1]], c(1,3,-1.5)))) >= agreement
  shared_downswings = Reduce("+", lapply(event_storage, function(i) get_event_group(i[[1]], -c(1,3,-1.5)))) >= agreement
  
  # assign shared event category
  event_locations_new = shared_upramps * 0
  event_locations_new[shared_upramps] = 2
  event_locations_new[shared_downramps] = -2
  event_locations_new[shared_upswings] = 1
  event_locations_new[shared_downswings] = -1
  
  event_locations_new[shared_upramps & shared_upswings] = 3
  event_locations_new[shared_upramps & shared_downswings] = 1.5
  event_locations_new[shared_downramps & shared_downswings] = -3
  event_locations_new[shared_downramps & shared_upswings] = -1.5
  
  # make new event_storage, use first replicate as event_details
  event_storage = list(event_locations_new, event_storage[[1]][[2]])
  return(event_storage)
  
}