rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/load_data.R")

data_mat = load_data("example_data/other_data/SRP_wt_rho_table.txt")
event_locations1 = read.csv("analysis/SRP/replicates/lenient_1.csv")
event_locations2 = read.csv("analysis/SRP/replicates/lenient_2.csv")
event_locations3 = read.csv("analysis/SRP/replicates/lenient_3.csv")

get_event_group <- function(event_locations_temp, event_group) {
  return(sapply(1:ncol(event_locations_temp), function(i) event_locations_temp[,i] %in% event_group))
}

get_shared_all <- function(event_group) {
  return((get_event_group(event_locations1, event_group) & get_event_group(event_locations2, event_group)) & get_event_group(event_locations3, event_group))
}

get_shared_2 <- function(event_group) {
  return((get_event_group(event_locations1, event_group) + get_event_group(event_locations2, event_group) + get_event_group(event_locations3, event_group)) >= 2)
}

# upramps, downramps, upswings, downswings
shared_all = lapply(list(c(2,3,1.5), -c(2,3,1.5), c(1,3,-1.5), -c(1,3,-1.5)), function(i) get_shared_all(i))
shared_2 = lapply(list(c(2,3,1.5), -c(2,3,1.5), c(1,3,-1.5), -c(1,3,-1.5)), function(i) get_shared_2(i))

create_event_locations_shared <- function(shared_list) {
  event_locations_shared = event_locations1 * 0
  event_locations_shared[which(shared_list[[1]], arr.ind = T)] = 2
  event_locations_shared[which(shared_list[[2]], arr.ind = T)] = -2
  event_locations_shared[which(shared_list[[3]], arr.ind = T)] = 1
  event_locations_shared[which(shared_list[[4]], arr.ind = T)] = -1
  
  # overlapping ramps/swings
  event_locations_shared[which(shared_list[[1]] & shared_list[[3]], arr.ind = T)] = 3
  event_locations_shared[which(shared_list[[1]] & shared_list[[4]], arr.ind = T)] = 1.5
  event_locations_shared[which(shared_list[[2]] & shared_list[[4]], arr.ind = T)] = -3
  event_locations_shared[which(shared_list[[2]] & shared_list[[3]], arr.ind = T)] = -1.5
  
  return(event_locations_shared)
}

event_locations_shared_all = create_event_locations_shared(shared_all)
event_locations_shared_2 = create_event_locations_shared(shared_2)

concurrent_events_all = find_concurrent_events(event_locations_shared_all, 2, event_types = c(-1,1))
concurrent_events_2 = find_concurrent_events(event_locations_shared_2, 2, event_types = c(-1,1))

pdf("analysis/SRP/replicates/replicate_shared_events.pdf", width = 14, height = 14)
make_visual(data_mat[[1]], event_locations_shared_all, concurrent_events_all)
make_visual(data_mat[[1]], event_locations_shared_2, concurrent_events_2)
dev.off()