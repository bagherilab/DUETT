rm(list=ls())

# setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")
setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/ShapeSeq_events.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/load_data.R")

data_mat = load_data("example_data/other_data/SRP_wt_rho_table.txt")
event_locations1 = read.csv("analysis/SRP/replicates/SRP_wt_Rep1_rho_table.csv")
event_locations2 = read.csv("analysis/SRP/replicates/SRP_wt_Rep2_rho_table.csv")
event_locations3 = read.csv("analysis/SRP/replicates/SRP_wt_Rep3_rho_table.csv")

get_event_group <- function(event_locations_temp, event_group) {
  return(sapply(1:ncol(event_locations_temp), function(i) event_locations_temp[,i] %in% event_group))
}

get_shared_all <- function(event_group) {
  return(((get_event_group(event_locations1, event_group) & get_event_group(event_locations2, event_group)) & get_event_group(event_locations3, event_group)) | ((get_event_group(event_locations1, -event_group) & get_event_group(event_locations2, -event_group)) & get_event_group(event_locations3, -event_group)))
}

event_group = c(1,2,3,1.5)
shared_all = get_shared_all(event_group)

shared_2 = ((get_event_group(event_locations1, event_group) + get_event_group(event_locations2, event_group) + get_event_group(event_locations3, event_group)) >= 2) | ((get_event_group(event_locations1, -event_group) + get_event_group(event_locations2, -event_group) + get_event_group(event_locations3, -event_group)) >= 2)

event_locations_shared_all = event_locations1 * 0
event_locations_shared_all[which(shared_all, arr.ind = T)] = event_locations1[which(shared_all, arr.ind = T)]

event_locations_shared_2 = event_locations1 * 0
event_locations_shared_2[which(shared_2, arr.ind = T)] = event_locations1[which(shared_2, arr.ind = T)]

concurrent_events_all = find_concurrent_events(event_locations_shared_all, 2, event_types = c(-1,1))
concurrent_events_2 = find_concurrent_events(event_locations_shared_2, 2, event_types = c(-1,1))

pdf("analysis/SRP/replicates/replicate_shared_events.pdf", width = 14, height = 14)
make_visual(data_mat, event_locations_shared_all, concurrent_events_all)
make_visual(data_mat, event_locations_shared_2, concurrent_events_2)
dev.off()