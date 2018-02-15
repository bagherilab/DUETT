rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/ShapeSeq_events.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/load_data.R")

file_list = c("analysis/F/replicates/shared_F_0mM.csv", 
              "analysis/F/replicates/shared_F_10mM.csv", 
              "analysis/F_subtract/F_subtract.csv")
outfile = "analysis/F/compare_F_and_F_subtract.pdf"

data_mat = load_data("example_data/other_data/F_subtract.txt")
# event_locations_list = lapply(file_list, function(i) read.csv(i))
event_locations1 = read.csv(file_list[[1]], row.names = 1)
event_locations2 = read.csv(file_list[[2]], row.names = 1)
event_locations3 = read.csv(file_list[[3]])

get_event_group <- function(event_locations_temp, event_group) {
  return(sapply(1:ncol(event_locations_temp), function(i) event_locations_temp[,i] %in% event_group))
}

event_group = c(1,3,-1.5)

# upswings
upswings = ((get_event_group(event_locations1, -c(event_group, 2, -2, 0)) & get_event_group(event_locations2, event_group)) & get_event_group(event_locations3, event_group)) |
  ((get_event_group(event_locations1, -event_group) & get_event_group(event_locations2, c(event_group, 2, -2, 0))) & get_event_group(event_locations3, event_group))

# downswings
downswings = ((get_event_group(event_locations1, c(event_group, 2, -2, 0)) & get_event_group(event_locations2, -event_group)) & get_event_group(event_locations3, -event_group)) |
  ((get_event_group(event_locations1, event_group) & get_event_group(event_locations2, -c(event_group, 2, -2, 0))) & get_event_group(event_locations3, -event_group))

event_locations = upswings * 0
event_locations[upswings] = 1
event_locations[downswings] = -1
concurrent_events = find_concurrent_events(event_locations, 2, event_types = c(-1,1))

pdf(outfile, width = 14, height = 14)
make_visual(data_mat, event_locations, concurrent_events, log_colors = T, diverging = T)
dev.off()



