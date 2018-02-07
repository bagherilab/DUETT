rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/ShapeSeq_events.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/load_data.R")

make_shared_plots <- function(file_list = c("analysis/F/replicates/F_subtract_1.csv", "analysis/F/replicates/F_subtract_2.csv", "analysis/F/replicates/F_subtract_3.csv"),
                              data_mat_file = "example_data/other_data/F_wt_0mM_NaF_rho_table.txt",
                              outfile = "analysis/F_subtract/replicates/replicate_shared_events.pdf",
                              out_table = "F_0mM.csv") {
  
  data_mat = load_data(data_mat_file)
  # event_locations_list = lapply(file_list, function(i) read.csv(i))
  event_locations1 = read.csv(file_list[[1]])
  event_locations2 = read.csv(file_list[[2]])
  event_locations3 = read.csv(file_list[[3]])
  
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
  
  pdf(outfile, width = 14, height = 14)
  make_visual(data_mat, event_locations_shared_all, concurrent_events_all, log_colors = F, diverging = F)
  make_visual(data_mat, event_locations_shared_2, concurrent_events_2, log_colors = F, diverging = F)
  dev.off()
  
  write.csv(event_locations_shared_all, file = out_table)
}

make_shared_plots(file_list = c("analysis/F/replicates/F_0mM_1.csv", 
                                "analysis/F/replicates/F_0mM_2.csv", 
                                "analysis/F/replicates/F_0mM_3.csv"),
                  data_mat_file = "example_data/other_data/F_wt_0mM_NaF_rho_table.txt",
                  outfile = "analysis/F/replicates/shared_F_0mM.pdf",
                  out_table = "analysis/F/replicates/shared_F_0mM.csv")


make_shared_plots(file_list = c("analysis/F/replicates/F_10mM_1.csv", 
                                "analysis/F/replicates/F_10mM_2.csv", 
                                "analysis/F/replicates/F_10mM_3.csv"), 
                  data_mat_file = "example_data/other_data/F_wt_10mM_NaF_rho_table.txt",
                  outfile = "analysis/F/replicates/shared_F_10mM.pdf",
                  out_table = "analysis/F/replicates/shared_F_10mM.csv")

