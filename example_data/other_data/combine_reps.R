
rm(list=ls())

setwd("C:/Users/Xuebert//Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/other_data/")

combine_reps <- function(filename1, filename2, new_filename) {
  
  for (n_rep in 1:3) {
    filename = paste(filename1, n_rep, filename2, sep = "")
    nc = max(count.fields(filename, sep="\t"))
    # read the file
    new_data_mat = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
    new_data_mat = as.matrix(new_data_mat)
    
    if (n_rep == 1) {
      data_mat = new_data_mat
    } else {
      data_mat = data_mat + new_data_mat
    }
  }
  
  data_mat = data_mat / 3
  write.table(data_mat, sep = "\t", file = new_filename, col.names = F, row.names = F)
}

combine_reps("F_wt_10mM_NaF_Rep", "_rho_table.txt", "F_wt_10mM_NaF_rho_table.txt")
combine_reps("F_wt_0mM_NaF_Rep", "_rho_table.txt", "F_wt_0mM_NaF_rho_table.txt")
combine_reps("SRP_wt_Rep", "_rho_table.txt", "SRP_wt_rho_table.txt")

