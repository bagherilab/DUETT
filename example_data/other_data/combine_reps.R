
rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/other_data/")

for (n_rep in 1:3) {
  filename = paste("F_wt_10mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  data_mat = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  data_mat = as.matrix(data_mat)
  write.table(data_mat, sep = "\t", file = "F_wt_10mM_NaF_rho_table.txt", col.names = F, row.names = F)
  
  filename = paste("F_wt_0mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  data_mat = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  data_mat = as.matrix(data_mat)
  write.table(data_mat, sep = "\t", file = "F_wt_0mM_NaF_rho_table.txt", col.names = F, row.names = F)
  
}


