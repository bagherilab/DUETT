
setwd("~/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/other_data/")

for (n_rep in 1:3) {
  filename = paste("F_wt_0mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  raw_data_0 = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  raw_data_0 = as.matrix(raw_data_0)
  
  filename = paste("F_wt_10mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  raw_data_10 = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  raw_data_10 = as.matrix(raw_data_10)
  
  subtract_data = raw_data_10 - raw_data_0
  
  # try logging the data
  # negative_values = subtract_data < 0 & !is.na(subtract_data)
  # positive_values = subtract_data > 0 & !is.na(subtract_data)
  # subtract_data[negative_values] = -log2(abs(subtract_data[negative_values]) + 1)
  # subtract_data[positive_values] = log2(subtract_data[positive_values] + 1)
  
  write.table(subtract_data, sep = "\t", file = paste("F_subtract_", n_rep, ".txt", sep = ""), col.names = F, row.names = F)
}


