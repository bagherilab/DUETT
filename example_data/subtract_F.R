
rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/other_data/")
# setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/other_data/")

raw_data_0 = vector("list", 3)
raw_data_10 = vector("list", 3)
raw_data_subtract = vector("list", 3)

for (n_rep in 1:3) {
  filename = paste("F_wt_0mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  raw_data_0[[n_rep]] = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  raw_data_0[[n_rep]] = as.matrix(raw_data_0[[n_rep]])
  
  filename = paste("F_wt_10mM_NaF_Rep", n_rep, "_rho_table.txt", sep = "")
  nc = max(count.fields(filename, sep="\t"))
  # read the file
  raw_data_10[[n_rep]] = read.table(filename, sep="\t", header=FALSE, col.names = 1:nc, fill=TRUE, colClasses="numeric")
  raw_data_10[[n_rep]] = as.matrix(raw_data_10[[n_rep]])
  
  raw_data_subtract[[n_rep]] = raw_data_10[[n_rep]] - raw_data_0[[n_rep]]
  
  # try logging the data
  # negative_values = subtract_data < 0 & !is.na(subtract_data)
  # positive_values = subtract_data > 0 & !is.na(subtract_data)
  # subtract_data[negative_values] = -log2(abs(subtract_data[negative_values]) + 1)
  # subtract_data[positive_values] = log2(subtract_data[positive_values] + 1)
  
  write.table(raw_data_subtract[[n_rep]], sep = "\t", file = paste("F_subtract_", n_rep, ".txt", sep = ""), col.names = F, row.names = F)
  
}

# average the reps then subtract
subtract_averaged = ((raw_data_10[[1]] + raw_data_10[[2]] + raw_data_10[[3]]) / 3) - ((raw_data_0[[1]] + raw_data_0[[2]] + raw_data_0[[3]]) / 3)
write.table(subtract_averaged, sep = "\t", file = "F_subtract.txt", col.names = F, row.names = F)

# reformat data after writing
for (n_rep in 1:3) {
  
  raw_data_0[[n_rep]] = as.vector(raw_data_0[[n_rep]])
  raw_data_0[[n_rep]] = raw_data_0[[n_rep]][!is.na(raw_data_0[[n_rep]])]
  
  raw_data_10[[n_rep]] = as.vector(raw_data_10[[n_rep]])
  raw_data_10[[n_rep]] = raw_data_10[[n_rep]][!is.na(raw_data_10[[n_rep]])]
  
  raw_data_subtract[[n_rep]] = as.vector(raw_data_subtract[[n_rep]])
  raw_data_subtract[[n_rep]] = raw_data_subtract[[n_rep]][!is.na(raw_data_subtract[[n_rep]])]
}

# make correlations
test_i = matrix(c(1,2,1,3,2,3), ncol = 2, byrow = T)
cor_storage = matrix(NA, nrow = 3, ncol = 3)
for (n_test in 1:3) {
  cor_storage[[1,n_test]] = cor(raw_data_0[[test_i[[n_test,1]]]], raw_data_0[[test_i[[n_test,2]]]])
  cor_storage[[2,n_test]] = cor(raw_data_10[[test_i[[n_test,1]]]], raw_data_10[[test_i[[n_test,2]]]])
  cor_storage[[3,n_test]] = cor(raw_data_subtract[[test_i[[n_test,1]]]], raw_data_subtract[[test_i[[n_test,2]]]])
}

subtract_averaged = ((raw_data_10[[1]] + raw_data_10[[2]] + raw_data_10[[3]]) / 3) - (raw_data_0[[1]] + raw_data_0[[2]] + raw_data_0[[3]]) / 3
cor_subtract_averaged = sapply(1:3, function(i) cor(subtract_averaged, raw_data_subtract[[i]]))


