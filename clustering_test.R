# clustering test
rm(list=ls())

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/load_data.R")
raw_data = load_data("example_data/SRP_wt_Rep1_rho_table.txt")[[1]]

data_mat = raw_data
# data_mat[is.na(data_mat)] = 0

# retain only first 100 columns
data_mat = data_mat[,1:100]

# transpose so samples become variables
# data_mat = t(data_mat)

cluster_obj = hclust(dist(t(data_mat)))

plot(cluster_obj)
num_cluster = 10
cluster_cut = cutree(cluster_obj, num_cluster)

# reorder columns according to clusters
data_mat = data_mat[,order(cluster_cut)]

# insert columns of NAs to separate clusters
indices = cumsum(table(cluster_cut))
for (n_cluster in 1:(num_cluster-1)) {
  data_mat = cbind(data_mat[,1:indices[[n_cluster]]], 
                   matrix(NA, ncol = 2, nrow = nrow(data_mat)), 
                   data_mat[,(indices[[n_cluster]]+1):ncol(data_mat)])
}

heatmap(data_mat, Rowv = NA, Colv = NA)
