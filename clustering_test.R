# clustering test
rm(list=ls())

library(mpmi)
library(viridis)

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")
# setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/load_data.R")
source("support_functions/plotting/make_visual.R")
raw_data = load_data("example_data/SRP_wt_rho_table.txt")[[1]]

data_mat = raw_data
# data_mat[is.na(data_mat)] = 0

# retain only first 100 columns
data_mat = data_mat[,1:100]
colnames(data_mat) = paste("Col", 1:ncol(data_mat), sep = "")

# remove columns that are basically zero
zero_column = which(colMeans(data_mat, na.rm = T) <= 1)
data_mat = data_mat[,-zero_column]

# turn NAs into zero
data_mat[is.na(data_mat)] = 0

# trying correlations
sim_matrix = cor(data_mat)
# turn all into positives
sim_matrix[sim_matrix<0]=0
# turn the diagonals into 0's
sim_matrix[as.logical(diag(ncol(sim_matrix)))] <- 0

# trying mutual information
# sim_matrix = cmi(data_mat, na.rm = T)$mi

# processing and plotting heatmap of similarity matrix
dimnames(sim_matrix) = list(colnames(data_mat), colnames(data_mat))
pdf("clustering_test.pdf")
make_visual(sim_matrix, event_locations=matrix(NA,nrow=0,ncol=4), concurrent_events=matrix(NA,nrow=0,ncol=4), numbering = F)

# custom cluster from correlation
cluster_cut = c(1,2, rep(3,5), rep(4,7), rep(5,3), rep(6,3), rep(7,3), rep(8,4))
num_cluster = length(unique(cluster_cut))

# make axes
col_names = sapply(strsplit(colnames(sim_matrix), "Col"), function(i) i[[2]])
par(xpd = T)
num_col = ncol(sim_matrix)
text(rep(par("usr")[[1]], num_col), seq(1,0,length.out = num_col+2), labels = c("", col_names,""), pos = 4, cex = 0.75)
text(rep(par("usr")[[2]], num_col), seq(1,0,length.out = num_col+2), labels = c("", col_names,""), pos = 2, cex = 0.75)
text(seq(par("usr")[[1]], par("usr")[[2]],length.out = num_col+4), rep(par("usr")[[3]], num_col), labels = c("", "", col_names, "", ""), pos = 3, cex = 0.75)
text(seq(par("usr")[[1]], par("usr")[[2]],length.out = num_col+4), rep(par("usr")[[4]], num_col), labels = c("", "", col_names, "", ""), pos = 1, cex = 0.75)

# hierarchical clustering
# cluster_obj = hclust(dist(t(data_mat)))

# par(mfrow = c(1,1))
# plot(cluster_obj)
# num_cluster = 8
ylim = c(0, 15)
# cluster_cut = cutree(cluster_obj, num_cluster)
# col_names = names(cluster_cut)[order(cluster_cut)]

# reorder columns according to clusters
data_mat = data_mat[,order(cluster_cut)]
# determine colors
color_list = viridis_pal(option = "D")(num_cluster)

# hijack plot_dump_PID.R
source("support_functions/plotting/plot_dump_PID.R")

par(mfrow = c(4,4))
for (n_col in 1:ncol(data_mat)) {
  plot_data(data_vector = data_mat[,n_col], event_locations = NULL, concurrent_events = NULL, n_col = n_col, ylim = ylim, dot_color = color_list[[cluster_cut[order(cluster_cut)][[n_col]]]], disable_title = T, mar = c(2,2,2,2))
  title(main = paste("Clust", sort(cluster_cut)[[n_col]], " ", col_names[[n_col]], sep = ""))
}

dev.off()
