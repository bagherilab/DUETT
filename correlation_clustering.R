# clustering test
rm(list=ls())

library(mpmi)
library(viridis)

setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")
# setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

source("support_functions/load_data.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/plotting/plot_dump_PID.R")

raw_data = load_data("example_data/SRP_wt_rho_table.txt")[[1]]

data_mat = raw_data

# retain only first 100 columns
data_mat = data_mat[,1:100]
colnames(data_mat) = paste("Col", 1:ncol(data_mat), sep = "")

# keep the original
data_mat_orig = data_mat

# remove columns that are basically zero
zero_column = which(colMeans(data_mat, na.rm = T) <= 1)
data_mat = data_mat[,-zero_column]

# turn NAs into zero
data_mat[is.na(data_mat)] = 0

############################################################

# trying correlations
sim_matrix = cor(data_mat)
# turn all into positives
sim_matrix[sim_matrix<0]=0
# turn the diagonals into 0's
sim_matrix[as.logical(diag(ncol(sim_matrix)))] <- 0

# processing and plotting heatmap of similarity matrix
dimnames(sim_matrix) = list(colnames(data_mat), colnames(data_mat))

pdf("correlation_clustering.pdf")

make_visual(sim_matrix, event_locations=matrix(NA,nrow=0,ncol=4), concurrent_events=matrix(NA,nrow=0,ncol=4), numbering = F)

# make axes
col_names = sapply(strsplit(colnames(sim_matrix), "Col"), function(i) i[[2]])
par(xpd = T)
num_col = ncol(sim_matrix)
text(rep(par("usr")[[1]], num_col), seq(1,0,length.out = num_col+2), labels = c("", col_names,""), pos = 4, cex = 0.75)
text(rep(par("usr")[[2]], num_col), seq(1,0,length.out = num_col+2), labels = c("", col_names,""), pos = 2, cex = 0.75)
text(seq(par("usr")[[1]], par("usr")[[2]],length.out = num_col+4), rep(par("usr")[[3]], num_col), labels = c("", "", col_names, "", ""), pos = 3, cex = 0.75)
text(seq(par("usr")[[1]], par("usr")[[2]],length.out = num_col+4), rep(par("usr")[[4]], num_col), labels = c("", "", col_names, "", ""), pos = 1, cex = 0.75)

# custom cluster from correlation
cluster_cut = c(1,2, rep(3,5), rep(4,7), rep(5,3), rep(6,3), rep(7,3), rep(8,4))
num_cluster = length(unique(cluster_cut))
# reorder columns according to clusters
data_mat = data_mat[,order(cluster_cut)]
# determine colors
color_list = viridis_pal(option = "D")(num_cluster)

ylim = c(0, 15)
par(mfrow = c(4,4))

for (n_col in 1:ncol(data_mat)) {
  plot_data(data_vector = data_mat[,n_col], event_locations = NULL, concurrent_events = NULL, n_col = n_col, ylim = ylim, dot_color = color_list[[cluster_cut[order(cluster_cut)][[n_col]]]], disable_title = T, mar = c(2,2,2,2))
  title(main = paste("Clust", sort(cluster_cut)[[n_col]], " ", col_names[[n_col]], sep = ""))
}

dev.off()

# do correlation for each column that has an event
# load events
event_mat = read.csv("analysis/SRP/replicates/SRP.csv")
# create column names
colnames(event_mat) = paste("Col", 1:ncol(event_mat), sep = "")

# retain only first 100 columns
event_mat = event_mat[,1:100]

# find non zero events
event_columns = names(which(colSums(event_mat != 0) > 0))

data_mat_orig[is.na(data_mat_orig)] = 0
cor_list = list()
for (n_col in event_columns) {
  # get correlations (remove original column)
  cor_list[[n_col]] = cor(data_mat_orig[,n_col], data_mat_orig[, -which(colnames(data_mat_orig) == n_col)])[1,]
  # sort the correlations
  cor_list[[n_col]] = round(sort(cor_list[[n_col]], decreasing = T), digits = 3)
}

p_values = list()
for (n_col in names(cor_list)) {
  window_size = 10
  columns = as.numeric(sapply(strsplit(names(cor_list[[n_col]]), "Col"), function(i) i[[2]]))
  high_cor = columns[1:(window_size*2)] # all columns with high correlation
  num_high = sum(abs(high_cor - as.numeric(strsplit(n_col, "Col")[[1]][[2]])) <= window_size) # number of columns that have high correlation and are within the distance
  num_relevant_columns = sum(abs(columns - as.numeric(strsplit(n_col, "Col")[[1]][[2]])) <= window_size)
  
  p_values[[n_col]] = sum(dhyper(num_high:99, num_relevant_columns, 99-(window_size*2), window_size*2))
  
}

# print cor_list
table_output = matrix(NA, nrow = 99*2, ncol = length(p_values) * 2)
colnames_temp = vector("list", length(p_values) * 2)
colnames_temp[seq(1, (length(p_values) * 2), by = 2)] <- names(p_values)
colnames_temp[seq(2, (length(p_values) * 2), by = 2)] <- ""
colnames(table_output) = colnames_temp

for (n_col in names(cor_list)) {
  table_output[,n_col] = names(cor_list[[n_col]])
  table_output[,which(colnames(table_output) == n_col) + 1] = cor_list[[n_col]]
}

write.table(table_output, file = "column_correlations.csv", sep = ",", quote = F, row.names = F)
write.table(p_values, file = "correlation_p_values.csv", sep = ",", quote = F, row.names = F)
