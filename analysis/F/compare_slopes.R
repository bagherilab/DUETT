#
setwd("~/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

rm(list=ls())

source("support_functions/load_data.R")
source("../../SML_frameworks/Q2.R")

data_mat_0mM = load_data("example_data/other_data/F_wt_0mM_NaF_Rep1_rho_table.txt")[[1]]
num_rows = nrow(data_mat_0mM)
data_mat_0mM = rbind(data_mat_0mM, load_data("example_data/other_data/F_wt_0mM_NaF_Rep2_rho_table.txt")[[1]])
data_mat_0mM = rbind(data_mat_0mM, load_data("example_data/other_data/F_wt_0mM_NaF_Rep3_rho_table.txt")[[1]])

data_mat_10mM = load_data("example_data/other_data/F_wt_10mM_NaF_Rep1_rho_table.txt")[[1]]
data_mat_10mM = rbind(data_mat_10mM, load_data("example_data/other_data/F_wt_10mM_NaF_Rep2_rho_table.txt")[[1]])
data_mat_10mM = rbind(data_mat_10mM, load_data("example_data/other_data/F_wt_10mM_NaF_Rep3_rho_table.txt")[[1]])

# x
x = rep(1:num_rows, 3) + 20
ylim = c(0,20)

pdf("analysis/F/compare_slopes.pdf")
par(mfrow = c(2,2))
for (n_col in 52:55) {
  lm_obj = lm.fit(x = as.matrix(1:6), y = data_mat_0mM[50:55,n_col])
  y_fit = lm_obj$coefficients * 1:20
  
  plot(x = x, y = data_mat_0mM[,n_col], ylim = ylim, col = "grey50", main = paste("Column", n_col), ylab = "SHAPE reactivity", xlab = "Transcript length")
  lines(x = 70:89, y = y_fit, col = "red")
  
}

for (n_col in 52:55) {  
  lm_obj = lm.fit(x = as.matrix(1:6), y = data_mat_0mM[50:55,n_col])
  y_fit = lm_obj$coefficients * 1:15
  
  plot(x = x, y = data_mat_10mM[,n_col], ylim = ylim, col = "grey50", main = paste("Column", n_col), ylab = "SHAPE reactivity", xlab = "Transcript length")
  lines(x = 70:84, y = y_fit, col = "red")
  y_actual = data_mat_10mM[50:64, n_col]
  # SS_res = sum((y_fit - y_actual) ^ 2)
  # SS_tot = sum((y_actual - mean(y_actual))^ 2)
  # Q2 = 1 - SS_res / SS_tot
  R2 = Q2(as.matrix(y_fit), as.matrix(y_actual), as.matrix(rep(mean(y_actual), length(y_actual))))
  R2 = round(R2, digits = 2)
  text(21, 18, paste("R2 = ", R2), pos = 4)
  
  plot(y_actual, y_fit, xlim = c(0, max(y_fit, y_actual)), ylim = c(0, max(y_fit, y_actual)))
  lines(0:20, 0:20, lty = 2)
}
dev.off()

