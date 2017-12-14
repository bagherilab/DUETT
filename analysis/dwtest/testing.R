
setwd("~/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/analysis/dwtest/")

rm(list=ls())

library(lmtest)

num_points = 40

x = 1:num_points
y = 1:num_points + rnorm(num_points, sd = 1)

result = dwtest(y ~ x)
plot(x, y, main = paste("DW=", round(result$statistic, digits = 2), " p-value=", round(result$p.value, digits = 2), sep = ""))



flat_region = 5

y = c(rep(1, flat_region), seq(1, num_points,length.out=num_points-(2*flat_region)), rep(num_points, flat_region)) + rnorm(num_points)
result = dwtest(y ~ x)
plot(x, y, main = paste("DW=", round(result$statistic, digits = 2), " p-value=", round(result$p.value, digits = 4), sep = ""))



source("../../support_functions/load_data.R")
data_file = "../../example_data/other_data/SRP_wt_rho_table.txt"
data_mat = load_data(data_file)

make_plot <- function(rows = 1:40, col_i = 14) {
  ylim = c(0,10)
  x = 1:40
  y = data_mat[rows, col_i]
  result = dwtest(y ~ x)
  plot(x, y, main = paste("DW=", round(result$statistic, digits = 2), " p-value=", round(result$p.value, digits = 4), sep = ""), ylim = ylim)
}

par(mfrow = c(2,2))
par(mar = c(2,2,2,2))

make_plot()
make_plot(21:60, 41)
make_plot(66:105, 42)
# make_plot(66:105, 44)
make_plot(31:70, 40)



# negative autocorrelation?
par(mfrow = c(1, 1))
x = 1:40
y = 1:40 + rnorm(40, sd = 2)#rep(c(-1, 1), 20)
result = dwtest(y ~ x)
plot(x, y, main = paste("DW=", round(result$statistic, digits = 2), " p-value=", round(result$p.value, digits = 4), sep = ""))

x = 1:40
y = (1:40)[seq(1,40,2)]
y = as.vector(rbind(y, rep(40, 20)))
result = dwtest(y ~ x)
plot(x, y, main = paste("DW=", round(result$statistic, digits = 2), " p-value=", round(result$p.value, digits = 4), sep = ""))

