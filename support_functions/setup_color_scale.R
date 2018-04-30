# setup color_scale

rm(list=ls())

library(RColorBrewer)

red = colorRampPalette(colors = c("white", "#FF7D7D", "red", "#CD000D", "#99000D", "black"))(100)

blue = colorRampPalette(colors = c("white", "blue", "black"))(100)
green = colorRampPalette(colors = c("white", "green", "black"))(100)
orange = colorRampPalette(colors = c("white", "darkgoldenrod1", "black"))(100)
purple = colorRampPalette(colors = c("white", "purple", "black"))(100)

color_scale_list = list(red = red, blue = blue, green = green, orange = orange, purple = purple)

save(color_scale_list, file = "support_functions/color_scale.RData")

# plot(0:1)
# source("support_functions/legend_col.R")
# legend_col(matrix(seq(0, 1, length.out = 100), 10), color_scale = purple)