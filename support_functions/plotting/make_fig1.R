# make_visual <- function(data_mat, event_locations, concurrent_events, log_colors = F, color_scale = NULL, numbering = T, numbering_offset = 0, numbering_interval = 5, axis_label_resize = 1, lwd = 0.01, fg = "black", circle_size_override = NULL, box_resize = 1, diverging = F, event_colors = NULL) {

rm(list=ls())

setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/SHAPE-Seq_event_detector/")

size = 40
data_mat = matrix(runif(size^2), size) / 8
event_locations = data_mat * 0

swing_list = list(list(12:19, size/2-1, 1, 0.7, 1), # open hairpin
                list(10:18, size/2, 1, 0.7, 1),
                list(11:19, size/2+1, 1, 0.7, 1),
                list(13:19, size/2+2, 1, 0.7, 1),
                
                list(30:32, size/2-1, -1, 0.6, 0.7), # 2nd tiny loop
                list(31:33, size/2, -1, 0.6, 0.7),
                
                list(20:22, size/2-3, 1, 0.3, 0.4), # closed stem left
                list(18:21, size/2-4, 1, 0.3, 0.4),
                list(20:22, size/2-5, 1, 0.3, 0.4),
                list(22:24, size/2-3, -1, 0, 1/8),
                list(21:23, size/2-4, -1, 0, 1/8),
                list(22:26, size/2-5, -1, 0, 1/8),
                
                list(20:22, size/2+3, 1, 0.3, 0.4), # closed stem right
                list(20:22, size/2+4, 1, 0.3, 0.4),
                list(19:22, size/2+5, 1, 0.3, 0.4),
                list(23:25, size/2+3, -1, 0, 1/8),
                list(23:25, size/2+4, -1, 0, 1/8),
                list(23:24, size/2+5, -1, 0, 1/8),
                
                list(30:33, size/2+10, 1, 0.6, 0.7), # 2nd open hairpin
                list(31:35, size/2+11, 1, 0.6, 0.7))

for (swing in swing_list) {
  # browser()
  data_mat[swing[[1]], swing[[2]]] = runif(length(swing[[1]]), min = swing[[4]], max = swing[[5]])
  event_locations[swing[[1]],swing[[2]]] = swing[[3]]
  
  # add reactivity ramps
  if (swing[[3]] == 1) {
    ramp_length = sample(5:9, 1)
    data_mat[(min(swing[[1]])-ramp_length):(min(swing[[1]])-1), swing[[2]]] = seq(1/8, (swing[[5]]+swing[[4]])/2, length.out = ramp_length) + runif(ramp_length, -1/8,1/8)
  }
}

# main loop (first two nucleotides)
data_mat[20:30, size/2-1] = runif(length(20:30), 0.7,1)
data_mat[32:size, size/2-1] = runif(length(32:size), 1/8, 0.2)

# 2nd tiny loop
data_mat[19:31, size/2] = runif(length(19:31), 0.7,1)
data_mat[33:size, size/2] = runif(length(33:size), 1/8, 0.2)

# main loop (second two nucleotides)
data_mat[20:size, size/2+1] = runif(length(20:size), 0.7,1)
data_mat[20:size, size/2+2] = runif(length(20:size), 0.7,1)

# new hairpin
data_mat[34:size, size/2+10] = runif(length(34:size), 0.7,1)
data_mat[32:size, size/2+11] = runif(length(32:size), 0.7,1)

# list(30:33, size/2+10, 1, 0.6, 0.7), # 2nd open hairpin
# list(31:35, size/2+11, 1, 0.6, 0.7))

concurrent_events = matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("row1", "col1", "row2", "col2")))

log_colors = F
color_scale = NULL
numbering = T
numbering_offset = 0
numbering_interval = 5
axis_label_resize = 1
lwd = 0.01
fg = "black"
circle_size_override = NULL
box_resize = 0.75
diverging = F
event_colors = NULL

pdf('test.pdf')
source("support_functions/utility_functions.R")
source("support_functions/color_to_hex.R")
source("support_functions/plotting/make_boxes.R")
source("support_functions/plotting/axis_labels.R")
source("support_functions/plotting/make_visual_PID.R")
source("support_functions/plotting/make_visual_concurrent.R")

if (log_colors) {
  negative_values = data_mat < 0 & !is.na(data_mat)
  positive_values = data_mat > 0 & !is.na(data_mat)
  data_mat[negative_values] = -log2(abs(data_mat[negative_values]) + 1)
  data_mat[positive_values] = log2(data_mat[positive_values] + 1)
}

# set colors
if (is.null(event_colors)) {
  event_colors = list(upswing = color_to_hex("red", 0.1), downswing = color_to_hex("blue", 0.1)) # transparent
  # event_colors = list(upswing = color_to_hex("red", 0), downswing = color_to_hex("blue", 0)) # not transparent
}
clear_color = color_to_hex("white", 1)
library(RColorBrewer)
if (diverging) {
  library(grDevices)
  base_color = colorRamp(c("lightslateblue", "white", "lightcoral")) ((1:100)/100)
  # convert to hexcode
  base_color = apply(base_color, 1, function(i) do.call(rgb, as.list(i / 255)))
} else {
  base_color = colorRampPalette(brewer.pal(n = 7, name ="Greys"))(100)
}

# make grey boxes for the data
make_boxes(data_mat = data_mat, color_scale = base_color, lwd = lwd, fg = clear_color, box_resize = box_resize, diverging = diverging)
if (numbering) {axis_labels(n_row = nrow(data_mat), n_col = ncol(data_mat), numbering_offset = numbering_offset, numbering_interval = numbering_interval, axis_label_resize = axis_label_resize)}

# add swing and ramp events
par(new = T)
make_visual_PID(event_locations, event_draw = c(-1,1, -1.5,1.5, -3,3), ramp_draw = c(-1,1), event_colors = event_colors, box_resize = box_resize, lwd = 2)
par(new = T)

# add concurent events
num_rows = nrow(event_locations)
make_visual_concurrent(concurrent_events, num_rows, lwd = 3)
par(new = F)

dev.off()
# }