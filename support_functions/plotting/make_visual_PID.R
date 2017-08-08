make_visual_PID <- function(event_locations, event_draw = c(-3,-1,1,3), ramp_draw = c(-3,-2,2,3), event_colors = NULL, lwd = 1.2, label_axes = F) {
  source("support_functions/plotting/make_visual.R")
  source("support_functions/plotting/axis_labels.R")
  clear_color = "#FFFFFF00"
  
  if (is.null(event_colors)) {event_colors = c("red", "blue")}
  
  plot(1, type = "n", xlim = c(0, ncol(event_locations)+1), ylim = c(0, nrow(event_locations)+1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if (label_axes) {axis_labels(nrow(event_locations), ncol(event_locations))}
  
  par(new = T)
  for (n_event in event_draw) {
    circle_colors = event_locations * NA
    
    event_indices = which(event_locations == n_event, arr.ind = T)
    if (length(event_indices) > 0) {
      circle_colors[event_indices] = 18
      circle_size_override = circle_colors
      
      ifelse(n_event > 0, event_color <- event_colors[[1]], event_color <- event_colors[[2]])
      
      make_visual(circle_colors, label_axes = F, lwd = lwd, fg = event_color, circle_size_override = circle_size_override, color_scale = clear_color)
      par(new = T)
    }
  }
  
  for (ramp_type in ramp_draw) {
    
    event_indices = which(event_locations == ramp_type*2 | event_locations == ramp_type*3, arr.ind = T)
    
    if (length(event_indices) > 0) {
      # sort by col then row
      event_indices = event_indices[order(event_indices[,"col"], event_indices[,"row"]),]
      # add lines for ramps
      ramp_list = list(event_indices[1,]) # storage for different ramps
      counter = 1
      for (n_row in 1:(nrow(event_indices) - 1)) { # search over the rows
        if (event_indices[[n_row, "col"]] == event_indices[[n_row+1, "col"]] & diff(event_indices[c(n_row, n_row+1), "row"]) == 1) { # if the next row is part of the same ramp
          ramp_list[[counter]] = rbind(ramp_list[[counter]], event_indices[n_row+1,]) # store new point
        } else { # new ramp
          ramp_list = c(ramp_list, list(event_indices[n_row + 1,]))
          counter = counter + 1
        }
      }
      
      for (n_ramp in ramp_list) {
        ifelse(ramp_type > 0, event_color <- event_colors[[1]], event_color <- event_colors[[2]])
        lines(x = n_ramp[,"col"], y = nrow(event_locations) - n_ramp[,"row"] + 1, col = event_color, lwd = 1.4)
        par(new = T)
      }
    }
  }
  
}