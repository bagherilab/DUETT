make_visual_PID <- function(event_locations, event_draw = c(-3,-1,1,3), ramp_draw = c(-1,1), event_colors = NULL, lwd = 1.2, box_resize = 1) {
  
  source("support_functions/plotting/make_boxes.R")
  clear_color = "#FFFFFF00"
  
  if (is.null(event_colors)) {event_colors = c("red", "blue")}
  
  plot(1, type = "n", xlim = c(0, ncol(event_locations)+1), ylim = c(0, nrow(event_locations)+1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  par(new = T)
  # coloring swing events
  for (n_event in event_draw) {
    circle_colors = event_locations * NA
    
    event_indices = which(event_locations == n_event, arr.ind = T)
    if (length(event_indices) > 0) {
      circle_colors[event_indices] = 18
      circle_size_override = circle_colors
      
      ifelse(n_event %in% c(1,3,-1.5), event_color <- event_colors[[1]], event_color <- event_colors[[2]])
      
      make_boxes(data_mat = circle_colors, lwd = lwd, fg = event_color, circle_size_override = circle_size_override, color_scale = clear_color, box_resize = box_resize)
      par(new = T)
    }
  }
  
  # coloring ramps
  for (ramp_type in ramp_draw) {
    
    # find ramps as 2's, 1.5's, or 3's
    event_indices = which(event_locations == ramp_type*2 | event_locations == ramp_type*3 | event_locations == ramp_type*1.5, arr.ind = T)
    
    if (length(event_indices) > 0) {
      # sort by col then row
      event_indices = event_indices[order(event_indices[,"col"], event_indices[,"row"]),]
      # add lines for ramps
      ramp_list = list(event_indices[1,, drop = F]) # storage for different ramps
      
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
        # if (n_ramp[,"col"] == 14) {browser()}
        ifelse(ramp_type > 0, event_color <- event_colors[[1]], event_color <- event_colors[[2]])
        
        # detect if single row
        if (is.null(dim(n_ramp))) {
          n_ramp = matrix(n_ramp, ncol = 2, dimnames = list(NULL, c("row", "col")))
        }
        
        # line_error = try(lines(x = n_ramp[,"col"], y = nrow(event_locations) - n_ramp[,"row"] + 1, col = event_color, lwd = 1.4), silent = T)
        # if (class(line_error) == "try-error") {browser()}
        lines(x = n_ramp[,"col"], y = nrow(event_locations) - n_ramp[,"row"] + 1, col = event_color, lwd = 2)
        par(new = T)
      }
    }
  }
  
}