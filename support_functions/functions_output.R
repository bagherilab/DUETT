output_graphic <- observe({
  if (input$graph_output == 0) return()
  
  # only output file when button is pressed (I don't get this logic)
  isolate({
    # store number form of outfile_format
    outfile_format = as.numeric(input$outfile_format)
    
    width = get_val$plotting_parameters()$width
    height = get_val$plotting_parameters()$height
    
    # print replicates
    if (get_val$print_replicate()) {
      return_list = calc$replicate_events()
      event_storage = return_list[[1]]
      concurrent_events = return_list[[2]]
      data_mat = get_val$data()
      
      for (n_file in 1:length(event_storage)) {
        filename = paste(input$outfile, "_", n_file, ".pdf", sep = "")
        pdf(filename, width = width, height = height)
        make_visual(data_mat[[n_file]], event_storage[[n_file]][[1]], 
                    concurrent_events[[n_file]], get_val$plotting_parameters()$log_colors,
                    numbering = get_val$plotting_parameters()$numbering, 
                    numbering_offset = get_val$plotting_parameters()$numbering_offset,
                    numbering_interval = get_val$plotting_parameters()$numbering_interval,
                    axis_label_resize = get_val$plotting_parameters()$axis_label_resize,
                    box_resize = get_val$plotting_parameters()$box_resize,
                    diverging = get_val$plotting_parameters()$diverging)
        dev.off()
      }
    }
    
    # print shared
    return_list = calc$events()
    event_locations = return_list[[1]]
    event_details = return_list[[2]]
    concurrent_events = return_list[[3]]
    filename = paste(input$outfile, ".pdf", sep = "")
    
    pdf(filename, width = width, height = height)
    make_visual(get_val$mean_data(), event_locations, concurrent_events, get_val$plotting_parameters()$log_colors,
                numbering = get_val$plotting_parameters()$numbering, 
                numbering_offset = get_val$plotting_parameters()$numbering_offset,
                numbering_interval = get_val$plotting_parameters()$numbering_interval,
                axis_label_resize = get_val$plotting_parameters()$axis_label_resize,
                box_resize = get_val$plotting_parameters()$box_resize,
                diverging = get_val$plotting_parameters()$diverging)
    dev.off()
    
    ############# columns #############
    
    num_plots = 16
    
    # print replicates
    if (get_val$print_replicate()) {
      data_mat = get_val$data()
      return_list = calc$replicate_events()
      event_storage = return_list[[1]]
      concurrent_events = return_list[[2]]
      for (n_file in 1:length(data_mat)) {
        filename = paste(input$outfile, "_columns_", n_file, ".pdf", sep = "")
        pdf(filename, width = width, height = height)
        
        counter = 1
        columns_to_show = get_val$plotting_parameters()$columns_to_show
        suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / num_plots), each = num_plots)))
        
        for (col_group in col_groups) {
          counter = counter + 1
          my_col_group <- col_group
          my_i <- counter
          plot_name = paste("plot", my_i, sep = "")
          make_col_detail_plots(
            my_col_group, data_mat[n_file], event_storage[[n_file]][[1]],  # be sure to pass data_mat as a list
            event_storage[[n_file]][[2]], concurrent_events[[n_file]],
            cutoffs = list(P = get_val$P(), I = get_val$I(), R = get_val$R(), p_value = get_val$p_value(), linear_coeff = get_val$linear_coeff(), dws = get_val$dws()), 
            event_colors = c("red", "blue"),
            ylim = get_val$plotting_parameters()$ylim,
            xaxis_offset = get_val$numbering_offset(),
            xaxis_interval = get_val$numbering_interval())
        }
        dev.off()
      }
    }
    
    # print shared
    filename = paste(input$outfile, "_columns.pdf", sep = "")
    return_list = calc$events()
    event_locations = return_list[[1]]
    event_details = return_list[[2]]
    concurrent_events = return_list[[3]]
    pdf(filename, width = width, height = height)
    
    counter = 1
    columns_to_show = get_val$plotting_parameters()$columns_to_show
    suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / num_plots), each = num_plots)))
    
    for (col_group in col_groups) {
      counter = counter + 1
      my_col_group <- col_group
      my_i <- counter
      plot_name = paste("plot", my_i, sep = "")
      make_col_detail_plots(
        my_col_group, 
        get_val$data(), event_locations, event_details, concurrent_events,
        cutoffs = list(P = get_val$P(), I = get_val$I(), R = get_val$R(), p_value = get_val$p_value(), linear_coeff = get_val$linear_coeff(), dws = get_val$dws()), 
        event_colors = c("red", "blue"),
        ylim = get_val$plotting_parameters()$ylim,
        xaxis_offset = get_val$numbering_offset(),
        xaxis_interval = get_val$numbering_interval())
    }
    dev.off()
  })
})

# make output table
output_table <- observe({
  if (input$table_output == 0) return()
  
  isolate({
    
    # print replicates too
    if (get_val$print_replicate()) {
      return_list = calc$replicate_events()
      event_storage = return_list[[1]]
      concurrent_events = return_list[[2]]
      
      for (n_file in 1:length(event_storage)) {
        write.table(event_storage[[n_file]][[1]], file = paste(input$outfile, "_", n_file, ".csv", sep = ""), sep = ",", quote = F, row.names = F)
        
        if (nrow(concurrent_events[[n_file]]) >= 1) {
          event_type1 = sapply(1:nrow(concurrent_events[[n_file]]), function(i) event_storage[[n_file]][[1]][[concurrent_events[[n_file]][i,1], concurrent_events[[n_file]][i,2]]])
          event_type2 = sapply(1:nrow(concurrent_events[[n_file]]), function(i) event_storage[[n_file]][[1]][[concurrent_events[[n_file]][i,3], concurrent_events[[n_file]][i,4]]])
        } else {
          event_type1 = c()
          event_type2 = c()
        }
        concurrent_events_table = cbind(concurrent_events[[n_file]], event_type1, event_type2)
        write.table(concurrent_events_table, file = paste(input$outfile, "_concurrent_events_", n_file, ".csv", sep = ""), sep = ",", quote = F, row.names = F)
      }
    }
    
    # print events
    return_list = calc$events()
    event_locations = return_list[[1]]
    concurrent_events = return_list[[3]]
    write.table(event_locations, file = paste(input$outfile, ".csv", sep = ""), sep = ",", quote = F, row.names = F)
    
    # print concurrent_events
    if (nrow(concurrent_events) >= 1) {
      event_type1 = sapply(1:nrow(concurrent_events), function(i) event_locations[[concurrent_events[i,1], concurrent_events[i,2]]])
      event_type2 = sapply(1:nrow(concurrent_events), function(i) event_locations[[concurrent_events[i,3], concurrent_events[i,4]]])
    } else {
      event_type1 = c()
      event_type2 = c()
    }
    concurrent_events_table = cbind(concurrent_events, event_type1, event_type2)
    write.table(concurrent_events_table, file = paste(input$outfile, "_concurrent_events.csv", sep = ""), sep = ",", quote = F, row.names = F)
    
    
  })
})

# table details
output_table_details <- observe({
  
  if (input$table_details_output == 0) return()
  
  write_table <- function(table_obj, file_ID, replicate_num = NULL) {
    if (is.null(replicate_num)) {
      write.table(table_obj, file = paste(input$outfile, "_", file_ID, ".csv", sep = ""), sep = ",", quote = F)
    } else {
      write.table(table_obj, file = paste(input$outfile, "_", file_ID, "_", replicate_num, ".csv", sep = ""), sep = ",", quote = F)
    }
  }
  
  isolate({
    if (get_val$print_replicate()) {
      event_storage = calc$replicate_events()[[1]]
      
      for (n_file in 1:length(event_details)) {
        write_table(event_storage[[n_file]][[2]]$P_values, "P", n_file)
        write_table(event_storage[[n_file]][[2]]$I_values, "I", n_file)
        write_table(event_storage[[n_file]][[2]]$R_values, "R", n_file)
        write_table(event_storage[[n_file]][[2]]$p_values, "p_values", n_file)
        write_table(event_storage[[n_file]][[2]]$betas, "betas", n_file)
        write_table(event_storage[[n_file]][[2]]$dws, "dws", n_file)
      }
    }
    
    event_details = calc$events()[[2]]
    
    write_table(event_details$P_values, "P", NULL)
    write_table(event_details$I_values, "I", NULL)
    write_table(event_details$R_values, "R", NULL)
    write_table(event_details$p_values, "p_values", NULL)
    write_table(event_details$betas, "betas", NULL)
    write_table(event_details$dws, "dws", NULL)
  })
})


# make table of UI settings
output_UI_settings <- observe({
  if (input$table_UI_settings == 0) return()
  
  isolate({
    settings_names = names(input)
    temp = sapply(settings_names, function(i) input[[i]])
    temp[c("graph_output", "table_details_output", "table_output", "update", "table_UI_settings")] = NULL
    temp = matrix(temp, ncol = 1, dimnames = list(names(temp), NULL))
    write.table(temp, file = paste(input$outfile, "_UI_settings.csv", sep = ""), sep = ",", quote = F)
  })
})