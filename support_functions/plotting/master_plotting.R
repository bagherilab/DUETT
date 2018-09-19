master_plotting <- reactive({
  
  # browser()
  return_list = calc$events()
  # browser()
  if (identical(NA, return_list)) {return()}
  event_locations = return_list[[1]]
  event_details = return_list[[2]]
  concurrent_events = return_list[[3]]
  
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  
  # heatmap
  local({output[["plot1"]] <- renderPlot({
    make_visual(get_val$mean_data(), event_locations, concurrent_events, get_val$plotting_parameters()$log_colors,
                numbering = get_val$plotting_parameters()$numbering,
                numbering_offset = get_val$plotting_parameters()$numbering_offset,
                numbering_interval = get_val$plotting_parameters()$numbering_interval,
                axis_label_resize = get_val$plotting_parameters()$axis_label_resize,
                box_resize = get_val$plotting_parameters()$box_resize,
                diverging = get_val$plotting_parameters()$diverging)})})
  
  # details
  counter = 1
  num_plots = 16
  columns_to_show = calc$columns_to_show()
  suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / num_plots), each = num_plots)))
  
  for (col_group in col_groups) {
    counter = counter + 1
    local({
      my_col_group <- col_group
      local_counter <- counter
      plot_name = paste("plot", local_counter, sep = "")
      output[[plot_name]] <- renderPlot({
        make_col_detail_plots(
          my_col_group, 
          get_val$data(), event_locations, event_details, concurrent_events,
          cutoffs = list(P = get_val$P(), I = get_val$I(), R = get_val$R(), p_value = get_val$p_value(), linear_coeff = get_val$linear_coeff(), dws = get_val$dws()), 
          event_colors = c("red", "blue"),
          ylim = get_val$plotting_parameters()$ylim,
          xaxis_offset = get_val$numbering_offset(),
          xaxis_interval = get_val$numbering_interval())
      })
    })
  }
})