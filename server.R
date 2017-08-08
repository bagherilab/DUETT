# server.R

rm(list=ls())

source("support_functions/load_data.R")
source("support_functions/ShapeSeq_events.R")
source("support_functions/plotting/plot_dump_PID.R")

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  get_width <- reactive({width = input$width})
  get_height <- reactive({height = input$height})
  
  get_ylim <- reactive({ylim = as.numeric(strsplit(input$ylim, ",")[[1]])})
  get_show_columns <- reactive({show_columns = as.numeric(strsplit(input$show_columns, ",")[[1]])})
  get_show_all <- reactive({input$show_all_columns})
  get_num_plots <- reactive({
    ifelse(get_show_all(), num_plots <- get_ncol(), num_plots <- length(get_show_columns()))
    num_plots = ceiling(num_plots / 4)
  })
  get_parameters <- reactive({list(cutoffs = list(P = input$P, I = input$I, D = input$D, p_value = input$p_value, b_min = input$b_min, dwp = input$dwp), window_size = input$window_size, I_length = input$window_size, noise_length = input$noise_length, event_gap = input$event_gap)})
  
  get_ncol <- reactive({ncol(get_data())})
  get_data_file <- reactive({input$data_file})
  get_data <- reactive({
    ifelse(is.null(input$data_file), data_file <- "example_data.csv", data_file <- get_data_file()$name)
    data_mat = load_data(data_file)
  })
  
  get_update <- reactive({input$update})
  
  # get_events <- reactive({
  get_events <- reactive({
    if (get_update() == 0) {
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({return_list = do.call(ShapeSeq_events, c(list(get_data()), get_parameters()))})
    }
  })
  
  make_quad_plot_func <- function(col_group, data_mat, event_locations, event_details) {
    par(mfrow = c(2,2))
    make_quad_plot(
      col_group,
      data_mat, event_locations, event_details,
      cutoffs = cutoffs, 
      event_colors = c("red", "blue"),
      main = n_col,
      ylim = get_ylim())
  }
  
  #################### plotting ####################
  
  master_plotting <- reactive({
    
    return_list = get_events()
    if (identical(NA, return_list)) {return()}
    event_locations = return_list[[1]]
    event_details = return_list[[2]]
    
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    
    # heatmap
    local({output[["plot1"]] <- renderPlot({heatmap_plot(get_data(), event_locations)})})
    
    # details
    ifelse(get_show_all(), show_columns <- 1:get_ncol(), show_columns <- get_show_columns())
    counter = 1
    suppressWarnings(col_groups <- split(show_columns, rep(1:ceiling(length(show_columns) / 4), each = 4)))
    
    for (col_group in col_groups) {
      counter = counter + 1
      local({
        my_col_group <- col_group
        my_i <- counter
        plot_name = paste("plot", my_i, sep = "")
        output[[plot_name]] <- renderPlot({
          make_quad_plot(
            my_col_group, 
            get_data(), event_locations, event_details,
            cutoffs = get_parameters(), 
            event_colors = c("red", "blue"),
            ylim = get_ylim())
        })
      })
    }
    
  })
  
  
  # interface to put plots to output
  output$plots <- renderUI({
    master_plotting()
    plot_output_list <- lapply(1:(get_num_plots()+1), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 72 * get_height(), width = 72 * get_width())
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  #################### table ####################
  # make output table
  output_table <- observe({
    if (input$table_output == 0) return()
    
    return_list = get_events()
    event_locations = return_list[[1]]
    
    write.table(event_locations, file = paste(input$outfile, ".csv", sep = ""), sep = ",", quote = F)
  })
  
  #################### output figure ####################
  # make figure
  output_graphic <- observe({
    if (input$graph_output == 0) return()
    
    # only output file when button is pressed (I don't get this logic)
    isolate({
      # store number form of outfile_format
      outfile_format = as.numeric(input$outfile_format)
      
      return_list = get_events()
      event_locations = return_list[[1]]
      event_details = return_list[[2]]
      
      filename = paste(input$outfile, ".pdf", sep = "")
      pdf(filename, width = get_width(), height = get_height())
      heatmap_plot(get_data(), event_locations)
      dev.off()
      
      filename = paste(input$outfile, "_columns.pdf", sep = "")
      pdf(filename, width = get_width(), height = get_height())
      ifelse(get_show_all(), show_columns <- 1:get_ncol(), show_columns <- get_show_columns())
      counter = 1
      suppressWarnings(col_groups <- split(show_columns, rep(1:ceiling(length(show_columns) / 4), each = 4)))
      
      for (col_group in col_groups) {
        counter = counter + 1
        my_col_group <- col_group
        my_i <- counter
        plot_name = paste("plot", my_i, sep = "")
        make_quad_plot(
          my_col_group, 
          get_data(), event_locations, event_details,
          cutoffs = get_parameters(), 
          event_colors = c("red", "blue"),
          ylim = get_ylim())
      }
      dev.off()
    })
  })
})