# server.R

rm(list=ls())

source("support_functions/load_data.R")
source("support_functions/ShapeSeq_events.R")
source("support_functions/plotting/plot_dump_PID.R")
source("support_functions/utility_functions.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  # file inputs and outputs
  get_data_file <- reactive({input$data_file})
  
  # plotting parameters
  get_width <- reactive({input$width})
  get_height <- reactive({input$height})
  get_numbering <- reactive({input$numbering})
  get_numbering_interval <- reactive({input$numbering_interval})
  get_axis_label_resize <- reactive({input$axis_label_resize})
  get_ylim <- reactive({as.numeric(strsplit(input$ylim, ",")[[1]])})
  get_columns_display <- reactive({input$column_display})
  get_custom_columns <- reactive({as.numeric(strsplit(input$custom_columns, ",")[[1]])})
  
  # PID parameters
  get_P <- reactive({input$P})
  get_I <- reactive({input$I})
  get_D <- reactive({input$D})
  get_I_length <- reactive({input$window_size}) # not actually an input
  get_window_size <- reactive({input$window_size})
  get_noise_length <- reactive({input$noise_length})
  get_event_gap <- reactive({input$event_gap})
  
  # linear ramp paramers
  get_ramp_window <- reactive({input$ramp_length})
  get_p_value <- reactive({input$p_value})
  get_b_min <- reactive({input$b_min})
  get_dwp <- reactive({input$dwp})
  
  get_concurrent_distance <- reactive({input$concurrent_distance})
  
  get_update <- reactive({input$update})
  
  #################### calculate for events ####################
  get_data <- reactive({
    ifelse(is.null(input$data_file), data_file <- "example_data/SRP_EQ_Rep1_rho_table.txt", data_file <- get_data_file()$datapath)
    data_mat = load_data(data_file)
  })
  get_ncol <- reactive({ncol(get_data())})
  get_nrow <- reactive({nrow(get_data())})
  
  # PID values
  calc_D_values <- reactive({
    data_mat = get_data()
    window_size = get_window_size()
    if (window_size == 0) {D_values = data_mat * 0}
    D_values = diff_data(data_mat, window_size)
  })
  calc_I_values <- reactive({
    D_values = calc_D_values()
    num_row = get_nrow()
    num_col = get_ncol()
    I_values = matrix(NA, nrow = num_row, ncol = num_col)
    if (get_I_length() > num_row) {return(I_values)}
    for (n_col in 1:ncol(D_values)) {
      I_values[(get_I_length()+1):num_row,n_col] = sapply(1:(num_row-get_I_length()), function(i) sum(D_values[i:(i+get_I_length()),n_col])) / get_I_length()
    }
    I_values
  })
  calc_P_values <- reactive({
    D_values = calc_D_values()
    P_values = D_values / (get_data() - D_values)
  })
  
  # linear ramp values
  calc_linear_values <- reactive({
    library(lmtest)
    ramp_window = get_ramp_window()
    data_mat = get_data()
    p_values = data_mat * NA
    betas = p_values
    dwp = p_values
    
    if (ramp_window > nrow(data_mat)) { # error
      warning("Ramp window is higher than number of rows!")
      return(list(p_values = p_values, betas = betas, dwp = dwp))
    } else if (ramp_window <= 1) {
      return(list(p_values = p_values, betas = betas, dwp = dwp))
    }
    
    for (n_col in 1:ncol(p_values)) {
      for (n_window in 1:(nrow(p_values) - ramp_window - 1)) {
        x = 1:ramp_window
        y = data_mat[n_window:(n_window + ramp_window - 1), n_col]
        
        if (!any(is.na(y))) {
          lm_summary = summary(lm(y ~ x))
          
          p_values[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,4]]
          betas[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,1]]
          
          # check that y isn't all 0's (dwtest doesn't like that)
          if (sum(sapply(y, function(i) i==0)) == length(y)) {
            dwp[[n_window + ramp_window - 1, n_col]] = 1
          } else {
            dwp[[n_window + ramp_window - 1, n_col]] = dwtest(y ~ x)$p.value
          }
        }
      }
    }
    list(p_values = p_values, betas = betas, dwp = dwp)
  })
  
  get_events <- reactive({
    if (get_update() == 0) {
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({
        
        D_values = calc_D_values()
        I_values = calc_I_values()
        P_values = calc_P_values()
        linear_values = calc_linear_values()
        
        event_storage = do.call(ShapeSeq_events, list(P_values, I_values, D_values, linear_values, get_data(), get_window_size(), get_I_length(), get_ramp_window(), get_noise_length(), get_event_gap(), cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), b_min = get_b_min(), dwp = get_dwp())))
        
        concurrent_events = find_concurrent_events(event_storage[[1]], concurrent_distance = get_concurrent_distance(), comparison_point = "start")
        return_list = c(event_storage, list(concurrent_events))
      })
    }
  })
  
  get_columns_to_show <- reactive({
    columns_display = get_columns_display()
    if (columns_display == 1) {
      columns_to_show = get_custom_columns()
    } else if (columns_display == 2) {
      columns_to_show = 1:get_ncol()
    } else  if (columns_display == 3) {
      event_locations = get_events()[[1]]
      columns_to_show = which(colSums((event_locations != 0) & !is.na(event_locations)) > 0)
    }
  })
  
  get_plotting_parameters <- reactive({
    if (get_update() == 0) {
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({
        list(numbering = get_numbering(),
             numbering_interval = get_numbering_interval(),
             axis_label_resize = get_axis_label_resize(),
             columns_to_show = get_columns_to_show(),
             ylim = get_ylim(),
             height = get_height(),
             width = get_width())
      })
    }
  })
  
  #################### plotting ####################
  
  master_plotting <- reactive({
    
    return_list = get_events()
    if (identical(NA, return_list)) {return()}
    event_locations = return_list[[1]]
    event_details = return_list[[2]]
    concurrent_events = return_list[[3]]
    
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    
    # heatmap
    local({output[["plot1"]] <- renderPlot({
      make_visual(get_data(), event_locations, concurrent_events, 
                  numbering = get_plotting_parameters()$numbering,
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize)})})
    
    # details
    counter = 1
    columns_to_show = get_plotting_parameters()$columns_to_show
    suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / 4), each = 4)))
    
    for (col_group in col_groups) {
      counter = counter + 1
      local({
        my_col_group <- col_group
        local_counter <- counter
        plot_name = paste("plot", local_counter, sep = "")
        output[[plot_name]] <- renderPlot({
          make_col_detail_plots(
            my_col_group, 
            get_data(), event_locations, event_details, concurrent_events,
            cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), b_min = get_b_min(), dwp = get_dwp()), 
            event_colors = c("red", "blue"),
            ylim = get_plotting_parameters()$ylim)
        })
      })
    }
  })
  
  # interface to put plots to output
  output$plots <- renderUI({
    master_plotting()
    
    # check if update was called at least once
    if (identical(get_plotting_parameters(), NA)) {return()}
    
    num_plots = ceiling(length(get_plotting_parameters()$columns_to_show) / 4)
    plot_output_list <- lapply(1:(num_plots+1), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 72 * get_plotting_parameters()$height, width = 72 * get_plotting_parameters()$height)
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
    concurrent_events = return_list[[3]]
    
    write.table(event_locations, file = paste(input$outfile, ".csv", sep = ""), sep = ",", quote = F, row.names = F)
    write.table(concurrent_events, file = paste(input$outfile, "_concurrent_events.csv", sep = ""), sep = ",", quote = F, row.names = F)
  })
  
  output_table_details <- observe({
    
    if (input$table_details_output == 0) return()
    
    event_details = get_events()[[2]]
    
    write.table(event_details$P_values, file = paste(input$outfile, "_P.csv", sep = ""), sep = ",", quote = F)
    write.table(event_details$I_values, file = paste(input$outfile, "_I.csv", sep = ""), sep = ",", quote = F)
    write.table(event_details$D_values, file = paste(input$outfile, "_D.csv", sep = ""), sep = ",", quote = F)
    write.table(event_details$p_values, file = paste(input$outfile, "_p_values.csv", sep = ""), sep = ",", quote = F)
    write.table(event_details$betas, file = paste(input$outfile, "_betas.csv", sep = ""), sep = ",", quote = F)
    write.table(event_details$dwp, file = paste(input$outfile, "_dwp.csv", sep = ""), sep = ",", quote = F)
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
      concurrent_events = return_list[[3]]
      
      width = get_plotting_parameters()$width
      height = get_plotting_parameters()$height
      filename = paste(input$outfile, ".pdf", sep = "")
      
      pdf(filename, width = width, height = height)
      make_visual(get_data(), event_locations, concurrent_events, 
                  numbering = get_plotting_parameters()$numbering, 
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize)
      dev.off()
      
      filename = paste(input$outfile, "_columns.pdf", sep = "")
      pdf(filename, width = width, height = height)
      
      counter = 1
      columns_to_show = get_plotting_parameters()$columns_to_show
      suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / 4), each = 4)))
      
      for (col_group in col_groups) {
        counter = counter + 1
        my_col_group <- col_group
        my_i <- counter
        plot_name = paste("plot", my_i, sep = "")
        make_col_detail_plots(
          my_col_group, 
          get_data(), event_locations, event_details, concurrent_events,
          cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), b_min = get_b_min(), dwp = get_dwp()), 
          event_colors = c("red", "blue"),
          ylim = get_plotting_parameters()$ylim)
      }
      dev.off()
    })
  })
})