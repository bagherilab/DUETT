# server.R

rm(list=ls())

source("support_functions/load_data.R")
source("support_functions/sanitize.R")
source("support_functions/ShapeSeq_events.R")
source("support_functions/plotting/plot_dump_PID.R")
source("support_functions/utility_functions.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/plotting/make_visual.R")

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  # file inputs and outputs
  get_data_file <- reactive({input$data_file})
  get_diverging <- reactive({input$diverging})
  get_width <- reactive({input$width})
  get_height <- reactive({input$height})
  
  # plotting parameters
  get_log_colors <- reactive({input$log_colors})
  get_numbering <- reactive({input$numbering})
  get_numbering_offset <- reactive({sanitize(input$numbering_offset, "numbering_offset")})
  get_numbering_interval <- reactive({sanitize(input$numbering_interval, "numbering_interval")})
  get_axis_label_resize <- reactive({sanitize(input$axis_label_resize, "axis_label_resize")})
  get_box_resize <- reactive({sanitize(input$box_resize, "box_resize")})
  get_ylim <- reactive({sanitize(input$ylim, "ylim")})
  get_columns_display <- reactive({input$column_display})
  get_custom_columns <- reactive({sanitize(input$custom_columns, "custom_columns")})
  
  # PID parameters
  get_P <- reactive({sanitize(input$P, "P")})
  get_I <- reactive({sanitize(input$I, "I")})
  get_I_length <- reactive({
    I_length = sanitize(input$I_length, "I_length")
    if (I_length == "default") {I_length = get_window_size()}
    I_length
  })
  get_D <- reactive({sanitize(input$D, "D")})
  # get_I_length <- reactive({sanitize(input$window_size, "window_size")}) # not actually an input
  get_window_size <- reactive({sanitize(input$window_size, "window_size")})
  get_event_gap <- reactive({sanitize(input$event_gap, "event_gap")})
  get_noise_length <- reactive({sanitize(input$noise_length, "noise_length")})
  
  # linear ramp paramers
  get_ramp_length <- reactive({sanitize(input$ramp_length, "ramp_length")})
  get_p_value <- reactive({sanitize(input$p_value, "p_value")})
  get_linear_coeff <- reactive({sanitize(input$linear_coeff, "linear_coeff")})
  get_dwp <- reactive({sanitize(input$dwp, "dwp")})
  
  get_concurrent_distance <- reactive({sanitize(input$concurrent_distance, "concurrent_distance")})
  get_conc_event_types <- reactive({
    event_types = c()
    if (input$concurrent_swings) {event_types = c(-1, event_types, 1)}
    if (input$concurrent_ramps) {event_types = c(-2, event_types, 2)}
    event_types
  })
  
  get_update <- reactive({input$update})
  
  #################### calculate for events ####################
  get_data <- reactive({
    ifelse(is.null(input$data_file), data_file <- "example_data/SRP_Gln111_Rep1_rho_table.txt", data_file <- get_data_file()$datapath)
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
    P_values = D_values / abs(get_data() - D_values)
  })
  
  # linear ramp values
  calc_linear_values <- reactive({
    library(lmtest)
    ramp_length = get_ramp_length()
    data_mat = get_data()
    p_values = data_mat * NA
    betas = p_values
    dwp = p_values
    
    if (ramp_length > nrow(data_mat)) { # error
      warning("Ramp window is higher than number of rows!")
      return(list(p_values = p_values, betas = betas, dwp = dwp))
    } else if (ramp_length <= 1) {
      return(list(p_values = p_values, betas = betas, dwp = dwp))
    }
    
    for (n_col in 1:ncol(p_values)) {
      for (n_window in 1:(nrow(p_values) - ramp_length - 1)) {
        x = 1:ramp_length
        y = data_mat[n_window:(n_window + ramp_length - 1), n_col]
        
        if (!any(is.na(y))) {
          lm_summary = summary(lm(y ~ x))
          
          p_values[[n_window + ramp_length - 1, n_col]] = lm_summary$coefficients[[2,4]]
          betas[[n_window + ramp_length - 1, n_col]] = lm_summary$coefficients[[2,1]]
          
          # check that y is not all the same (dwtest doesn't like that)
          if (length(unique(y)) == 1) {
            dwp[[n_window + ramp_length - 1, n_col]] = 1
          } else {
            dwp[[n_window + ramp_length - 1, n_col]] = dwtest(y ~ x)$p.value
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
        
        event_storage = do.call(ShapeSeq_events, list(P_values, I_values, D_values, linear_values, get_data(), get_window_size(), get_I_length(), get_ramp_length(), get_noise_length(), get_event_gap(), cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dwp = get_dwp())))
        
        concurrent_events = find_concurrent_events(event_storage[[1]], concurrent_distance = get_concurrent_distance(), comparison_point = "start", event_types = get_conc_event_types())
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
        list(log_colors = get_log_colors(),
             numbering = get_numbering(),
             numbering_offset = get_numbering_offset(),
             numbering_interval = get_numbering_interval(),
             axis_label_resize = get_axis_label_resize(),
             box_resize = get_box_resize(),
             columns_to_show = get_columns_to_show(),
             ylim = get_ylim(),
             height = get_height(),
             width = get_width(),
             diverging = get_diverging())
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
      make_visual(get_data(), event_locations, concurrent_events, get_plotting_parameters()$log_colors,
                  numbering = get_plotting_parameters()$numbering,
                  numbering_offset = get_plotting_parameters()$numbering_offset,
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize,
                  box_resize = get_plotting_parameters()$box_resize,
                  diverging = get_plotting_parameters()$diverging)})})
    
    # details
    counter = 1
    num_plots = 4 # doesn't actually work
    columns_to_show = get_plotting_parameters()$columns_to_show
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
            get_data(), event_locations, event_details, concurrent_events,
            cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dwp = get_dwp()), 
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
    
    isolate({
      return_list = get_events()
      event_locations = return_list[[1]]
      concurrent_events = return_list[[3]]
      
      write.table(event_locations, file = paste(input$outfile, ".csv", sep = ""), sep = ",", quote = F, row.names = F)
      
      list(window_size = get_window_size(), I_length = get_I_length(), ramp_length = get_ramp_length(), noise_length = get_noise_length(), event_gap = get_event_gap(), P = get_P(), I = get_I(), D = get_D(), ramp_p_value = get_p_value(), linear_coeff = get_linear_coeff(), dwp = get_dwp(), concurrent_distance = get_concurrent_distance(), concurrent_event_types = get_conc_event_types())
      
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
  
  output_table_details <- observe({
    
    if (input$table_details_output == 0) return()
    
    isolate({
      event_details = get_events()[[2]]
      
      write.table(event_details$P_values, file = paste(input$outfile, "_P.csv", sep = ""), sep = ",", quote = F)
      write.table(event_details$I_values, file = paste(input$outfile, "_I.csv", sep = ""), sep = ",", quote = F)
      write.table(event_details$D_values, file = paste(input$outfile, "_D.csv", sep = ""), sep = ",", quote = F)
      write.table(event_details$p_values, file = paste(input$outfile, "_p_values.csv", sep = ""), sep = ",", quote = F)
      write.table(event_details$betas, file = paste(input$outfile, "_betas.csv", sep = ""), sep = ",", quote = F)
      write.table(event_details$dwp, file = paste(input$outfile, "_dwp.csv", sep = ""), sep = ",", quote = F)
    })
  })
  
  #################### UI settings ####################
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
      make_visual(get_data(), event_locations, concurrent_events, get_plotting_parameters()$log_colors,
                  numbering = get_plotting_parameters()$numbering, 
                  numbering_offset = get_plotting_parameters()$numbering_offset,
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize,
                  box_resize = get_plotting_parameters()$box_resize,
                  diverging = get_plotting_parameters()$diverging)
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
          cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dwp = get_dwp()), 
          event_colors = c("red", "blue"),
          ylim = get_plotting_parameters()$ylim)
      }
      dev.off()
    })
  })
})