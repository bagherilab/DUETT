# server.R

rm(list=ls())

source("support_functions/load_data.R")
source("support_functions/sanitize.R")
source("support_functions/DUETT_events.R")
source("support_functions/merge_replicates.R")
source("support_functions/plotting/plot_dump_PIR.R")
source("support_functions/utility_functions.R")
source("support_functions/find_concurrent_events.R")
source("support_functions/optimize_thresholds.R")
source("support_functions/plotting/make_visual.R")
source("support_functions/optimize_modal.R")

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  # file inputs and outputs
  get_data_file <- reactive({input$data_file})
  get_print_replicate <- reactive({input$print_replicate})
  get_diverging <- reactive({input$diverging})
  get_width <- reactive({input$width})
  get_height <- reactive({input$height})
  
  # plotting parameters
  get_agreement <- reactive({input$agreement})
  get_log_colors <- reactive({input$log_colors})
  get_numbering <- reactive({input$numbering})
  get_numbering_offset <- reactive({sanitize(input$numbering_offset, "numbering_offset")})
  get_numbering_interval <- reactive({sanitize(input$numbering_interval, "numbering_interval")})
  get_axis_label_resize <- reactive({sanitize(input$axis_label_resize, "axis_label_resize")})
  get_box_resize <- reactive({sanitize(input$box_resize, "box_resize")})
  get_ylim <- reactive({sanitize(input$ylim, "ylim")})
  get_columns_display <- reactive({input$column_display})
  get_custom_columns <- reactive({sanitize(input$custom_columns, "custom_columns")})
  
  # PIR parameters
  get_P <- reactive({sanitize(input$P, "P")})
  get_I <- reactive({sanitize(input$I, "I")})
  get_I_length <- reactive({
    I_length = sanitize(input$I_length, "I_length")
    if (I_length == "default") {I_length = get_window_size()}
    I_length
  })
  get_R <- reactive({sanitize(input$R, "R")})
  # get_I_length <- reactive({sanitize(input$window_size, "window_size")}) # not actually an input
  get_window_size <- reactive({sanitize(input$window_size, "window_size")})
  get_event_gap <- reactive({sanitize(input$event_gap, "event_gap")})
  get_duration <- reactive({sanitize(input$duration, "duration")})
  
  # linear ramp paramers
  get_ramp_length <- reactive({sanitize(input$ramp_length, "ramp_length")})
  get_p_value <- reactive({sanitize(input$p_value, "p_value")})
  get_linear_coeff <- reactive({sanitize(input$linear_coeff, "linear_coeff")})
  get_dws <- reactive({sanitize(input$dws, "dws")})
  
  get_concurrent_distance <- reactive({sanitize(input$concurrent_distance, "concurrent_distance")})
  get_conc_event_types <- reactive({
    event_types = c()
    if (input$concurrent_swings) {event_types = c(-1, event_types, 1)}
    if (input$concurrent_ramps) {event_types = c(-2, event_types, 2)}
    event_types
  })
  
  get_update <- reactive({input$update})
  get_optimize <- reactive({input$optimize})
  
  #################### calculate for events ####################
  get_data <- reactive({
    # ifelse(is.null(input$data_file), data_file <- list("example_data/SRP_wt_Rep1_rho_table.txt", "example_data/SRP_wt_Rep2_rho_table.txt", "example_data/SRP_wt_Rep3_rho_table.txt"), data_file <- get_data_file()$datapath)
    ifelse(is.null(input$data_file), data_file <- "example_data/SRP_wt_Rep1_rho_table.txt", data_file <- get_data_file()$datapath)
    data_mat = load_data(data_file)
  })
  get_mean_data <- reactive({
    data_mat = get_data()
    data_mat = Reduce("+", data_mat) / length(data_mat)
  })
  get_ncol <- reactive({ncol(get_data()[[1]])})
  get_nrow <- reactive({nrow(get_data()[[1]])})
  
  # PIR values
  calc_P_values <- reactive({
    data_mat = get_data()
    window_size = get_window_size()
    if (window_size == 0) {
      P_values = lapply(data_mat, function(i) i * 0)
    } else {
      P_values = lapply(data_mat, function(i) diff_data(i, window_size))
    }
  })
  calc_I_values <- reactive({
    P_values = calc_P_values()
    num_row = get_nrow()
    num_col = get_ncol()
    I_values = lapply(1:length(P_values), function(i) matrix(NA, nrow = num_row, ncol = num_col))
    if (get_I_length() > num_row) {return(I_values)}
    
    for (n_file in 1:length(P_values)) {
      for (n_col in 1:num_col) {
        I_values[[n_file]][(get_I_length()+1):num_row,n_col] = sapply(1:(num_row-get_I_length()), function(i) sum(P_values[[n_file]][i:(i+get_I_length()),n_col])) / get_I_length()
      }
    }
    I_values
  })
  calc_R_values <- reactive({
    P_values = calc_P_values()
    data_mat = get_data()
    R_values = lapply(1:length(data_mat), function(i) P_values[[i]] / abs(data_mat[[i]] - P_values[[i]]))# weirdly elegant way to calculate it. the denominator reduces to the average window value
  })
  
  # linear ramp values
  calc_linear_values <- reactive({
    library(lmtest)
    ramp_length = get_ramp_length()
    data_mat = get_data()
    p_values = lapply(data_mat, function(i) i * NA)
    betas = p_values
    dws = p_values
    
    num_rows = nrow(data_mat[[1]])
    if (ramp_length > num_rows) { # error
      warning("Ramp window is higher than number of rows!")
      return(list(p_values = p_values, betas = betas, dws = dws))
    } else if (ramp_length <= 1) {
      return(list(p_values = p_values, betas = betas, dws = dws))
    }
    
    for (n_file in 1:length(data_mat)) {
      for (n_col in 1:ncol(p_values[[1]])) {
        for (n_window in 1:(num_rows - ramp_length - 1)) {
          x = 1:ramp_length
          y = data_mat[[n_file]][n_window:(n_window + ramp_length - 1), n_col]
          
          if (!any(is.na(y))) {
            lm_summary = summary(lm(y ~ x))
            
            p_values[[n_file]][[n_window + ramp_length - 1, n_col]] = lm_summary$coefficients[[2,4]]
            betas[[n_file]][[n_window + ramp_length - 1, n_col]] = lm_summary$coefficients[[2,1]]
            
            # check that y is not all the same (dwtest doesn't like that)
            if (length(unique(y)) == 1) {
              dws[[n_file]][[n_window + ramp_length - 1, n_col]] = 2
            } else {
              dws[[n_file]][[n_window + ramp_length - 1, n_col]] = dwtest(y ~ x)$statistic
            }
          }
        }
      }
    }
    # reformat linear values
    lapply(1:length(p_values), function(i) list(p_values = p_values[[i]], betas = betas[[i]], dws = dws[[i]]))
  })
  
  get_replicate_events <- reactive({
    P_values = calc_P_values()
    I_values = calc_I_values()
    R_values = calc_R_values()
    linear_values = calc_linear_values()
    data_mat = get_data()
    
    num_files = length(R_values)
    event_storage = vector("list", num_files)
    concurrent_events = vector("list", num_files)
    for (n_file in 1:num_files) {
      event_storage[[n_file]] = do.call(DUETT_events, list(P_values[[n_file]], I_values[[n_file]], R_values[[n_file]], linear_values[[n_file]], data_mat[[n_file]], get_window_size(), get_I_length(), get_ramp_length(), get_duration(), get_event_gap(), cutoffs = list(P = get_P(), I = get_I(), R = get_R(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dws = get_dws())))
      
      concurrent_events[[n_file]] = find_concurrent_events(event_storage[[n_file]][[1]], concurrent_distance = get_concurrent_distance(), comparison_point = "start", event_types = get_conc_event_types())
    }
    return_list = list(event_storage, concurrent_events)
  })
  
  get_events <- reactive({
    if (get_update() == 0) {
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({
        
        event_storage = get_replicate_events()[[1]]
        num_files = length(event_storage)
        
        if (num_files > 1) {
          # merge replicates
          event_storage = merge_replicates(event_storage, get_agreement())
        } else {
          event_storage = event_storage[[1]]
        }
        
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
  
  get_optimize_parameters <- reactive({
    list(window_size_optimize = input$window_size_optimize,
         P_start = input$P_start, P_end = input$P_end, P_interval = input$P_interval,
         I_start = input$I_start, I_end = input$I_end, I_interval = input$I_interval,
         R_start = input$R_start, R_end = input$R_end, R_interval = input$R_interval)
  })
  
  get_plotting_parameters <- reactive({
    
    # place modal for optimizing parameters
    observeEvent(input$optimize, {
      showModal(optimize_modal())
    })
    
    # call optimize_thresholds
    observeEvent(input$execute_optimize, {
      optimize_thresholds(get_data(), get_optimize_parameters(), input$output_optimize)
    })
    
    
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
      make_visual(get_mean_data(), event_locations, concurrent_events, get_plotting_parameters()$log_colors,
                  numbering = get_plotting_parameters()$numbering,
                  numbering_offset = get_plotting_parameters()$numbering_offset,
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize,
                  box_resize = get_plotting_parameters()$box_resize,
                  diverging = get_plotting_parameters()$diverging)})})
    
    # details
    counter = 1
    num_plots = 16
    columns_to_show = get_columns_to_show()
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
            cutoffs = list(P = get_P(), I = get_I(), R = get_R(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dws = get_dws()), 
            event_colors = c("red", "blue"),
            ylim = get_plotting_parameters()$ylim,
            xaxis_offset = get_numbering_offset(),
            xaxis_interval = get_numbering_interval())
        })
      })
    }
  })
  
  # interface to put plots to output
  output$plots <- renderUI({
    master_plotting()
    
    # check if update was called at least once
    if (identical(get_plotting_parameters(), NA)) {return()}
    
    # num_plots = ceiling(length(get_plotting_parameters()$columns_to_show) / 4)
    num_plots = 16
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
      
      # print replicates too
      if (get_print_replicate()) {
        return_list = get_replicate_events()
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
      return_list = get_events()
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
      if (get_print_replicate()) {
        event_storage = get_replicate_events()[[1]]
        
        for (n_file in 1:length(event_details)) {
          write_table(event_storage[[n_file]][[2]]$P_values, "P", n_file)
          write_table(event_storage[[n_file]][[2]]$I_values, "I", n_file)
          write_table(event_storage[[n_file]][[2]]$R_values, "R", n_file)
          write_table(event_storage[[n_file]][[2]]$p_values, "p_values", n_file)
          write_table(event_storage[[n_file]][[2]]$betas, "betas", n_file)
          write_table(event_storage[[n_file]][[2]]$dws, "dws", n_file)
        }
      }
      
      event_details = get_events()[[2]]
      
      write_table(event_details$P_values, "P", NULL)
      write_table(event_details$I_values, "I", NULL)
      write_table(event_details$R_values, "R", NULL)
      write_table(event_details$p_values, "p_values", NULL)
      write_table(event_details$betas, "betas", NULL)
      write_table(event_details$dws, "dws", NULL)
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
      
      width = get_plotting_parameters()$width
      height = get_plotting_parameters()$height
      
      # print replicates
      if (get_print_replicate()) {
        return_list = get_replicate_events()
        event_storage = return_list[[1]]
        concurrent_events = return_list[[2]]
        data_mat = get_data()
        
        for (n_file in 1:length(event_storage)) {
          filename = paste(input$outfile, "_", n_file, ".pdf", sep = "")
          pdf(filename, width = width, height = height)
          make_visual(data_mat[[n_file]], event_storage[[n_file]][[1]], 
                      concurrent_events[[n_file]], get_plotting_parameters()$log_colors,
                      numbering = get_plotting_parameters()$numbering, 
                      numbering_offset = get_plotting_parameters()$numbering_offset,
                      numbering_interval = get_plotting_parameters()$numbering_interval,
                      axis_label_resize = get_plotting_parameters()$axis_label_resize,
                      box_resize = get_plotting_parameters()$box_resize,
                      diverging = get_plotting_parameters()$diverging)
          dev.off()
        }
      }
      
      # print shared
      return_list = get_events()
      event_locations = return_list[[1]]
      event_details = return_list[[2]]
      concurrent_events = return_list[[3]]
      filename = paste(input$outfile, ".pdf", sep = "")
      
      pdf(filename, width = width, height = height)
      make_visual(get_mean_data(), event_locations, concurrent_events, get_plotting_parameters()$log_colors,
                  numbering = get_plotting_parameters()$numbering, 
                  numbering_offset = get_plotting_parameters()$numbering_offset,
                  numbering_interval = get_plotting_parameters()$numbering_interval,
                  axis_label_resize = get_plotting_parameters()$axis_label_resize,
                  box_resize = get_plotting_parameters()$box_resize,
                  diverging = get_plotting_parameters()$diverging)
      dev.off()
      
      ############# columns #############
      
      num_plots = 16
      
      # print replicates
      if (get_print_replicate()) {
        data_mat = get_data()
        return_list = get_replicate_events()
        event_storage = return_list[[1]]
        concurrent_events = return_list[[2]]
        for (n_file in 1:length(data_mat)) {
          filename = paste(input$outfile, "_columns_", n_file, ".pdf", sep = "")
          pdf(filename, width = width, height = height)
          
          counter = 1
          columns_to_show = get_plotting_parameters()$columns_to_show
          suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / num_plots), each = num_plots)))
          
          for (col_group in col_groups) {
            counter = counter + 1
            my_col_group <- col_group
            my_i <- counter
            plot_name = paste("plot", my_i, sep = "")
            make_col_detail_plots(
              my_col_group, data_mat[n_file], event_storage[[n_file]][[1]],  # be sure to pass data_mat as a list
              event_storage[[n_file]][[2]], concurrent_events[[n_file]],
              cutoffs = list(P = get_P(), I = get_I(), R = get_R(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dws = get_dws()), 
              event_colors = c("red", "blue"),
              ylim = get_plotting_parameters()$ylim,
              xaxis_offset = get_numbering_offset(),
              xaxis_interval = get_numbering_interval())
          }
          dev.off()
        }
      }
      
      # print shared
      filename = paste(input$outfile, "_columns.pdf", sep = "")
      return_list = get_events()
      event_locations = return_list[[1]]
      event_details = return_list[[2]]
      concurrent_events = return_list[[3]]
      pdf(filename, width = width, height = height)
      
      counter = 1
      columns_to_show = get_plotting_parameters()$columns_to_show
      suppressWarnings(col_groups <- split(columns_to_show, rep(1:ceiling(length(columns_to_show) / num_plots), each = num_plots)))
      
      for (col_group in col_groups) {
        counter = counter + 1
        my_col_group <- col_group
        my_i <- counter
        plot_name = paste("plot", my_i, sep = "")
        make_col_detail_plots(
          my_col_group, 
          get_data(), event_locations, event_details, concurrent_events,
          cutoffs = list(P = get_P(), I = get_I(), R = get_R(), p_value = get_p_value(), linear_coeff = get_linear_coeff(), dws = get_dws()), 
          event_colors = c("red", "blue"),
          ylim = get_plotting_parameters()$ylim,
          xaxis_offset = get_numbering_offset(),
          xaxis_interval = get_numbering_interval())
      }
      dev.off()
    })
  })
})