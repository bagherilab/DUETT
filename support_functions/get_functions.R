# all of the get functions

get_width <- reactive({width = input$width})
get_height <- reactive({height = input$height})

get_ylim <- reactive({as.numeric(strsplit(input$ylim, ",")[[1]])})
get_numbering <- reactive({input$numbering})
get_numbering_interval <- reactive({input$numbering_interval})
get_show_columns <- reactive({show_columns = as.numeric(strsplit(input$show_columns, ",")[[1]])})
get_show_all <- reactive({input$show_all_columns})
get_num_plots <- reactive({
  ifelse(get_show_all(), num_plots <- get_ncol(), num_plots <- length(get_show_columns()))
  num_plots = ceiling(num_plots / 4)
})
get_D <- reactive({input$D})
get_I <- reactive({input$I})
get_P <- reactive({input$P})
get_ramp_window <- reactive({input$ramp_length})
get_p_value <- reactive({input$p_value})
get_b_min <- reactive({input$b_min})
get_dwp <- reactive({input$dwp})
get_window_size <- reactive({input$window_size})
get_I_length <- reactive({input$window_size})
get_noise_length <- reactive({input$noise_length})
get_event_gap <- reactive({input$event_gap})

get_ncol <- reactive({ncol(get_data())})
get_data_file <- reactive({input$data_file})
get_data <- reactive({
  ifelse(is.null(input$data_file), data_file <- "example_data.csv", data_file <- get_data_file()$name)
  data_mat = load_data(data_file)
})

get_update <- reactive({input$update})

###
get_D_values <- reactive({
  data_mat = get_data()
  D_values = matrix(NA, nrow = nrow(data_mat), ncol = ncol(data_mat))
  for (n_col in 1:ncol(data_mat)) {
    D_values[,n_col] = diff_data(data_mat[,n_col], get_window_size())
  }
  D_values
})
get_I_values <- reactive({
  D_values = get_D_values()
  num_row = nrow(D_values)
  num_col = ncol(D_values)
  I_values = matrix(NA, nrow = num_row, ncol = num_col)
  for (n_col in 1:ncol(D_values)) {
    I_values[(get_I_length()+1):num_row,n_col] = sapply(1:(num_row-get_I_length()), function(i) sum(D_values[i:(i+get_I_length()),n_col])) / get_I_length()
  }
  I_values
})
get_P_values <- reactive({
  D_values = get_D_values()
  P_values = D_values / (get_data() - D_values)
  P_values
})
get_linear_values <- reactive({
  library(lmtest)
  data_mat = get_data()
  p_values = data_mat * NA
  betas = p_values
  dwp = p_values
  ramp_window = get_ramp_window()
  for (n_col in 1:ncol(p_values)) {
    for (n_window in 1:(nrow(p_values) - ramp_window - 1)) {
      x = 1:ramp_window
      y = data_mat[n_window:(n_window + ramp_window - 1), n_col]
      
      if (!any(is.na(y))) {
        lm_summary = summary(lm(y ~ x))
        
        p_values[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,4]]
        betas[[n_window + ramp_window - 1, n_col]] = lm_summary$coefficients[[2,1]]
        dwp[[n_window + ramp_window - 1, n_col]] = dwtest(y ~ x)$p.value
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
      
      D_values = get_D_values()
      I_values = get_I_values()
      P_values = get_P_values()
      linear_values = get_linear_values()
      return_list = do.call(ShapeSeq_events, list(P_values, I_values, D_values, linear_values, get_data(), get_window_size(), get_I_length(), get_ramp_window(), get_noise_length(), get_event_gap(), cutoffs = list(P = get_P(), I = get_I(), D = get_D(), p_value = get_p_value(), b_min = get_b_min(), dwp = get_dwp())))
    })
  }
})