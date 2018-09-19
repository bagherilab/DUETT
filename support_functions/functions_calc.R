# PIR values
P_values <- reactive({
  data_mat = get_val$data()
  window_size = get_val$window_size()
  if (window_size == 0) {
    P_values = lapply(data_mat, function(i) i * 0)
  } else {
    P_values = lapply(data_mat, function(i) diff_data(i, window_size))
  }
})

I_values <- reactive({
  P_values = calc$P_values()
  num_row = get_val$num_row()
  num_col = get_val$num_col()
  I_values = lapply(1:length(P_values), function(i) matrix(NA, nrow = num_row, ncol = num_col))
  I_length = get_val$I_length()
  if (I_length > num_row) {return(I_values)}
  
  for (n_file in 1:length(P_values)) {
    for (n_col in 1:num_col) {
      I_values[[n_file]][(I_length+1):num_row,n_col] = sapply(1:(num_row-I_length), function(i) sum(P_values[[n_file]][i:(i+I_length),n_col])) / I_length
    }
  }
  I_values
})

R_values <- reactive({
  P_values = calc$P_values()
  data_mat = get_val$data()
  R_values = lapply(1:length(data_mat), function(i) P_values[[i]] / abs(data_mat[[i]] - P_values[[i]]))# weirdly elegant way to calculate it. the denominator reduces to the average window value
})

# linear ramp values
linear_values <- reactive({
  library(lmtest)
  ramp_length = get_val$ramp_length()
  data_mat = get_val$data()
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


events <- reactive({
  if (get_val$update() == 0) {
    return_list = NA
  } else {
    # only output file when button is pressed (I don't get this logic)
    isolate({
      
      P_values = calc$P_values()
        I_values = calc$I_values()
        R_values = calc$R_values()
        linear_values = calc$linear_values()
        data_mat = get_val$data()
      
      event_storage = do.call(DUETT_events, list(P_values, I_values, R_values, linear_values, data_mat, get_val$window_size(), get_val$I_length(), get_val$ramp_length(), get_val$duration(), get_val$event_gap(), cutoffs = list(P = get_val$P(), I = get_val$I(), R = get_val$R(), p_value = get_val$p_value(), linear_coeff = get_val$linear_coeff(), dws = get_val$dws()), get_val$concurrent_distance(), conc_event_types = get_val$conc_event_types()))[[1]]
      
      # event_storage = event_storage[[1]]
      num_files = length(event_storage)
      
      if (num_files > 1) {
        # merge replicates
        event_storage = merge_replicates(event_storage, get_val$agreement())
      } else {
        event_storage = event_storage[[1]]
      }
      
      concurrent_events = find_concurrent_events(event_storage[[1]], concurrent_distance = get_val$concurrent_distance(), comparison_point = "start", event_types = get_val$conc_event_types())
      return_list = c(event_storage, list(concurrent_events))
    })
  }
})

columns_to_show <- reactive({
  columns_display = get_val$columns_display()
  if (columns_display == 1) {
    columns_to_show = get_val$custom_columns()
  } else if (columns_display == 2) {
    columns_to_show = 1:get_val$num_col()
  } else  if (columns_display == 3) {
    event_locations = calc$events()[[1]]
    columns_to_show = which(colSums((event_locations != 0) & !is.na(event_locations)) > 0)
  }
})

