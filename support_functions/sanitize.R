sanitize <- function(value, function_call) {
  
  # check for specialty ones
  if (function_call == "ylim") {
    value = as.numeric(strsplit(value, ",")[[1]])
    if (!is.numeric(value)) {
      warning("ylim should be two numbers separated a comma (no space")
    }
    return(value)
  } else if (function_call == "custom_columns") {
    value = as.numeric(strsplit(value, ",")[[1]])
    if (!(is.numeric(value) & !any(!(value %% 1 == 0)))) {
      warning("Custom columns should be integers separated by commas (no spaces)")
    }
    return(value)
  } else if (function_call == "I_length") {
    options(warn = -1) # temporarily turn off warnings
    is_number = !is.na(as.numeric(value)) # check for number in string
    options(warn = 0) # turn warnings back on
    
    if (is_number) {
      if (!(as.numeric(value) %% 1 == 0)){
        warning("If I window length is specified as a number, it must be an integer.  Setting to default")
        value = "default"
      } else {
        value = as.numeric(value)
      }
    } else {
      value = "default"
    }
    return(value)
  }
  
  # check that it is a number and is length 1
  if (!(length(value) == 1 & is.numeric(value))) {
    stop(paste(function_call, "is supposed to be numeric and of length 1"))
  }
  
  # check for other types of sanitizations
  sanitize_types = NULL
  switch(function_call,
         numbering_offset = sanitize_types <- c("integer"),
         numbering_interval = sanitize_types <- c("integer"),
         axis_label_resize = sanitize_types <- c("positive"),
         box_resize = sanitize_types <- c("positive"),
         P = sanitize_types <- c("positive"),
         I = sanitize_types <- c("positive"),
         R = sanitize_types <- c("positive"),
         window_size = sanitize_types <- c("positive", "integer"),
         event_gap = sanitize_types <- c("positive", "integer"),
         duration = sanitize_types <- c("positive", "integer"),
         ramp_length = sanitize_types <- c("positive", "integer"),
         p_value = sanitize_types <- c("positive", "between 0 and 1"),
         linear_coeff = sanitize_types <- c("positive"),
         dws = sanitize_types <- c("positive"),#, "between 0 and 1"),
         concurrent_distance = sanitize_types <- c("positive")
  )
  
  if (is.null(sanitize_types)) {
    print(function_call)
    print(value)
    stop("You screwed up sanitization code Albert")
  }
  
  warn_flag = F
  if ("integer" %in% sanitize_types) {
    if (value %% 1 != 0) {
      warn_flag = T
    }
  }
  
  if ("positive" %in% sanitize_types) {
    if (value < 0) {
      # browser()
      warn_flag = T
    }
  }
  
  if ("between 0 and 1" %in% sanitize_types) {
    if ((value < 0) | (value > 1)) {
      warn_flag = T
    }
  }
  
  if (warn_flag) {
    print(paste(function_call, "is supposed to be", paste(sanitize_types, sep = " and ")))
    # warning(paste(function_call, "is supposed to be", paste(sanitize_types, sep = " and ")))
  }
  
  return(value)
}