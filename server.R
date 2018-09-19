# server.R

rm(list=ls())

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  # create environment specifically for get functions
  get_val <- new.env()
  # load the get functions
  sys.source("support_functions/functions_get.R", envir = get_val)
  
  #################### calculate for events ####################
  
  calc <- new.env()
  sys.source("support_functions/functions_calc.R", envir = calc)
  
  #################### plotting ####################
  
  # master plot function
  master <- new.env()
  sys.source("support_functions/plotting/master_plotting.R", envir = master)
  
  # interface to put plots to output
  output$plots <- renderUI({
    master$master_plotting()
    
    # check if update was called at least once
    if (identical(get_val$plotting_parameters(), NA)) {return()}
    
    # num_plots = ceiling(length(get_val$plotting_parameters()$columns_to_show) / 4)
    num_plots = 16
    plot_output_list <- lapply(1:(num_plots+1), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 72 * get_val$plotting_parameters()$height, width = 72 * get_val$plotting_parameters()$height)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  #################### outputs ####################
  
  make_output <- new.env()
  sys.source("support_functions/functions_output.R", envir = make_output)
  
  
})