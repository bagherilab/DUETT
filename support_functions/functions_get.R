
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

# file inputs and outputs
data_file <- reactive({input$data_file})
print_replicate <- reactive({input$print_replicate})
diverging <- reactive({input$diverging})
width <- reactive({input$width})
height <- reactive({input$height})

# plotting parameters
agreement <- reactive({input$agreement})
log_colors <- reactive({input$log_colors})
numbering <- reactive({input$numbering})
numbering_offset <- reactive({sanitize(input$numbering_offset, "numbering_offset")})
numbering_interval <- reactive({sanitize(input$numbering_interval, "numbering_interval")})
axis_label_resize <- reactive({sanitize(input$axis_label_resize, "axis_label_resize")})
box_resize <- reactive({sanitize(input$box_resize, "box_resize")})
ylim <- reactive({sanitize(input$ylim, "ylim")})
columns_display <- reactive({input$column_display})
custom_columns <- reactive({sanitize(input$custom_columns, "custom_columns")})

# PIR parameters
P <- reactive({sanitize(input$P, "P")})
I <- reactive({sanitize(input$I, "I")})
I_length <- reactive({
  I_length = sanitize(input$I_length, "I_length")
  if (I_length == "default") {I_length = window_size()}
  I_length
})
R <- reactive({sanitize(input$R, "R")})
# I_length <- reactive({sanitize(input$window_size, "window_size")}) # not actually an input
window_size <- reactive({sanitize(input$window_size, "window_size")})
event_gap <- reactive({sanitize(input$event_gap, "event_gap")})
duration <- reactive({sanitize(input$duration, "duration")})

# linear ramp paramers
ramp_length <- reactive({sanitize(input$ramp_length, "ramp_length")})
p_value <- reactive({sanitize(input$p_value, "p_value")})
linear_coeff <- reactive({sanitize(input$linear_coeff, "linear_coeff")})
dws <- reactive({sanitize(input$dws, "dws")})

concurrent_distance <- reactive({sanitize(input$concurrent_distance, "concurrent_distance")})
conc_event_types <- reactive({
  event_types = c()
  if (input$concurrent_swings) {event_types = c(-1, event_types, 1)}
  if (input$concurrent_ramps) {event_types = c(-2, event_types, 2)}
  event_types
})

update <- reactive({input$update})
optimize <- reactive({input$optimize})

data <- reactive({
  # ifelse(is.null(input$data_file), data_file <- list("example_data/SRP_wt_Rep1_rho_table.txt", "example_data/SRP_wt_Rep2_rho_table.txt", "example_data/SRP_wt_Rep3_rho_table.txt"), data_file <- get_data_file()$datapath)
  ifelse(is.null(input$data_file), data_file <- "example_data/SRP_wt_Rep1_rho_table.txt", data_file <- get_val$data_file())
  data_mat = load_data(data_file)
})
mean_data <- reactive({
  data_mat = get_val$data()
  data_mat = Reduce("+", data_mat) / length(data_mat)
})
num_col <- reactive({ncol(get_val$data()[[1]])})
num_row <- reactive({nrow(get_val$data()[[1]])})

optimize_parameters <- reactive({
  list(window_size_optimize = input$window_size_optimize,
       P_start = input$P_start, P_end = input$P_end, P_interval = input$P_interval,
       I_start = input$I_start, I_end = input$I_end, I_interval = input$I_interval,
       R_start = input$R_start, R_end = input$R_end, R_interval = input$R_interval)
})

plotting_parameters <- reactive({
  
  # place modal for optimizing parameters
  observeEvent(input$optimize, {
    showModal(optimize_modal())
  })
  
  # call optimize_thresholds
  observeEvent(input$execute_optimize, {
    optimize_thresholds(get_val$data(), get_val$optimize_parameters(), input$output_optimize)
  })
  
  
  if (get_val$update() == 0) {
    return_list = NA
  } else {
    # only output file when button is pressed (I don't get this logic)
    isolate({
      list(log_colors = get_val$log_colors(),
           numbering = get_val$numbering(),
           numbering_offset = get_val$numbering_offset(),
           numbering_interval = get_val$numbering_interval(),
           axis_label_resize = get_val$axis_label_resize(),
           box_resize = get_val$box_resize(),
           columns_to_show = calc$columns_to_show(),
           ylim = get_val$ylim(),
           height = get_val$height(),
           width = get_val$width(),
           diverging = get_val$diverging())
    })
  }
})


