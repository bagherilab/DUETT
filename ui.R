rm(list=ls())

if(!("shiny" %in% installed.packages())) install.packages("shiny")
if(!("shinyalert" %in% installed.packages())) install.packages("shinyalert")

library(shiny)
library(shinyalert)

shinyUI(
  bootstrapPage(
    titlePanel("DUETT"),
    
    fluidRow(
      column(3, 
             # sidebarLayout(
             
             # load data
             wellPanel(
               titlePanel("File inputs and outputs"),
               fileInput("data_file", "Input data file", multiple = T),
               checkboxInput('print_replicate', "Print replicate info?", FALSE),
               textInput("outfile", "Output file name", value = "example_output", placeholder = "example_output"),
               actionButton("table_output", label = "Print event file"),
               actionButton("table_UI_settings", label = "Print UI settings"),
               actionButton("table_details_output", label = "Print details files"),
               br(),
               br(),
               actionButton("graph_output", label = "Make figure pdf file"),
               
               sliderInput("width", label = "Figure width", min=3, max=20, value=14, ticks=F),
               sliderInput("height", label = "Figure height", min=3, max=20, value=14, ticks=F)
             )
      ),
      
      column(3, 
             # plotting parameters
             wellPanel(
               titlePanel("Plotting"),
               
               checkboxInput('diverging', "Diverging data?", FALSE),
               checkboxInput('log_colors', 'Log the colors?', FALSE),
               
               checkboxInput('numbering', 'Number rows/columns?', TRUE),
               conditionalPanel(
                 condition = "input.numbering == true",
                 numericInput("numbering_interval", label = "Numbering interval", value = 5, min = 1, step = 1),
                 numericInput("numbering_offset", label = "Numbering offset", value = 20, step = 1),
                 numericInput("axis_label_resize", label = "Resize axis labels", value = 1, min = 0, step = 0.1),
                 numericInput("box_resize", label = "Resize boxes", value = 1, min = 0, step = 0.05)
               ),
               titlePanel(h4("Column detail plotting")),
               textInput("ylim", label = "y axis range", value = "0,20", placeholder = "0,20"),
               selectInput("column_display", label = "Columns to display?", choices = list("Custom columns" = 1, "All columns" = 2, "Columns with events" = 3), selected = 2),
               conditionalPanel(
                 condition = "input.column_display == 1",
                 textInput("custom_columns", label = "Columns to display", value = "1,11,16,26,57", placeholder = "1,11,14,37"))
             )
      ),
      
      column(3, 
             wellPanel(
               titlePanel("PID"),
               
               numericInput("agreement", label = "Replicate agreement", value = 3, min = 1, step = 1),
               numericInput("P", label = "Absolute (P)", value = 0.2, min = 0, step = 0.1),
               numericInput("I", label = "Integral (I)", value = 0.25, min = 0, step = 0.1),
               textInput("I_length", label = "I length", value = "default"),
               numericInput("R", label = "Relative (R)", value = 0.5, min = 0, step = 0.1),
               numericInput("window_size", label = "Window size", value = 9, min = 1, step = 1),
               
               titlePanel(h4("Noise parameters")),
               numericInput("duration", label = "Event duration", value = 4, min = 0, step = 1),
               numericInput("event_gap", label = "Event gap", value = 1, min = 0, step = 1)
             )),
      
      column(3, 
             wellPanel(
               titlePanel("Linear ramp"),
               numericInput("ramp_length", label = "Ramp length", value = 30, min = 0, step = 1),
               numericInput("p_value", label = "Ramp p-value", value = 0.0001, min = 0, max = 1, step = 0.0001),
               numericInput("linear_coeff", label = "Linear coefficient", value = 0.15, min = 0, step = 0.01),
               numericInput("dws", label = "Durbin-Watson statistic", value = 1.25, min = 0, max = 4, step = 0.1),
               
               br(),
               numericInput("concurrent_distance", label = "Concurrency distance", value = 2, min = -1, step = 1),
               tags$head(tags$style(HTML('#update{background-color:lightgreen}'))),
               checkboxInput('concurrent_ramps', 'Ramp events?', FALSE),
               checkboxInput('concurrent_swings', 'Swing events?', TRUE),
               br(),
               actionButton("update", "Update plot"),
               br(),
               useShinyalert(),
               br(),
               actionButton("optimize", "Optimize thresholds")
             )
      )),
    
    mainPanel(
      # This is the dynamic UI for the plots
      uiOutput("plots"),
      br(),
      br()
    )
  )
)

