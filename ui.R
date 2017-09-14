rm(list=ls())

if(!("shiny" %in% installed.packages())) install.packages("shiny")

library(shiny)

shinyUI(
  fluidPage(
    titlePanel("ShapeSeq event detector"),
    
    fluidRow(
      column(3, 
             # sidebarLayout(
             
             # load data
             wellPanel(
               titlePanel("File inputs and outputs"),
               fileInput("data_file", "Input data file"),
               textInput("outfile", "Output file name", value = "example_output", placeholder = "example_output"),
               actionButton("table_output", label = "Print event file"),
               actionButton("table_details_output", label = "Print details files"),
               br(),
               br(),
               actionButton("graph_output", label = "Make figure pdf file")
             )
      ),
      
      column(3, 
             # plotting parameters
             wellPanel(
               titlePanel("Plotting parameters"),
               
               sliderInput("width", label = "Figure width", min=3, max=20, value=14, ticks=F),
               sliderInput("height", label = "Figure height", min=3, max=20, value=14, ticks=F),
               
               checkboxInput('numbering', 'Number rows/columns?', TRUE),
               conditionalPanel(
                 condition = "input.numbering == true",
                 numericInput("numbering_interval", label = "Numbering interval", value = 5, min = 1, step = 1)
               ),
               titlePanel(h4("Column detail plotting")),
               textInput("ylim", label = "y axis range", value = "0,20", placeholder = "0,20"),
               selectInput("column_display", label = "Columns to display?", choices = list("Custom columns" = 1, "All columns" = 2, "Columns with events" = 3), selected = 1),
               conditionalPanel(
                 condition = "input.column_display == 1",
                 textInput("custom_columns", label = "Columns to display", value = "1,11,16,26,57", placeholder = "1,11,14,37"))
             )
      ),
      
      column(3, 
             wellPanel(
               titlePanel("PID parameters"),
               
               numericInput("P", label = "Proportional (P)", value = 0.25, min = 0, step = 0.1),
               numericInput("I", label = "Integral (I)", value = 0.8, min = 0, step = 0.1),
               numericInput("D", label = "Differential (D)", value = 0.8, min = 0, step = 0.1),
               numericInput("window_size", label = "Window size", value = 9, min = 1, step = 1),
               
               titlePanel(h4("Noise parameters")),
               numericInput("event_gap", label = "Event gap", value = 1, min = 0, step = 1),
               numericInput("noise_length", label = "Noise length", value = 2, min = 0, step = 1)
             )),
      
      column(3, 
             wellPanel(
               titlePanel("Linear ramp parameters"),
               numericInput("ramp_length", label = "Ramp length", value = 40, min = 5, step = 1),
               numericInput("p_value", label = "Ramp p-value", value = 0.0001, min = 0, max = 1, step = 0.0001),
               numericInput("b_min", label = "Beta coefficient", value = 0.15, min = 0, step = 0.01),
               numericInput("dwp", label = "Durbin-Watson p-value", value = 0.01, min = 0, max = 1, step = 0.01),
               
               br(),
               numericInput("concurrent_distance", label = "Concurrency distance", value = 2, min = -1, step = 1),
               
               br(),
               tags$head(tags$style(HTML('#update{background-color:lightgreen}'))),
               actionButton("update", "Update plot")
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
# ))