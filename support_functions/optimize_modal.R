optimize_modal <- function() {
  modalDialog(
    footer = tagList(
      numericInput("window_size_optimize", label = "Window size", value = 9, min = 0, step = 1),
      textInput("output_optimize", label = "Output file", value = "optimized_thresholds"),
      hr(),
      span("Optimize PIR parameters? (This will take some time)  "),
      modalButton("Cancel"),
      tags$head(tags$style(HTML('#execute_optimize{background-color:lightgreen}'))),
      actionButton("execute_optimize", "Yes")),
    fluidRow(
      column(4, wellPanel(
        numericInput("P_start", label = "P start", value = 0.05, min = 0, step = 0.05),
        numericInput("P_end", label = "P end", value = 1, min = 0, step = 0.05),
        numericInput("P_interval", label = "P interval", value = 0.05, min = 0, step = 0.05)
      )),
      column(4, wellPanel(
        numericInput("I_start", label = "I start", value = 0.025, min = 0, step = 0.05),
        numericInput("I_end", label = "I end", value = 0.5, min = 0, step = 0.05),
        numericInput("I_interval", label = "I interval", value = 0.025, min = 0, step = 0.025)
      )),
      column(4, wellPanel(
        numericInput("R_start", label = "R start", value = 0.025, min = 0, step = 0.05),
        numericInput("R_end", label = "R end", value = 0.5, min = 0, step = 0.025),
        numericInput("R_interval", label = "R interval", value = 0.025, min = 0, step = 0.025)
      ))
    )
  )
}