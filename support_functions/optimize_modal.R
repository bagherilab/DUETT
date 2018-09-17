optimize_modal <- function() {
  modalDialog(
    footer = tagList(
      numericInput("window_size_optimize", label = "Window size", value = 9, min = 0, step = 1),
      textInput("output_optimize", label = "Output file", value = "optimized_thresholds"),
      hr(),
<<<<<<< HEAD
      span("Optimize PIR parameters? (This will take some time)  "),
=======
      span("Optimize PID parameters? (This will take some time)  "),
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
      modalButton("Cancel"),
      actionButton("execute_optimize", "Yes")),
    fluidRow(
      column(4, wellPanel(
<<<<<<< HEAD
        numericInput("P_start", label = "P start", value = 0.05, min = 0, step = 0.05),
        numericInput("P_end", label = "P end", value = 1, min = 0, step = 0.05),
        numericInput("P_interval", label = "P interval", value = 0.05, min = 0, step = 0.05)
=======
        numericInput("P_start", label = "P start", value = 0.025, min = 0, step = 0.05),
        numericInput("P_end", label = "P end", value = 0.5, min = 0, step = 0.025),
        numericInput("P_interval", label = "P interval", value = 0.025, min = 0, step = 0.025)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
      )),
      column(4, wellPanel(
        numericInput("I_start", label = "I start", value = 0.025, min = 0, step = 0.05),
        numericInput("I_end", label = "I end", value = 0.5, min = 0, step = 0.05),
        numericInput("I_interval", label = "I interval", value = 0.025, min = 0, step = 0.025)
      )),
      column(4, wellPanel(
<<<<<<< HEAD
        numericInput("R_start", label = "R start", value = 0.025, min = 0, step = 0.05),
        numericInput("R_end", label = "R end", value = 0.5, min = 0, step = 0.025),
        numericInput("R_interval", label = "R interval", value = 0.025, min = 0, step = 0.025)
=======
        numericInput("D_start", label = "D start", value = 0.05, min = 0, step = 0.05),
        numericInput("D_end", label = "D end", value = 1, min = 0, step = 0.05),
        numericInput("D_interval", label = "D interval", value = 0.05, min = 0, step = 0.05)
>>>>>>> 051550ee474a4d0d7c336a56572e8f69175d09a0
      ))
    )
  )
}