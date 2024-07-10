tab_genome_datasets_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::div(
      class = "buttons-group",
      lightButton(ns("command_fill"), "Select All", width = "150px"),
      lightButton(ns("command_clear"), "Deselect All", width = "150px")
    ),
    shiny::div(
      class = "selection-error",
      shiny::textOutput(ns("select_error"))
    ),
    shiny::p(shiny::tags$b("To select a different set of CpGs")),
    shiny::p(
      "- to change significance level: double click on the numbers in the",
      shiny::tags$b("Threshold"),
      " column",
    ),
    DT::DTOutput(ns("data_selection_targets")),
    shiny::br()
  )
}
