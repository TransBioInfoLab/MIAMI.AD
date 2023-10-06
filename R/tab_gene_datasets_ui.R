tab_gene_datasets_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::div(
      class = "buttons-group",
      lightButton(ns("command_fill"), "Select All", width = "150px"),
      lightButton(ns("command_clear"), "Deselect All", width = "150px")
    ),
    DT::DTOutput(ns("data_selection_targets")),
    shiny::br()
  )
}
