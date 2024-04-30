tab_download_dmrs_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    shiny::div(
      class = "buttons-group",
      lightButton(ns("command_fill"), "Select All", width = "150px"),
      lightButton(ns("command_clear"), "Deselect All", width = "150px")
    ),
    shiny::div(
      class = "buttons-group",
      shiny::downloadButton(ns("download_data"), "Download Tables", width = "150px")
    ),
    DT::DTOutput(ns("data_dmrs")),
    shiny::br()
  )
}
