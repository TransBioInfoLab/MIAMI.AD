tab_download_dmrs_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
    DT::DTOutput(ns("data_dmrs")),
    shiny::br()
  )
}
