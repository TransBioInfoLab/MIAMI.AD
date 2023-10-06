tab_about_server <- function(id, common) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$switch.tab, {
      common$update_tab("download_tab")
    })

    output$miamiad_logo <- shiny::renderImage({
      list(src = "inst/shiny/www/logo.jpg",
           alt = "Missing Logo"
      )
    },
    deleteFile = FALSE)
  })
}
