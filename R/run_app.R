#' Run the MIAMI-AD app
#' @export
run_app <- function() {
  ## app.R ##
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("shiny", "www", package = "MIAMI-AD")
  )
  shiny::addResourcePath(
    prefix = "Plots",
    directoryPath = system.file("shiny", "Plots", package = "MIAMI-AD")
    )

  # run app
  # raw_data <- read_in_data()
  #
  # ui <- create_ui_page(raw_data)
  # server <- create_server_page(raw_data)
  #
  # shiny::shinyApp(ui, server)
}
