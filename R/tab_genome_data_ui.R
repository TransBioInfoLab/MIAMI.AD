tab_genome_data_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body dt-one-line",
    shiny::div(
      class = "buttons-group",
      shiny::downloadButton(
        ns("download_data"),
        "Download Tables",
        width = "150px"
      ),
      shiny::actionButton(
        inputId = 'link_eforge',
        label = "eFORGE",
        icon = icon("th"),
        width = "150px",
        onclick = "window.open('https://eforge.altiusinstitute.org/', '_blank')"
      )
    ),
    shiny::tags$p(
      "To perform gene ontology analysis, click on ",
      shiny::tags$em("Download Tables"),
      " to get the list of CpGs, and use those on ",
      shiny::tags$em("eFORGE")
    ),
    
    shiny::h4("Selected Datasets", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_data")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::div(
      id = "command_button",
      class = "top-margin-s flex",
      shiny::h4("Annotated CpGs", style = "margin-right: 10px;"),
      shiny::actionButton(ns("command_explore"), "Explore Top 10 CpGs")
    ),
    DT::DTOutput(ns("data_manhattan")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::br()
  )
}
