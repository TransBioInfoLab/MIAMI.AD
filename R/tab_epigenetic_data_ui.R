tab_epigenetic_data_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "top-margin-m tab-body",
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
    

    shiny::h4("Selected Epigenetic Clocks", class = "top-margin-m"),
    DT::DTOutput(ns("data_selection_data")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("Variable Definitions", class = "top-margin-m"),
    DT::DTOutput(ns("data_definitions")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::div(
      class = "top-margin-s flex",
      shiny::h4(
        "Minimum number of epigenetics clocks that include the CpG",
        style = "margin-right: 10px;"),
      shiny::actionButton(ns("command_explore"), "Explore Top 10 CpGs")
    ),

    shiny::sliderInput(
      inputId = ns("select_count"),
      label = shiny::tags$b("Minimum CpG Count"),
      value = 1,
      min = 1,
      max = 3,
      step = 1,
      width = "200px"),

    DT::DTOutput(ns("data_cpgs")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::br()
  )
}
