tab_download_UI <- function(id, df_labels, df_family) {
  ns <- shiny::NS(id)

  shiny::tagList(
    primaryButton(ns("tour_select"), "Start Tutorial", class = "tour-btn btn-sm"),

    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Download Data"
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        id = ns("side_panel"),
        width = 3,
        create_genome_version_input(ns, tour = TRUE),
        shiny::br(),
        create_phenotype_input(df_labels$Phenotype, ns, tour = TRUE)
      ),

      shiny::mainPanel(
        id = ns("main_panel"),
        width = 9,
        shiny::tabsetPanel(
          id = ns("main_tabs"),
          shiny::tabPanel(
            title = "Datasets",
            value = ns("tab_datasets"),
            tab_download_datasets_UI(ns("datasets"))
          ),

          shiny::tabPanel(
            title = "Epigenetic Clocks",
            value = ns("tab_epigenetic"),
            tab_download_epigenetic_UI(ns("epigenetic"))
          ),

          shiny::tabPanel(
            title = "DMRs",
            value = ns("tab_dmr"),
            tab_download_dmrs_UI(ns("dmr"))
          )
        )
      )
    )
  )
}
