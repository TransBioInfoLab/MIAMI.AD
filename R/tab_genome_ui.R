tab_genome_UI <- function(id, df_labels) {
  ns <- shiny::NS(id)

  shiny::tagList(
    primaryButton(ns("tour_select"), "Tour", class = "tour-btn btn-sm"),

    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Genome-wide Query"
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
            tab_genome_datasets_UI(ns("datasets"))
          ),
          shiny::tabPanel(
            title = "Display Data",
            value = ns("tab_data"),
            tab_genome_data_UI(ns("data"))
          ),

          shiny::tabPanel(
            title = "Display Plot",
            value = ns("tab_plot"),
            tab_genome_plot_UI(ns("plot"))
          )
        )
      )
    )
  )
}
