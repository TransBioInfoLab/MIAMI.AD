tab_cpg_UI <- function(id, df_labels) {
  ns <- shiny::NS(id)

  shiny::tagList(
    primaryButton(ns("tour_select"), "Start Tutorial", class = "tour-btn btn-sm"),

    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "CpG Query"
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        id = ns("side_panel"),
        width = 3,
        create_genome_version_input(ns, tour = TRUE),
        shiny::br(),
        create_phenotype_input(df_labels$Phenotype, ns, tour = TRUE),
        shiny::br(),
        shiny::div(
          id = ns("select_cpg_tour"),
          shiny::h2(class = "input-header", "CpGs")
        ),
          shiny::radioButtons(inputId = ns("input_type"),
                       label = shiny::tags$b("Input Method"),
                       inline = FALSE,
                       choices = c("Provide a list of CpGs" = "manual",
                                   "Upload a file" = "file")
        ),
        create_cpg_conditional_input(ns, id),
      ),

      shiny::mainPanel(
        id = ns("main_panel"),
        width = 9,
        shiny::tabsetPanel(
          id = ns("main_tabs"),
          shiny::tabPanel(
            title = "Datasets",
            value = ns("tab_datasets"),
            tab_cpg_datasets_UI(ns("datasets"))
          ),
          shiny::tabPanel(
            title = "Display Data",
            value = ns("tab_data"),
            tab_cpg_data_UI(ns("data"))
          ),

          shiny::tabPanel(
            title = "Display Plot",
            value = ns("tab_plot"),
            tab_cpg_plot_UI(ns("plot"))
          )
        )
      )
    )
  )
}
