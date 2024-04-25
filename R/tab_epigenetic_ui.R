tab_epigenetic_UI <- function(id, df_labels, df_family){
  ns <- shiny::NS(id)

  shiny::tagList(
    primaryButton(ns("tour_select"), "Start Tutorial", class = "tour-btn btn-sm"),

    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Epigenetic Clock Query"
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        id = ns("side_panel"),
        width = 3,
        create_genome_version_input(ns, tour = TRUE),
        shiny::br(),
        shiny::div(
          id = ns("select_criteria_tour"),

          shiny::radioButtons(
            inputId = ns("cpg_direction"),
            label = shiny::h2(class = "input-header", "CpG Effect Direction Requirement"),
            inline = FALSE,
            choices = c("Require uniform direction" = "Uniform",
                        "Allow opposite direction" = "Both"),
            selected = "Uniform")
        ),
        shiny::br()
      ),

      shiny::mainPanel(
        id = ns("main_panel"),
        width = 9,
        shiny::tabsetPanel(
          id = ns("main_tabs"),
          shiny::tabPanel(
            title = "Datasets",
            value = ns("tab_datasets"),
            tab_epigenetic_datasets_UI(ns("datasets"))
          ),

          shiny::tabPanel(
            title = "Display Data",
            value = ns("tab_data"),
            tab_epigenetic_data_UI(ns("data"))
          ),

          shiny::tabPanel(
            title = "Display Plot",
            value = ns("tab_plot"),
            tab_epigenetic_plot_UI(ns("plot"))
          )
        )
      )
    )
  )
}
