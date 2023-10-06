tab_gene_UI <- function(id, df_labels, df_tracks_read) {
  ns <- shiny::NS(id)

  shiny::tagList(
    primaryButton(ns("tour_select"), "Tour", class = "tour-btn btn-sm"),

    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Gene Query"
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
          id = ns("genomic_position_tour"),
          shiny::h2(class = "input-header", "Genomic Position")
        ),
          shiny::radioButtons(
            inputId = ns("input_type"),
            label = shiny::tags$b("Input Method"),
            inline = FALSE,
            choices = c("Input a gene name" = "gene",
                        "Input a genomic region" = "range"
            )
        ),
        create_gene_conditional_input(ns, id),
        shiny::br(),
          shinyWidgets::pickerInput(
            inputId = ns('select_track'),
            label = shiny::h2(class = "input-header", "Genome Tracks"),
            choices = unique(df_tracks_read$Name),
            multiple = TRUE,
            selected = NULL,
            width = "200px")
      ),

      shiny::mainPanel(
        id = ns("main_panel"),
        width = 9,
        shiny::tabsetPanel(
          id = ns("main_tabs"),
          shiny::tabPanel(
            title = "Datasets",
            value = ns("tab_datasets"),
            tab_gene_datasets_UI(ns("datasets"))
          ),
          shiny::tabPanel(
            title = "Display Data",
            value = ns("tab_data"),
            tab_gene_data_UI(ns("data"))
          ),

          shiny::tabPanel(
            title = "Display Plot",
            value = ns("tab_plot"),
            tab_gene_plot_UI(ns("plot"))
          )
        )
      )
    )
  )
}
