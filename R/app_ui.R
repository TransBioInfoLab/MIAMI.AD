create_ui_page <- function(raw_data) {
  df_labels <- raw_data$labels
  df_family <- raw_data$clocks
  df_tracks_read <- raw_data$track_list

  shiny::navbarPage(
    title = "MIAMI-AD",
    id = "navbar_id",
    position = "fixed-top",
    collapsible = TRUE,
    theme = bslib::bs_theme(bootswatch = "cerulean"),
    header = shiny::tags$head(
      shiny::tags$head(htmltools::includeCSS(
        "inst/shiny/www/miamiad_style.css"
        )),
      cicerone::use_cicerone()
    ),
    shiny::tabPanel(
      "About",
      value = "about_page_tab",
      tab_about_UI("about_tab")
    ),
    shiny::tabPanel(
      "Genome-wide Query",
      value = "genome_query_tab",
      tab_genome_UI("genome_tab", df_labels = df_labels)
    ),
    shiny::tabPanel(
      "Gene Query",
      value = "gene_query_tab",
      tab_gene_UI("gene_tab", df_labels = df_labels, df_tracks_read = df_tracks_read)
    ),
    shiny::tabPanel(
      "CpG Query",
      value = "cpg_query_tab",
      tab_cpg_UI("cpg_tab", df_labels = df_labels)
    ),
    shiny::tabPanel(
      "Epigenetic Clock Query",
      value = "epigenetic_query_tab",
      tab_epigenetic_UI("epigenetic_tab", df_labels = df_labels, df_family = df_family)
    ),
    shiny::tabPanel(
      "Download",
      value = "download_tab",
      tab_download_UI("download_tab", df_labels = df_labels, df_family = df_family)
    ),
    shiny::tabPanel(
      "Contact & Contribute",
      tab_contact_UI("contact_tab")
    )
  )
}
