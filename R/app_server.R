create_server_page <- function(raw_data) {
  server <- function(input, output, session) {

    # Define General Variables
    common <- list(
      genome_version = shiny::reactiveVal("hg19"),
      cpg_text = shiny::reactiveVal(),

      raw_data = raw_data,

      update_tab = function(tab_name) {
        shiny::updateNavbarPage(session, "navbar_id", tab_name)
      }
    )

    # Call Tab Server Functions
    tab_about_server("about_tab", common = common)
    tab_tutorial_server("tutorial_tab", common = common)
    tab_genome_server("genome_tab", common = common)
    tab_gene_server("gene_tab", common = common)
    tab_cpg_server("cpg_tab", common = common)
    tab_epigenetic_server("epigenetic_tab", common = common)
    tab_download_server("download_tab", common = common)

    # Call Tab Tour Functions
    tab_gene_Tour("gene_tab")
    tab_cpg_Tour("cpg_tab")
    tab_genome_Tour("genome_tab")
    tab_epigenetic_Tour("epigenetic_tab")
    tab_download_Tour("download_tab")
  }

  return (server)
}
