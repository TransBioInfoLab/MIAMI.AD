tab_about_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      align = "center",
      class = "long-logo",
      shiny::imageOutput(ns("miamiad_logo"), height = "100%", width = "80%")
    ),
    shiny::fluidRow(shiny::column(
      width = 10, offset = 1,
      shiny::h3(shiny::tags$b("1. Introduction")),
      shiny::tags$p("We collated results from recent epigenome-wide ",
      "association studies in aging and AD and developed ",
      "MIAMI-AD, an integrative database of blood DNAm ",
      "across sex, aging, and AD. Researchers can search ",
      "for CpGs, regions, or genes of their interests to",
      " benefit from recent scientific advances. In the ",
      "following, we use ", shiny::em("studies"), " to refer to ",
      "research publications and ", shiny::em("datasets"), " to ",
      "refer to the resulting summary statistics from ",
      "the studies. "),
      shiny::tags$p("Several queries are provided: "),
      shiny::tags$p(
        shiny::tags$b("(1) Genome-wide Query"), " enables selection",
        "of CpGs based on significance threshold or ",
        "location; it provides:"
      ),
      shiny::div(
        shiny::tags$ul(
          shiny::tags$li(
            "Manhattan plot of a the entire genome for the chosen study"),
          shiny::tags$li("Selected CpGs (at user-specified significance",
                         " threshold or location) in the chosen study")
        )
      ),
      shiny::tags$p(
        shiny::tags$b("(2) Gene Query"), " provides an ",
        shiny::tags$b("overview of CpGs associated with a specific",
               " gene or region"), " including:"
      ),
      shiny::div(
        shiny::tags$ul(
          shiny::tags$li("Manhattan plot of CpGs located within genes"),
          shiny::tags$li("Annotations (UCSC Genes, ENSEMBL genes, and ",
             "transcripts, chromatin states, CpG islands)"),
          shiny::tags$li("Download summary statistics (odds ratio, effect ",
             "estimate, pValues) of CpGs and DMRs in recent AD and aging studies. ")
        )
      ),
      shiny::tags$p(
        shiny::tags$b("(3) CpG Query"), " provides ",
        shiny::tags$b("detailed information on specific CpGs"),
        " including:"
      ),
      shiny::div(
        shiny::tags$ul(
          shiny::tags$li("Forest plots from a meta-analysis"),
          shiny::tags$li("Annotations (relation to CpG island, UCSC reference gene group, mQTL information)"),
          shiny::tags$li("Meta-analysis results (odds ratio, direction in ",
             "individual datasets, confidence interval)"),
          shiny::tags$li("Individual study results (odds ratio, pValue)"),
          shiny::tags$li("Brain-blood correlations")
        )
      ),
      shiny::tags$p(
        shiny::tags$b("(4) Epigenetic Clocks Query"), " enables")
      ,
      shiny::div(
        shiny::tags$ul(
          shiny::tags$li("Selection of CpGs in multiple epigenetic clocks"),
          shiny::tags$li("Determination of shared and unique CpGs ",
                         "in multiple epigenetic clocks ")
        )
      ),
      shiny::h3(shiny::tags$b("2. Citation")),
      shiny::tags$p("If you find MIAMI-AD helpful, please cite our paper: "),
      shiny::tags$p("Lukacsovich D et al. (2024) MIAMI-AD (Methylation ",
        "in Aging and Methylation in AD): an integrative knowledgebase that ",
        "facilitates explorations of DNA methylation across sex, aging, and ",
        "Alzheimer's disease. ",
        shiny::tags$a(
          href = "https://pubmed.ncbi.nlm.nih.gov/39028752/",
          shiny::em("Oxford Academic"),
          target = "_blank"
        )
        ),
      shiny::h3(shiny::tags$b("3. Included Studies")),
      shiny::tags$p("MIAMI-AD included studies that met two main ",
        "criteria: (1) having more than 100 total subjects and (2) conducting",
        " a genome-wide study of more than 100k CpGs. For each study, we ",
        "included as many CpGs as possible, either CpGs that passed quality ",
        "control or CpGs listed in supplementary tables."),
      shiny::tags$p(
        "Please see",
        shiny::actionLink(ns("switch.tab"), "HERE"),
        "for details on the studies included in MIAMI-AD."
      ),
      shiny::h3(shiny::tags$b("4. Terms of Use")),
      shiny::tags$p("If you use MIAMI-AD, you agree to:"),
      shiny::div(
        shiny::tags$ul(
          shiny::tags$li("You will cite our paper above in any publication ",
                  "where you have used results from MIAMI-AD."),
          shiny::tags$li("You will also cite the original study papers in ",
                  "which the results were obtained.")
        ),
      ),
      shiny::h3(shiny::tags$b("5. Acknowledgements")),
      shiny::tags$p("This project is supported by funding from NIH ",
        "grants RF1AG061127, RF1NS128145, and R01AG062634."),
      shiny::br()
    ))
  )
}
