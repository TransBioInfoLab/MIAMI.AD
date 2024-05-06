tab_contact_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Contact & Contribute"
    ),
    
    shiny::h3(shiny::tags$b("Contact")),
    shiny::p(
      "For questions, comments, or to contribute to",
      " MIAMI-AD, you can contact us at:"),
    shiny::tags$p(
      "Lily Wang (",
      shiny::tags$a(
        href="mailto:lily.wang@miami.edu",
        "lily.wang@miami.edu"),
      ") or ",
      "David Lukacsovich (",
      shiny::tags$a(
        href="mailto:david.lukacsovich@miami.edu",
        "david.lukacsovich@miami.edu"),
      ")"
    ),
    shiny::br(),

    shiny::h3(shiny::tags$b("Contribute")),
    shiny::tags$p(
      "If you would like to add your publication's data to MIAMI-AD, you can",
      " contact us at the above accounts, and send us your data. We need the",
      " following information to be able to add your data to MIAMI-AD. As the",
      " accompanying data would be fairly large in many cases, instead of",
      " emailing the data to us, please upload it to a publicly accessible",
      " online storage site (ex: dropbox, google drive, or box), and send us",
      " a download link."
    ),

    shiny::h4("Paper Metadata", class = "top-margin-m"),
    shiny::tags$p(
      "In order to add your data to our database, we need the following",
      " information about your paper and the dataset"
    ),
    DT::DTOutput(ns("example_meta_legend")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::tags$p(
      "If your data contains multiple, distinctly analysed datasets (ex: you",
      " analysed differences in males and females separately, then please save",
      " them as separate files."
    ),

    shiny::h4("CpG Data", class = "top-margin-m"),
    shiny::tags$p(
      "For CpG data, we need the data in a matrix, preferably as a compressed",
      " text file (ex: .csv.gz or .tsv.gz), with the following columns:"
    ),
    DT::DTOutput(ns("example_cpg_legend")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_cpg")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("DMR Data", class = "top-margin-m"),
    shiny::tags$p(
      "For DMR data, we need the data in a matrix, with the following columns:"
    ),
    DT::DTOutput(ns("example_dmr_legend")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_dmr")) %>%
      shinycssloaders::withSpinner(proxy.height = 150)
    
  )
}
