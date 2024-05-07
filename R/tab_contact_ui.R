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
      " information about your paper and the dataset",
    ),
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("PMID"),
          " - Your paper's PMID"
          ),
        shiny::tags$li(
          shiny::tags$b("Author"),
          " - The first author of your paper"
        ),
        shiny::tags$li(
          shiny::tags$b("Year"),
          " - The year of the paper's publication"
        ),
        shiny::tags$li(
          shiny::tags$b("Description"),
          " - A ", shiny::tags$b("brief"), " description of the data"
        ),
        shiny::tags$li(
          shiny::tags$b("Label_Phenotype"),
          " - The phenotype to show on the displayed tables"
        ),
        shiny::tags$li(
          shiny::tags$b("CpG_Phenotype"),
          " - The Phenotype to show on the CpG Query metaplot titles"
        ),
        shiny::tags$li(
          shiny::tags$b("Sex_Specific"),
          " - Whether the data is sex-specific, and if so whether it is",
          " specific to a given gender, or is measuring sex-effects"
        ),
        shiny::tags$li(
          shiny::tags$b("Statistics_Label"),
          " - A very brief description of your CpG statistics, to be displayed",
          " on tables. If you are only submitting DMR data, you can ignore this"
        ),
        shiny::tags$li(
          shiny::tags$b("Statistics"),
          " - Whether the CpG statistics are Odds Ratio (OR), or something",
          " else (Estimate).  If you are only submitting DMR data, you can",
          " ignore this"
        ),
        shiny::tags$li(
          shiny::tags$b("Phenotype"),
          " - The phenotype(s) that best describe your dataset. If you are",
          " using a new phenotype, please keep them short, and avoid special",
          " characters"
        ),
        shiny::tags$li(
          shiny::tags$b("Full_EWAS"),
          " - Whether the CpG data contains all/most CpGs from an array, or",
          " only a select subset. If you are only submitting DMR data, you can",
          " ignore this"
        )
      )
    ),
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
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("cpg"),
          " - the names of the cpg probes"
        ),
        shiny::tags$li(
          shiny::tags$b("sample_group"),
        " - The dataset, or source that the raw data was collected from. If",
        " the results are a meta-data analysis, separate the sources by a",
        " space and plus sign ( + ). Otherwise, please don't use plus (+) or",
        " semi-colon (;) in the names"
        ),
        shiny::tags$li(
          shiny::tags$b("estimate"),
          " - the measured statistical estimate value"
        ),
        shiny::tags$li(
          shiny::tags$b("std_err"),
          " - the standard error on the statistical estimate"
        ),
        shiny::tags$li(
          shiny::tags$b("statistics_value"),
          " - The value of the test statistics. In most cases, this will be",
          " the same as the estimate. If you are measuring Odds Ratio (OR),",
          " this should be >0, centered on 1, and estimate should be centered",
          " on 0."
        ),
        shiny::tags$li(
          shiny::tags$b("pvalue"),
          " - the significance p-value"
        ),
        shiny::tags$li(
          shiny::tags$b("fdr"),
          " - multiple testing adjusted p-value"
        )
      )
    ),
    
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_cpg")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),

    shiny::h4("DMR Data", class = "top-margin-m"),
    shiny::tags$p(
      "For DMR data, we need the data in a matrix, with the following columns:"
    ),
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("region"),
          " - The genomic region of the DMR in the from CHR:START-END. If this",
          " is provided, there is no need to provide ", shiny::em("chr"), ", ",
          shiny::em("start"), ", or ", shiny::em("end"), "."
        ),
        shiny::tags$li(
          shiny::tags$b("chr"),
          " - The chromosome the DMR is located on"
        ),
        shiny::tags$li(
          shiny::tags$b("start"),
          " - The genomic start position of the DMR"
        ),
        shiny::tags$li(
          shiny::tags$b("end"),
          " - The genomic end position of the DMR"
        ),
        shiny::tags$li(
          shiny::tags$b("source"),
          " - The dataset, or source that the raw data was collected from. If",
          " the results are a meta-data analysis, separate the sources by a",
          " space and plus sign ( + ). Otherwise, please don't use plus (+) or",
          " semi-colon (;) in the names"
        ),
        shiny::tags$li(
          shiny::tags$b("nProbes"),
          " - The number of CpG probes in the DMR"
        ),
        shiny::tags$li(
          shiny::tags$b("pValue"),
          " - the significance p-value"
        ),
        shiny::tags$li(
          shiny::tags$b("adj.pValue"),
          " - multiple testing adjusted p-value"
        ),
        shiny::tags$li(
          shiny::tags$b("direction"),
          " - Whether the DMR is up- or down-regulated"
        )
      )
    ),
    
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_dmr")) %>%
      shinycssloaders::withSpinner(proxy.height = 150)
    
  )
}
