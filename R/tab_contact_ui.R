tab_contact_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Contact & Contribute"
    ),
    
    shiny::h5(shiny::tags$b("Contact")),
    shiny::p(
      "For questions, comments, or to contribute to  MIAMI-AD, please contact",
      " us at:"),
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

    shiny::h5(shiny::tags$b("Contribute")),
    shiny::tags$p(
      "To contribute your publication's data to MIAMI-AD, please send the ",
      shiny::tags$b("Metadata of publication"),
      " and ",
      shiny::tags$b("CpG Summary Statistics"),
      " and/or ",
      shiny::tags$b("DMR Summary Statistics"),
      " files below to the contact information. Since data files can be large,",
      " please upload them to an online storage site (e.g., Dropbox, Google",
      " Drive, or Box), and share the download link with us."
    ),
    shiny::tags$p(
      "Published studies in MAIMI-AD meet three criteria:"
    ),
    shiny::div(
      shiny::tags$ol(
        shiny::tags$li(
          "having more than 100 total samples from human subjects"
        ),
        shiny::tags$li(
          "conducting a genome-wide study of more than 100k CpGs"
        ),
        shiny::tags$li(
          "utilizing Illumina 450k, EPIC, or EPICv2 arrays"
        )
      )
    ),

    shiny::h6(shiny::tags$b("Paper Metadata"), class = "top-margin-m"),
    shiny::tags$p(
      "Please provide the following information about your publication:",
    ),
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("Title"),
          " - Paper's title"
        ),
        shiny::tags$li(
          shiny::tags$b("PMID"),
          " - Your paper's PMID"
          ),
        shiny::tags$li(
          shiny::tags$b("Author"),
          " - First author of your paper"
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
          shiny::tags$b("Tissue"),
          " - The tissue that DNA methylation is measured on"
        ),
        shiny::tags$li(
          shiny::tags$b("Phenotype"),
          " - The phenotype of the study (e.g., AD biomarkers, AD",
          " Neuropathology, Aging, Dementia Clinical Diagnosis, MCI, or Sex)"
        ),
        shiny::tags$li(
          shiny::tags$b("Sex_Specific"),
          " - Specify if the study-result is sex-specific, and if so whether",
          " it is specific to a given gender, or if it measures sex-effects"
        ),
        shiny::tags$li(
          shiny::tags$b("Statistics"),
          " - Statistics in the Summary Statistics dataset (see below) (e.g.,",
          " OR for AD, estimate for age effect)"
        ),
        shiny::tags$li(
          shiny::tags$b("Full_EWAS"),
          " - Specify if summary statistics include all (or most) CpGs, or",
          " just a subset from an array (omit if submitting only DMR data)"
        )
      )
    ),

    shiny::h6(shiny::tags$b("CpG Summary Statistics"), class = "top-margin-m"),
    shiny::tags$p(
      "Please provide summary statistics in a (preferably compressed) text",
      " file (e.g., .csv, .csv.gz, .tsv.gz), with the following columns:"
    ),
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("CpG"),
          " - Names of the CpG probes"
        ),
        shiny::tags$li(
          shiny::tags$b("sample_group"),
        " - The cohorts that raw data was collected from. For meta-analysis,",
        " separate the cohorts with a space and a plus sign (+). For example:",
        " AIBL + ADNI. Otherwise, please avoid using plus (+) or semi-colon",
        " (;) in cohort names."
        ),
        shiny::tags$li(
          shiny::tags$b("estimate"),
          " - Estimated effect size"
        ),
        shiny::tags$li(
          shiny::tags$b("std_err"),
          " - Standard error of the estimated effect size"
        ),
        shiny::tags$li(
          shiny::tags$b("pvalue"),
          " - Nominal P-value"
        ),
        shiny::tags$li(
          shiny::tags$b("fdr"),
          " - Multiple comparison adjusted P-value"
        )
      )
    ),
    
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_cpg")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    
    shiny::tags$p(
      "Note that if your study includes results for multiple phenotypes or",
      " subgroups (e.g., you analyzed male and female samples separately),",
      " then please save the results as separate files."
    ),

    shiny::h6(shiny::tags$b("DMR Summary Statistics"), class = "top-margin-m"),
    shiny::tags$p(
      "Please provide summary statistics in a text file (e.g., .csv, .tsv),",
      " with the following columns:"
    ),
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("DMR"),
          " - Genomic region of the DMRs in the format CHR:START-END. If this",
          " is provided, there is no need to provide ", shiny::em("chr"), ", ",
          shiny::em("start"), ", or ", shiny::em("end"), "."
        ),
        shiny::tags$li(
          shiny::tags$b("chr"),
          " - The chromosome the DMR is located on. If ",
          shiny::em("region"),
          " is provided, this does not need to be provided."
        ),
        shiny::tags$li(
          shiny::tags$b("start"),
          " - The genomic start position of the DMR. If ",
          shiny::em("region"),
          " is provided, this does not need to be provided."
        ),
        shiny::tags$li(
          shiny::tags$b("end"),
          " - The genomic end position of the DMR. If ",
          shiny::em("region"),
          " is provided, this does not need to be provided."
        ),
        shiny::tags$li(
          shiny::tags$b("sample_group"),
          " - The cohorts that raw data was collected from. For meta-analysis,",
          " separate the cohorts with a space and a plus sign (+). For example:",
          " AIBL + ADNI. Otherwise, please avoid using plus (+) or semi-colon",
          " (;) in cohort names."
        ),
        shiny::tags$li(
          shiny::tags$b("nProbes"),
          " - The number of CpG probes in the DMR"
        ),
        shiny::tags$li(
          shiny::tags$b("pvalue"),
          " - Nominal P-value"
        ),
        shiny::tags$li(
          shiny::tags$b("adjusted.P"),
          " - Multiple comparison adjusted P-value"
        ),
        shiny::tags$li(
          shiny::tags$b("direction"),
          " - Whether the DMR is hyper- or hypo- methylated"
        )
      )
    ),
    
    shiny::tags$p(
      "Example:"
    ),
    DT::DTOutput(ns("example_dmr")) %>%
      shinycssloaders::withSpinner(proxy.height = 150),
    
    shiny::tags$p(
      "Note that if your study includes results for multiple phenotypes or",
      " subgroups (e.g., you analyzed male and female samples separately),",
      " then please save the results as separate files."
    ),
    
    shiny::br()
    
  )
}
