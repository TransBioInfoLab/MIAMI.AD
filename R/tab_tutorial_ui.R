tab_tutorial_UI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(shiny::column(
      width = 10, offset = 1,
      shiny::h3(shiny::tags$b("Overview")),
      shiny::tags$p(
        "MIAMI-AD (",
        shiny::tags$a(
          href = "https://miami-ad.org/",
          shiny::em("https://miami-ad.org"),
          target="_blank"
        ),
        ") is a web application developed using the Shiny platform",
        shiny::tags$sup("1-3"),
        ". The web interface is organized into four main sections,",
        shiny::tags$b(
          "Genome-wide Query, Gene Query, CpG Query, and Epigenetic Clock Query"
        ),
        ". Each of these sections is further divided into three subsections,"
      ),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            shiny::tags$b("Datasets: "),
            "Here, users can select the studies or epigenetic clocks they wish",
            " to examine. The other subtabs remain hidden until a selection is",
            " made in this section."
          ),
          shiny::tags$li(
            shiny::tags$b("Display Data: "),
            "This section shows tables of summary statistics tailored to the",
            " chosen datasets and relevant parameters from the main tabs.",
            " Users also have the option to download these tables as Excel",
            " files."
          ),
          shiny::tags$li(
            shiny::tags$b("Display Plots: "),
            "Visual representations of the data can be found here, including",
            " Manhattan plots, Venn diagrams, forest plots, and genomic",
            " annotations, annotation from the UCSC track hub, and computed",
            " chromatin states from the NIH Roadmap Epigenomics project",
            shiny::tags$sup("4")
          )
        )
      ),

      shiny::tags$p(
        "The source code for MIAMI-AD is available at ",
        shiny::tags$a(
          href = "https://github.com/TransBioInfoLab/MIAMI.AD",
          shiny::em("https://github.com/TransBioInfoLab/MIAMI.AD"),
          target = "_blank"
        ),
        "."
      ),

      shiny::h3(shiny::tags$b("The Genome-wide Query tool")),
      shiny::tags$p(
        "Designed to allow users to select CpGs across all chromosomes based",
        " on a significance threshold, this tool is valuable for comparing",
        " association results from one or more studies. ",
        shiny::tags$b("Figure 1"),
        " illustrates the workflow of the Genome-wide Query:"
      ),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            "First, the user can choose one or more phenotypes of interest",
            " from options such as ",
            shiny::tags$em("'AD Neuropathology'"),
            ", ",
            shiny::tags$em("'Aging'"),
            ", ",
            shiny::tags$em("'AD Biomarkers'"),
            ", ",
            shiny::tags$em("'Dementia Clinical Diagnosis'"),
            ", ",
            shiny::tags$em("'Mild Cognitive Impairment (MCI)'"),
            ", or ",
            shiny::tags$em("'Sex'"),
            ", available in the left panel."
          ),
          shiny::tags$li(
            "On the right panel, under the ",
            shiny::tags$b("Dataset"),
            " tab, a table displays the studies and datasets associated with",
            " the selected phenotypes. Here, we refer to ",
            shiny::tags$em("'studies'"),
            " as research publications and ",
            shiny::tags$em("'datasets'"),
            " as the resulting summary statistics from these studies."
          ),
          shiny::tags$li(
            "Next, the user can select datasets of interest by checking the",
            " boxes in the rightmost column. The ",
            shiny::tags$b("Display Data"),
            " tab then presents the CpGs that meet the search criteria. It",
            " includes annotations on location and associated genes, along",
            " with summary statistics such as the direction of the",
            " association, test statistic values, raw ",
            shiny::tags$em("P"),
            " values, and multiple comparison-adjusted ",
            shiny::tags$em("P"),
            " values."
          ),
          shiny::tags$li(
            "Additionally, the ",
            shiny::tags$b("Display Plot"),
            " tab generates visualizations, such as Venn diagrams to",
            " illustrate the numbers of overlapping and unique CpGs across",
            " multiple studies, as well as Manhattan plots for genome-wide",
            " representation of the data."
          )
        )
      ),

      shiny::div(
        align = "center",
        shiny::imageOutput(ns("figure1"), height = "100%", width = "100%")
      ),

      shiny::h3(shiny::tags$b("The Gene Query tool")),
      shiny::tags$p(
        "This tool provides detailed information on DNAm differences within a",
        " specific gene or genomic region."
      ),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            "The interface for input is similar to the Genome-wide Query Tool,",
            " except that on the left panel, the user additionally specifies",
            " the name of the gene (or region) they want to explore and",
            " selects the desired annotation tracks such as UCSC gene,",
            " ENSEMBL genes, ENSEMBL transcripts, chromatin states,",
            " and CpG islands (",
            shiny::tags$b("Figure 2"),
            ")"
          ),
          shiny::tags$li(
            "Upon completing the inputs, the ",
            shiny::tags$b("Display Data"),
            " tab on the right panel presents a summary of statistics for CpGs",
            " located within a specified range of ",
            shiny::tags$u("+"),
            "2 kb around the gene of",
            " interest. This summary includes information on the direction of",
            " association and the values of the statistical measures (e.g.,",
            " odds ratio, t-statistic, and the corresponding ",
            shiny::tags$em("P"),
            ' values). In addition, under “Multi-omics Resources”, links to',
            " GWAS catalog",
            shiny::tags$sup("5"),
            ", NIAGADS GenomicsDB",
            shiny::tags$sup("6"),
            ", and the Agora brain gene expression",
            shiny::tags$sup("7"),
            " databases are presented, providing users a more comprehensive",
            " view on the genomic region or gene."
          ),
          shiny::tags$li(
            "The ",
            shiny::tags$b("Display Plot"),
            " tab generates visualizations, including a mini-Manhattan plot of",
            " the CpGs found within the gene, along with the selected",
            " annotation tracks such as CpG island, gene transcripts, and",
            " computed chromatin states. These plots offer a clear and concise",
            " way to interpret the data and understand the relationships",
            " between DNAm and the phenotype in the gene or region of interest."
          )
        )
      ),

      shiny::div(
        align = "center",
        shiny::imageOutput(ns("figure2"), height = "100%", width = "100%")
      ),

      shiny::h3(shiny::tags$b("The CpG Query tool")),
      shiny::tags$p(
        "This tool offers information about specific CpGs of interest (",
        shiny::tags$b("Figure 3"),
        ") To use this tool,"
      ),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            "The user begins by inputting the desired phenotypes on the left",
            " panel and providing a list of CpGs they want to explore. In the",
            " right panel, the user selects the relevant studies from the",
            " available ",
            shiny::tags$b("Datasets.")
          ),
          shiny::tags$li(
            "In the ",
            shiny::tags$b("Display Data"),
            " tab, the tool provides not only summary statistics for the",
            " selected CpGs but also valuable annotations for each CpG. These",
            " annotations include CpG location and genes associated with the",
            ' CpG. Under “Multi-omics and Multi-tissue Resources”, the tool',
            " provides links to mQTL (methylation quantitative trait loci)",
            "information obtained from a recent large-scale meta-analysis",
            shiny::tags$sup("8"),
            ", correlation of blood DNAm with brain DNAm at the particular CpG",
            shiny::tags$sup("9"),
            ", GWAS catalog",
            shiny::tags$sup("5"),
            ", NIAGADS GenomicsDB",
            shiny::tags$sup("6"),
            ", and the Agora brain gene expression",
            shiny::tags$sup("7"),
            "databases. Additionally, the tool includes the correlation of",
            " blood DNAm with brain DNAm at the particular CpG",
            shiny::tags$sup("9"),
            ", offering insights into potential cross-tissue relationships.",
            " Furthermore, MIAMI-AD searches among recently published",
            " epigenetic clocks and identifies matches if a selected CpG is a",
            " component of any epigenetic clock."
          ),
          shiny::tags$li(
            "Under the ",
            shiny::tags$b("Display Plot"),
            " tab, the tool generates forest plots to illustrate the",
            " association of the selected CpGs with the specified phenotypes",
            " across different datasets. This visualization aids in",
            " understanding the relationship between DNAm at CpG sites and the",
            " various phenotypes of interest."
          )
        )
      ),

      shiny::div(
        align = "center",
        shiny::imageOutput(ns("figure3"), height = "100%", width = "100%")
      ),

      shiny::h3(shiny::tags$b("The Epigenetic Clock Query tool")),
      shiny::p(
        "This tool serves the purpose of comparing with and selecting CpGs",
        " that have been utilized in constructing blood-based epigenetic",
        " clocks (",
        shiny::tags$b("Figure 4"),
        "). It incorporates the widely described pan-tissue clock by Horvath",
        " (2013)",
        shiny::tags$sup("7"),
        ", as well as the recently developed DunedinPACE clock, which is",
        " associated with the risk of dementia onset",
        shiny::tags$sup("8"),
        ", among others. To use the Epigenetic Clock tool,"
      ),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            "Users start by choosing the specific epigenetic clocks of",
            "interest from the ",
            shiny::tags$b("Dataset"),
            " category. "
          ),
          shiny::tags$li(
            "Subsequently, upon navigating to the ",
            shiny::tags$b("Display Data"),
            " tab, the tool provides a list of the CpGs that are incorporated",
            " into one or more of the selected clocks, accompanied by their",
            " respective coefficients within each clock."
          ),
          shiny::tags$li(
            "In the ",
            shiny::tags$b("Display Plot"),
            " section, users can view the number of CpGs shared across",
            " multiple clocks, as well as those unique to individual clocks,",
            " as visualized by Venn diagrams."
          )
        )
      ),

      shiny::div(
        align = "center",
        shiny::imageOutput(ns("figure4"), height = "100%", width = "100%")
      ),

      shiny::h3(shiny::tags$b("Citations")),
      shiny::p(
        "Lukacsovich D et al. (2023) MIAMI-AD (Methylation in Aging and",
        " Methylation in AD): an integrative atlas of DNA methylation across",
        " sex, aging, and Alzheimer's disease. ",
        shiny::tags$a(
          href = "https://www.medrxiv.org/content/10.1101/2023.12.04.23299412v1",
          shiny::em("medRxiv"),
          target="_blank"
        )
      ),
      shiny::tags$p(
        "Please also cite the original study papers in which the results were obtained."
      ),

      shiny::h3(shiny::tags$b("References")),
      shiny::div(
        shiny::tags$ol(
          shiny::tags$li(
            "Chang, W. et al. shiny: Web Application Framework for R. ",
            shiny::tags$em("R package version 1.7.4, "),
            shiny::tags$a(
              href = "https://CRAN.R-project.org/package=shiny",
              "https://CRAN.R-project.org/package=shiny",
              target = "_blank"
            ),
            " (2022)."
          ),
          shiny::tags$li(
            "Attali, D. shinyjs: Easily Improve the User Experience of Your",
            " Shiny Apps in Seconds. ",
            shiny::tags$em("R pakcage version 2.1.0"),
            ", ",
            shiny::tags$a(
              href = "https://CRAN.R-project.org/package=shinyjs",
              "https://CRAN.R-project.org/package=shinyjs",
              target = "_blank"
            ),
            " (2021)."
          ),
          shiny::tags$li(
            "Perrier, V., Meyer, F. & Granjon, D. shinyWidgets: Custom Inputs",
            " Widgets for Shiny ",
            shiny::tags$em("R package version 0.7.6"),
            ", ",
            shiny::tags$a(
              href = "https://CRAN.R-project.org/package=shinyWidgets",
              "https://CRAN.R-project.org/package=shinyWidgets",
              target = "_blank"
            ),
            " (2023)."
          ),
          shiny::tags$li(
            "Zhou, X. et al. Epigenomic annotation of genetic variants using",
            " the Roadmap Epigenome Browser. ",
            shiny::tags$em("Nat Biotechnol "),
            shiny::tags$b("33"),
            ", 345-6 (2015)."
          ),
          shiny::tags$li(
            "Sollis, E. et al. The NHGRI-EBI GWAS Catalog: knowledgebase and",
            " deposition resource. ",
            shiny::tags$em("Nucleic Acids Res"),
            " ",
            shiny::tags$b("51"),
            ", D977-D985 (2023)."
          ),
          shiny::tags$li(
            "Greenfest-Allen, E. et al. NIAGADS Alzheimer's GenomicsDB: A",
            " resource for exploring Alzheimer's disease genetic and genomic",
            " knowledge ",
            shiny::tags$em("Alzheimers Dement"),
            " ",
            shiny::tags$b("20"),
            ", 1123-1136 (2024)."
          ),
          shiny::tags$li(
            "Sage Bionetworks. Agora: Discover Alzheimer's Disease Genes.",
            shiny::tags$a(
              href = "https://agora.adknowledgeportal.org/",
              "https://agora.adknowledgeportal.org/",
              target = "_blank"
            ),
            " (2024)."
          ),
          shiny::tags$li(
            "Min, J.L. et al. Genomic and phenotypic insights from an atlas of",
            " genetic effects on DNA methylation. ",
            shiny::tags$em("Nat Genet "),
            shiny::tags$b("53"),
            ", 1311-1321 (2021)."
          ),
          shiny::tags$li(
            "Hannon, E., Lunnon, K., Schalkwyk, L. & Mill, J. Interindividual",
            " methylomic variation across blood, cortex, and cerebellum:",
            " implications for epigenetic studies of neurological and",
            " neuropsychiatric phenotypes. ",
            shiny::tags$em("Epigenetics "),
            shiny::tags$b("10"),
            " 1024-32 (2015)."
          ),
          shiny::tags$li(
            "Horvath, S. DNA methylation age of human tissues and cell types. ",
            shiny::tags$em("Genome Biol "),
            shiny::tags$b("14"),
            ", R115 (2013)."
          ),
          shiny::tags$li(
            "Sugden, K. et al. Association of Pace of Aging Measured by",
            " Blood-Based DNA Methylation With Age-Related Cognitive",
            " Impairment and Dementia. ",
            shiny::tags$em("Neurology "),
            shiny::tags$b("99"),
            ", e1402-e1413 (2022)."
          )
        )
      ),
      shiny::br()
    ))
  )
}