tab_cpg_data_server <- function(id, common, df_toplot, df_selection_dt, input_selection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
    raw_data <- common$raw_data

    # define tables from raw_data
    df_labels <- raw_data$labels
    cpg_London <- raw_data$London
    cpg_mQTL <- raw_data$mQTL
    df_family <- raw_data$clocks
    df_external <- raw_data$external

    output$data_selection_data <- DT::renderDT({
      df_selection_dt()
    })

    # Create Tables
    ## Get Selected CpGS
    selected_cpgs <- shiny::reactive({
      # get reactives and inputs
      input_selection_ls <- input_selection()
      input_type <- input_selection_ls$input_type
      select_cpgId <- input_selection_ls$select_cpgId
      select_file <- input_selection_ls$select_file

      if (input_type == "manual"){
        # separate cpgs by comma (,), empty spaces ( +), and tabs (\t)
        cpgs <- unlist(stringr::str_split(select_cpgId, ",| +|\t|;"))
        cpgs <- unique(cpgs)
        cpgs <- cpgs[nchar(cpgs) > 0]
        return (cpgs)
      }

      if (input_type == "file"){
        if (is.null(select_file)) {
          return (ls())
        }
        filename <- select_file$datapath

        if (!file.exists(filename)){
          return (ls())
        }

        # read cpgs, allowing split by new lines (\n), comma (,),
        # empty spaces ( +), or tabs (\t)

        cpgs <- unlist(stringr::str_split(readLines(filename), ",| +|\t|;"))
        cpgs <- gsub('[\"]', "", cpgs)
        cpgs <- unique(cpgs)
        cpgs <- cpgs[nchar(cpgs) > 0]

        return (cpgs)
      }
    })

    ## CpG List Table
    df_cpg_targets <- shiny::reactive({
      # get reactives and inputs
      cpgs <- selected_cpgs()

      if (length(cpgs) == 0){
        df_cpg_targets <- data.frame(
          cpg = character(),
          chr = character(),
          pos = integer(),
          Illumina = character(),
          Relation_to_Island = character(),
          RefGene_Group = character(),
          mQTL = character(),
          London = character()
        )
      } else {
        df_pos <- get_cpg_sql_positions(cpgs, common$genome_version())
        df_cpg_param <- get_cpg_sql_parameters(df_pos$cpg, unique = FALSE)
        df_cpg_targets <- df_pos %>%
          dplyr::inner_join(df_cpg_param, by = c("cpg" = "cpg"))
      }
      
      return (df_cpg_targets)
    })
    
    ## Get Database Table
    df_external_targets <- shiny::reactive({
      # get reactives and inputs
      df_cpg_targets <- df_cpg_targets()
      
      # get target genes and cpgs
      df_cpg <- df_cpg_targets %>%
        dplyr::select("cpg", "Illumina") %>%
        dplyr::filter(nchar(.data$Illumina) > 0) %>%
        dplyr::mutate(Gene = strsplit(.data$Illumina, ";")) %>%
        tidyr::unnest("Gene") %>%
        dplyr::select("cpg", "Gene") %>%
        dplyr::distinct()
      
      # add annotations
      df_cpg <- df_cpg %>%
        dplyr::mutate(
          mQTL = create_mQTL_Link(.data$cpg, cpg_mQTL),
          London = create_London_Link(.data$cpg, cpg_London),
          GWAS = create_GWAS_gene_link(.data$Gene),
          AD = create_Niagads_link(
            .data$Gene, df_external %>% dplyr::filter(.data$Niagads)
          ),
          Agora = create_Agora_link(
            .data$Gene, df_external %>% dplyr::filter(.data$Agora)
          )
        )
      
      df_cpg
    })

    ## CpG Compilation Table
    df_cpg_stats <- shiny::reactive({
      # get reactives and inputs
      df_cpg_targets <- df_cpg_targets()
      df_plotdata <- df_toplot()

      # get list of targets
      Datasets <- df_plotdata$Dataset
      cpgs <- df_cpg_targets %>%
        dplyr::pull("cpg")

      if (length(Datasets) == 0 | length(cpgs) == 0){
        df_stats <- data.frame(
          cpg = character(),
          dataset = character(),
          sample_group = character(),
          pvalue = numeric(),
          statistics_value = numeric(),
          estimate = numeric(),
          std_err = numeric()
        )

        return (df_stats)
      }

      # get cpg statistics
      #df_stats <- get_cpg_statistics(cpgs, Datasets, method="Name")
      df_stats <- get_cpg_sql_statistics(cpgs, Datasets) %>%
        dplyr::arrange(.data$pvalue)

      return (df_stats)
    })

    ## Legends Table
    data_indiv_base <- shiny::reactive({
      # get reactives and inputs
      df_cpg_stats <- df_cpg_stats()
      df_plotdata <- df_toplot()
      selected_cpgs <- selected_cpgs()

      df_labels <- df_labels %>%
        dplyr::filter(.data$Dataset %in% df_plotdata$Dataset) %>%
        dplyr::select(-"Phenotype")

      df_stats <- create_statistics_table(
        df_cpg_stats, df_labels, table_category = "CpG"
      )

      return (df_stats)
    })

    data_epigenetic_show <- shiny::reactive({
      # get reactives and inputs
      selected_cpgs <- selected_cpgs()

      # get epigenetic dta
      df_family <- df_family %>%
        dplyr::filter(.data$cpg %in% selected_cpgs) %>%
        dplyr::mutate(Coefficient = round(.data$Coefficient, 3)) %>%
        tidyr::pivot_wider(names_from = "Family",
                    values_from = "Coefficient")

      return (df_family)
    })

    # Display Tables
    output$data_properties <- DT::renderDT({
      # get reactives and inputs
      df_cpg_targets <- df_cpg_targets()

      df_cpg_targets <- df_cpg_targets %>%
        dplyr::rename(CpG = "cpg")

      full_options <- list(columnDefs = list(
        list(className = 'dt-center', targets = "_all")),
        autowidth=F,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "None of the selected CpGs were in our database."))
      )

      DT::datatable(
        df_cpg_targets,
        rownames = FALSE,
        options = full_options
      )
    })
    
    output$data_external <- DT::renderDT({
      df_external_targets <- df_external_targets()
      
      df_external_targets <- df_external_targets %>%
        dplyr::rename(
          `Blood-Brain comparison` = "London",
          `AD Genomics` = "AD",
          `Gene Expression` = "Agora"
        )
      
      full_options <- list(columnDefs = list(
        list(className = 'dt-center', targets = "_all")),
        autowidth=F,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "None of the selected CpGs were in our database."))
      )
      
      DT::datatable(
        df_external_targets,
        escape = c(-3, -4, -5, -6, -7),
        rownames = FALSE,
        options = full_options
      )
    })

    output$data_indiv <- DT::renderDT({
      # get reactives and inputs
      data_indiv_base <- data_indiv_base()

      data_indiv_base <- data_indiv_base %>%
        dplyr::mutate(pStat = .data$pValue,
                      pValue = format_pvalues_column(.data$pValue))

      full_options <- list(columnDefs = list(
        list(orderData = 9, targets = 8),
        list(visible=FALSE, targets = 9),
        list(className = 'dt-center', targets = "_all")),
        autowidth=F,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "None of the selected CpGs were in the selected datasets."))
      )

      DT::datatable(data_indiv_base, rownames=FALSE, options = full_options)
    })

    output$data_epigenetic <- DT::renderDT({
      # get reactives and inputs
      data_epigenetic_show <- data_epigenetic_show()

      full_options <- list(columnDefs = list(
        list(className = 'dt-center', targets = "_all")),
        autowidth=F,
        language = list(
          zeroRecords = paste0(
            "No CpGs available. - ",
            "None of the selected CpGs were present in any epigenetic clocks."))
      )
      DT::datatable(data_epigenetic_show, rownames=FALSE, options = full_options)
    })

    # Download Data
    output$download_data <- shiny::downloadHandler(
      filename = function(){"CpG Statistics.xlsx"},
      content = function(filename){
        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Dataset Abbreviations")
        df_datasets <- df_toplot() %>%
          dplyr::select(-"PMID") %>%
          dplyr::rename(PMID = "PMID_Excel")
        openxlsx::writeData(wb, df_datasets, sheet="Dataset Abbreviations")

        openxlsx::addWorksheet(wb, "Annotations")
        df_annot <- df_cpg_targets()
        openxlsx::writeData(wb, df_annot, sheet = "Annotations")

        openxlsx::addWorksheet(wb, "Individual Datasets")
        openxlsx::writeData(wb, data_indiv_base(), sheet = "Individual Datasets")

        openxlsx::addWorksheet(wb, "Epigenetic Clocks")
        openxlsx::writeData(wb, data_epigenetic_show(), sheet = "Epigenetic Clocks")

        openxlsx::saveWorkbook(wb, file = filename)
      },
      contentType = "file/xlsx"
    )

    return(list(df_cpg_stats = df_cpg_stats))
  })
}
