# Write functions for dealing with inputting a list of genes in the gene input tab

#' For a list of genomic positions, merge any overlaps
#' 
#' @param df a dataframe that has chr, start, and end columns
remove_genomic_overlap <- function(df) {
  df <- df %>%
    dplyr::select("chr", "start", "end")
  
  if (nrow(df) < 2) {
    return(df)
  }
  
  data_ranges <- GenomicRanges::GRanges(
    seqnames = df$chr,
    ranges = IRanges::IRanges(start = df$start, end = df$end)
  )
  
  data_ranges <- GenomicRanges::reduce(data_ranges)
  
  df <- data.frame(data_ranges)
  df <- dplyr::select(df, chr = "seqnames", "start", "end")
  
  df
}

#' Get the cpgs and their positions for a genomic range
#' 
#' @param input_type whether the input is a 'gene_list' or not
#' @param chr_position_ls a list containing genomic position information
#' @param df_gene_ls a reactive dataframe of genomic position information
#' @param genome_version whether we are using hg19 or hg38 data
get_genomic_range_cpgs <- function(
    input_type, chr_position_ls, df_gene_ls, genome_version
) {
  if (input_type == "gene_list") {
    # reactives and inputs
    df_genes <- remove_genomic_overlap(df_gene_ls)
    
    if (nrow(df_genes) == 0) {
      return(
        filter_cpg_sql_positions(
          genome_version,
          "chr0",
          0,
          0
        )
      )
    }
    
    # get cpgs in target range
    df_pos <- plyr::adply(
      .data = df_genes,
      .margins = 1,
      .fun = function(row) {
        chr = row$chr
        start = row$start
        end = row$end
        
        filter_cpg_sql_positions(
          genome_version,
          chr,
          start,
          end
        )
      }
    )
  } else {
    # reactives and inputs
    select_chromosome <- chr_position_ls$chr
    select_start <- chr_position_ls$start
    select_end <- chr_position_ls$end
    
    # get cpgs in target range
    df_pos <- filter_cpg_sql_positions(
      genome_version,
      select_chromosome,
      select_start,
      select_end
    )
  }
  
  df_pos
}

get_genomic_range_dmrs <- function(
    input_type, chr_position_ls, df_gene_ls, df_dmr, selected_datasets
) {
  df_dmr <- dplyr::filter(df_dmr, .data$dataset %in% selected_datasets) %>%
    dplyr::mutate(index = dplyr::row_number())
  
  if (input_type == "gene_list") {
    gene_gr <- GenomicRanges::GRanges(
      seqnames = df_gene_ls$chr,
      ranges = IRanges::IRanges(start = df_gene_ls$start, end = df_gene_ls$end)
    )
    dmr_gr <- GenomicRanges::GRanges(
      seqnames = df_dmr$chr,
      ranges = IRanges::IRanges(start = df_dmr$start, end = df_dmr$end)
    )
    
    overlap_df <- GenomicRanges::findOverlaps(gene_gr, dmr_gr) %>%
      as.data.frame()
    
    df_dmr <- df_dmr %>%
      dplyr::filter(.data$index %in% overlap_df$subjectHits)
  } else {
    select_chromosome <- chr_position_ls$chr
    select_start <- chr_position_ls$start
    select_end <- chr_position_ls$end
    
    df_dmr <- dplyr::filter(
      df_dmr,
      .data$chr == select_chromosome,
      .data$end >= select_start,
      .data$start <= select_end
    )
  }
  
  df_dmr <- df_dmr %>%
    dplyr::select(
    "DMR", "dataset", sample_group = "source", "phenotype", "direction",
    "nProbes", "pValue") %>%
    dplyr::arrange(.data$pValue) %>%
    dplyr::mutate(pValue = format_pvalues_column(.data$pValue),
                  nProbes = as.integer(.data$nProbes))
  
  df_dmr
}

get_genomic_external_table <- function(
    input_type, chr_position_ls, df_gene_ls, input_gene, df_external
) {
  if (input_type == "gene_list") {
    df_gene <- df_gene_ls
  } else {
    select_chromosome <- chr_position_ls$chr
    select_start <- chr_position_ls$start
    select_end <- chr_position_ls$end
    select_gene <- input_gene
    if (is.na(select_gene) | is.null(select_gene)) {
      select_gene <- ""
    }
    
    df_gene <- data.frame(
      Gene = select_gene,
      chr = select_chromosome,
      start = select_start,
      end = select_end
    )
  }
  
  df_annotation <- df_gene %>%
    dplyr::mutate(
      Genomic_Region = paste0(.data$chr, ":", .data$start, "-", .data$end)
    ) %>%
    dplyr::select(
      "Genomic_Region", "Gene"
    ) %>%
    dplyr::mutate(
      GWAS = ifelse(
        nchar(.data$Gene) > 0,
        create_GWAS_gene_link(.data$Gene),
        create_GWAS_region_link(.data$Genomic_Region)
      )
    ) %>%
    dplyr::mutate(
      AD = create_Niagads_link(
        .data$Gene, df_external %>% dplyr::filter(.data$Niagads)
      ),
      Agora = create_Agora_link(
        .data$Gene, df_external %>% dplyr::filter(.data$Agora)
      )
    )
  
  df_annotation
}