#' Create the gene or genomic-range selector input
#'
#' @param ns Shiny namespace
#' @param id Shiny namespace id
create_gene_conditional_input <- function(ns = shiny::NS(NULL), id = "") {
  dropdown_text <- I(paste0(
    "function($dropdown) {if (!this.lastQuery.length)",
    " {this.close(); this.settings.openOnFocus = false;}}"
    ))

  tag <- shiny::tagList(
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s-input_type'] == 'gene'", id),
      shiny::selectizeInput(
        inputId = ns("select_gene"),
        label = shiny::tags$b("Select a Gene"),
        multiple = FALSE,
        choices = NULL,
        width = "150px",
        options = list(
          create = FALSE,
          placeholder = "Type Gene",
          maxItems = '1',
          onDropdownOpen = dropdown_text,
          onType = I(
            "function (str) {if (str === \"\") {this.close();}}")
        )
      ),
    ),
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s-input_type'] == 'range'", id),
      shiny::textInput(
        inputId = ns("select_range"),
        label = shiny::tags$b("Type genomic region"),
        value = "",
        placeholder = "CHR:START-END",
        width = "200px"
      )
    ),
    shiny::actionButton(ns("command_example"), "Example Input", width="150px")
  )

  return(tag)
}

#' Create the CpG list selector input
#'
#' @param ns Shiny namespace
#' @param id Shiny namespace id
create_cpg_conditional_input <- function(ns = shiny::NS(NULL), id = "") {
  tag <- shiny::tagList(
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s-input_type'] == 'manual'", id),
      shiny::textInput(
        inputId = ns("select_cpgId"),
        label = shiny::tags$b("Paste a list of comma or space-separated CpGs"),
        value = "",
        placeholder = "Type a list of CpGs"
      ),
      shiny::actionButton(ns("command_example"), 'Example Input', width='150px')
    ),
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s-input_type'] == 'file'",
        id),
      shiny::fileInput(
        inputId = ns("select_file"),
        label = shiny::tags$b(
          "Upload a file of comma- or space-separated CpGs"))
    )
  )

  return(tag)
}

#' Create the genome version selector input
#'
#' @param ns Shiny namespace
#' @param tour Whether or not to wrap a tour container around the input
create_genome_version_input <- function(ns = shiny::NS(NULL), tour = TRUE) {
  tag <- shiny::selectInput(
    inputId = ns("genome_version"),
    label = shiny::h2(class = "input-header", "Genome Version"),
    choices = c("hg19", "hg38"),
    width = "100%"
  )

  if (tour) {
    shiny::div(
      id = ns("genome_version_tour"),
      tag
    )
  } else {
    tag
  }
}

#' Create the phenotype selector input
#'
#' @param labels The options to show
#' @param ns Shiny namespace
#' @param preselected whether to have the phenotypes pre-selected or not
#' @param two_col whether to have the options in two columns (TRUE) or one
#'   column (FALSE)
#' @param tour Whether or not to wrap a tour container around the input
create_phenotype_input <- function(
    labels,
    ns = shiny::NS(NULL),
    preselected = FALSE,
    two_col = FALSE,
    tour = TRUE
) {
  if (preselected) {
    selected <- sort(unique(labels))
  } else {
    selected <- NULL
  }
  
  tag <- shiny::checkboxGroupInput(
    inputId = ns("select_phenotype"),
    label = shiny::h2(class = "input-header", "Phenotype"),
    inline = FALSE,
    choices = sort(unique(labels)),
    selected = selected,
    width = "100%"
  )
  
  if (two_col) {
    tag <- twocols(tag)
  }

  if (tour) {
    shiny::div(
      id = ns("select_phenotype_tour"),
      tag
    )
  } else {
    tag
  }
}

#' Modify a checkboxGroupInput to have two columns for the options
#'
#' @param tag A checkboxGroupInput tag
twocols <- function(tag) {
  if (!inherits(tag, "shiny.tag")) {
    stop("twocols: `tag` must be a shiny checkboxGroupInput")
  }

  classes <- strsplit(htmltools::tagGetAttribute(tag, "class"), "\\s+")[[1]]
  if (!"shiny-input-checkboxgroup" %in% classes) {
    stop("twocols: `tag` must be a shiny checkboxGroupInput")
  }

  htmltools::tagQuery(tag)$
    find(".shiny-options-group")$
    addClass("twocols")$
    allTags()
}

#' Modify a shiny actionButton to look better in "cerulean" theme in bootstrap 5
#'
#' In bootstrap 3, shiny actions have a "btn-default" class. In bootstrap 5, this class
#' no longer exists. There is a visual bug with the cerulean theme in bootstrap 5 in shiny
#' which can be fixed by replacing shiny's actionButton with this one.
#' @param ... All parameters passed to shiny actionButton
actionButton <- function(...) {
  htmltools::tagQuery(shiny::actionButton(...))$
    removeClass("btn-default")$
    addClass("btn-light")$
    allTags()
}

#' Create a lighter version of a button
#'
#' This function should only be used if bootstrap 5 is used.
#' @param ... All parameters passed to shiny actionButton
lightButton <- function(...) {
  htmltools::tagQuery(shiny::actionButton(...))$
    removeClass("btn-default")$
    addClass("btn-outline-primary")$
    allTags()
}

#' Create a primary button
#'
#' @param ... All parameters passed to shiny actionButton
primaryButton <- function(...) {
  htmltools::tagQuery(shiny::actionButton(...))$
    removeClass("btn-default")$
    addClass("btn-primary")$
    allTags()
}

#' Create an error message when clicking on Select All or Deselect All when
#'   there is nothing to click on
#' @param select_click a boolean indicating whether either button was clicked
#' @param df_datasets a dataframe that we check if it has 0 rows or not
create_select_error <- function(select_click, df_datasets) {
  if (select_click() & nrow(df_datasets()) == 0) {
    select_error <- paste0(
      "The 'Select All' and 'Deselect All' buttons are for selecting",
      " datasets. Please choose Phenotypes in the tab to the left before",
      " using these buttons. Thank you."
    )
  } else {
    select_error <- ""
  }
  
  select_error  
}

