#' Initialize an empty dataframe to store dataset plotting values in
#'
#' @param source a boolean whether to store the dataset source
#' @param metric a boolean about whether to have a p-value cutoff (True), or a checkbox on whether it can be selected (FALSE)
create_empty_dataframe <- function(source = FALSE, metric = FALSE) {
  # create dataframe
  df <- data.frame(
    Dataset = character(),
    Description = character(),
    Author = character(),
    Year = integer(),
    PMID = character(),
    PMID_Excel = character(),
    Full_EWAS = character()
  )

  # add extra columns as needed
  if (source){
    df <- df %>%
      dplyr::mutate(Source = character())
  }
  df <- df %>%
    dplyr::mutate(Select_Bool = logical(), Select = character())
  if (metric){
    df <- df %>%
      dplyr::mutate(
        Metric_Text = character(), Metric = character(),
        Filter_Text = character(), Filter = character(),
        Threshold = numeric()
        )
  }

  return(df)
}

#' Initialize an empty dataframe to store epigenetic data in
create_epigenetic_empty_dataframe <- function() {
  # create base table
  df <- data.frame(
    Epigenetic_Clock = character(),
    Description = character(),
    Author = character(),
    Year = integer(),
    PMID = character(),
    PMID_Excel = character()
  )

  return(df)
}

create_empty_reactive_table <- function(source = FALSE, metric = FALSE){
  # create dataframe
  df <- create_empty_dataframe(source=source, metric = metric)

  # convert to a reactive value
  df_datasets <- shiny::reactiveVal(df)

  return (df_datasets)
}

create_epigenetic_empty_reactive <- function(){
  # create base table
  df <- create_epigenetic_empty_dataframe()

  # convert to a reactive value
  df_datasets <- shiny::reactiveVal(df)

  return (df_datasets)
}

create_epigenetic_reactive_table <- function(df_family_labels){
  # Create base table
  df <- df_family_labels %>%
    dplyr::mutate(PMID_Excel = "", Select_Bool = FALSE, Select = "") %>%
    dplyr::mutate(Year = as.integer(.data$Year), PMID = as.character(.data$PMID)) %>%
    dplyr::select(
      "Epigenetic_Clock", "Description", "Author", "Year", "PMID",
      "PMID_Excel", "Select_Bool", "Select")

  # fix PMID's
  df <- df %>%
    dplyr::mutate(PMID_Excel = create_PMID_Link(.data$PMID, excel=TRUE),
                  PMID = create_PMID_Link(.data$PMID, excel=FALSE)
                  )

  # add checkboxes
  df$Select <- create_plotting_checkboxes(df$Select_Bool)

  # return a reactive
  df_datasets <- shiny::reactiveVal(df)

  return (df_datasets)
}

update_datasets_table <- function(
    df_datasets, df_labels, source=FALSE, has_cpg = TRUE, metric = FALSE
    ){
  # Filter df_labels to the relevant columns
  if (source){
    columns <- c("Dataset", "Description", "Author", "Year", "PMID", "Source", "Full_EWAS")
  } else {
    columns <- c("Dataset", "Description", "Author", "Year", "PMID", "Full_EWAS")
  }

  if (has_cpg) {
    df_labels <- df_labels %>%
      dplyr::filter(.data$CpG_Data == "Yes")
  }

  df_labels <- df_labels[,columns] %>%
    dplyr::distinct()

  # define target data and display data
  df_data_update <- df_datasets()

  if (source){
    df_labels <- df_labels %>%
      dplyr::mutate(Target = paste0(.data$Dataset, " ; ", .data$Source))
    df_data_update <- df_data_update %>%
      dplyr::mutate(Target = paste0(.data$Dataset, " ; ", .data$Source))
  } else {
    df_labels <- df_labels %>%
      dplyr::mutate(Target = .data$Dataset)
    df_data_update <- df_data_update %>%
      dplyr::mutate(Target = .data$Dataset)
  }

  # Remove unwanted rows
  df_data_update <- df_data_update %>%
    dplyr::filter(.data$Target %in% df_labels$Target)

  # Add new datasets
  df_miss <- df_labels %>%
    dplyr::filter(!(.data$Target %in% df_data_update$Target)) %>%
    dplyr::mutate(PMID_Excel = create_PMID_Link(.data$PMID, excel=TRUE),
                  PMID = create_PMID_Link(.data$PMID, excel=FALSE),
                  Select_Bool = FALSE,
                  Select = "",
                  Metric_Text = "pValue",
                  Metric = "",
                  Filter_Text = "< than",
                  Filter = "",
                  Threshold = 1e-5)
  if (metric && source){
    df_miss <- df_miss %>%
      dplyr::select(
        "Dataset", "Description", "Author", "Year", "PMID",
        "PMID_Excel", "Source", "Full_EWAS", "Select_Bool", "Select",
        "Metric_Text", "Metric", "Filter_Text", "Filter", "Threshold"
      )
  } else if (source){
    df_miss <- df_miss %>%
      dplyr::select(
        "Dataset", "Description", "Author", "Year", "PMID",
        "PMID_Excel", "Source", "Full_EWAS", "Select_Bool", "Select")
  } else {
    df_miss <- df_miss %>%
      dplyr::select(
        "Dataset", "Description", "Author", "Year", "PMID",
        "PMID_Excel", "Full_EWAS", "Select_Bool", "Select")
  }

  df_data_update <- df_data_update %>%
    dplyr::select(-"Target")

  df_data_update <- rbind(df_data_update, df_miss)

  # sort by year and dataset
  df_data_update <- df_data_update %>%
    dplyr::arrange(dplyr::desc(.data$Year), .data$Dataset)

  # Update Plotting Checkboxes
  df_data_update$Select <- create_plotting_checkboxes(
    df_data_update$Select_Bool)
  
  # Update Radio Buttons
  if (metric) {
    df_data_update$Metric <- create_plotting_buttons(
      df_data_update$Metric_Text, button = "Metric", name = "radiom"
    )
    df_data_update$Filter <- create_plotting_buttons(
      df_data_update$Filter_Text, button = "Filter", name = "radiof"
    )    
  }

  # update dataset table
  df_datasets(df_data_update)
}

create_plotting_checkbox <- function(index, toplot = TRUE, name = "checkb") {

  result <- as.character(shiny::checkboxInput(
    inputId = paste0(name, index),
    label = NULL,
    value = toplot,
    width = "20px"
    ))

  return(result)
}

create_plotting_checkboxes <- function(toplots, name="checkb") {
  results <- character(length(toplots))

  for (index in 1:length(toplots)){
    results[index] <- create_plotting_checkbox(
      index, toplot = toplots[index], name = name)
  }

  return(results)
}

create_plotting_button <- function(
    index, name = "radiob", selection, choices = c("pValue", "FDR")) {

  result <- as.character(shiny::radioButtons(
    inputId = paste0(name, index),
    label = NULL,
    choices = choices,
    selected = selection,
    inline = FALSE,
    width = "50px"
    ))

  return(result)
}

create_plotting_buttons <- function(
    selections, button = "Metric", name = "radiob"){
  results <- character(length(selections))
  if (button == "Metric") {
    choices <- c("pValue", "FDR")
  }
  if (button == "Filter") {
    choices <- c("< than", "> than")
  }

  for (index in 1:length(selections)){
    results[index] <- create_plotting_button(
      index, name = name, selection = selections[index], choices = choices)
  }

  return(results)
}

fill_plotting_table <- function(df_datasets, selection=TRUE){
  # get plotting dataframe
  df_data_update <- df_datasets()

  # set all checkboxest to 1 value
  df_data_update$Select_Bool <- selection
  df_data_update$Select <- create_plotting_checkboxes(df_data_update$Select_Bool)

  # update dataset table
  df_datasets(df_data_update)
}

#' create the javascript code for a checkbox-button in a data frame so that it
#' returns information when selected
#'
#' @param dtid the dataframe id used as an identifier
#' @param ns The NS function to add the shiny module id to the label
#' @param column the column number to return when selected
#' @param name the name identifier to use for the button
checkbox_js <- function(
    dtid = data.frame(), ns = shiny::NS(NULL),
    column = 1, name = "checkb") {
  js_text <- c(
    sprintf("$('[id^=%s]').on('click', function(){", name),
    "  var id = this.getAttribute('id');",
    sprintf("  var i = parseInt(/%s(\\d+)/.exec(id)[1]);", name),
    "  var value = $(this).prop('checked');",
    sprintf("  var info = [{row: i, col: %d, value: value}];", column),
    sprintf(
      "Shiny.setInputValue('%s', info);",
      ns(sprintf("%s_cell_edit:DT.cellInfo", dtid))
    ),
    "})"
  )

  return(js_text)
}

#' create the javascript code for a radio-button in a data frame so that it
#' returns information when selected
#'
#' @param dtid the dataframe id used as an identifier
#' @param ns The NS function to add the shiny module id to the label
#' @param column the columns of the dataframe corresponding to the checkboxes
#' @param label the names used for the checkboxes
radio_js <- function(
    dtid = data.frame(), ns = shiny::NS(NULL),
    column = 1, label = "radiob") {
  js_text <- c(
    sprintf("$('[id^=%s]').on('click', function(){", label),
    "  var id = this.getAttribute('id');",
    sprintf("  var i = parseInt(/%s(\\d+)/.exec(id)[1]);", label),
    "  var value = $(this).find('input:checked')[0].value;",
    sprintf("  var info = [{row: i, col: %d, value: value}];", column),
    sprintf(
      "Shiny.setInputValue('%s', info);",
      ns(sprintf("%s_cell_edit:DT.cellInfo", dtid))
    ),
    "})"
  )
  
  return(js_text)
}

multi_js <- function(
    dtid = data.frame(), ns = shiny::NS(NULL), columns = numeric(),
    labels = character(), button_types = character()
) {
  js_text <- c()
  
  for (index in 1:length(columns)) {
    button_type <- button_types[[index]]
    column <- columns[[index]]
    label <- labels[[index]]
    
    if (button_type == "radio") {
      js_text <- c(
        js_text,
        radio_js(dtid = dtid, ns = ns, column = column, label = label)
      )
    } else if (button_type == "check") {
      js_text <- c(
        js_text,
        checkbox_js(dtid = dtid, ns = ns, column = column, name = label)
      )
    }
  }
  
  return(js_text)
}