tab_contribute_UI <- function(id){
  shiny::tagList(
    shiny::column(width=8, shiny::h2(shiny::tags$b("Contribute Your Data"))),
    shiny::column(
      width=10,
      shiny::p("We welcome people to contribute their data to this project.",
               "Instructions for uploading your will be available here soon.")
    )
  )
}
