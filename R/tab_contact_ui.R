tab_contact_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h1(
      class = "page-header",
      id = ns("title"),
      "Contact & Contribute"
    ),

    shiny::p(
      "For questions, comments, or to contribute to",
      " MIAMI-AD, you can contact us at:"),
    shiny::p(
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
    )
  )
}
