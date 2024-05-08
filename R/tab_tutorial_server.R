tab_tutorial_server <- function(id, common) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    output$figure1 <- shiny::renderImage({
      list(src = "inst/shiny/www/tutorial_figure_1.jpg",
           alt = "Missing Figure"
      )
    },
    deleteFile = FALSE)
    
    output$figure2 <- shiny::renderImage({
      list(src = "inst/shiny/www/tutorial_figure_2.jpg",
           alt = "Missing Figure"
      )
    },
    deleteFile = FALSE)
    
    output$figure3 <- shiny::renderImage({
      list(src = "inst/shiny/www/tutorial_figure_3.jpg",
           alt = "Missing Figure"
      )
    },
    deleteFile = FALSE)
    
    output$figure4 <- shiny::renderImage({
      list(src = "inst/shiny/www/tutorial_figure_4.jpg",
           alt = "Missing Figure"
      )
    },
    deleteFile = FALSE)
  })
}
