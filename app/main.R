box::use(
  shiny[
    div,
    moduleServer, 
    NS,
    tags
  ]
)

box::use(
  app / view 
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  view$ui(ns("view"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    view$server("view")
  })
}