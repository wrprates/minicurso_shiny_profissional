box::use(
  readr[read_csv],
  shiny[
    div,
    moduleServer, 
    NS,
    reactiveVal,
    tags,
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
    dados <- read_csv("data/train.csv")

    dados_filtrados <- reactiveVal(NULL)

    view$server("view", dados, dados_filtrados)
  })
}