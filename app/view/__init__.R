box::use(
  bslib[
    bs_theme,
    nav_panel,
    page_navbar,
    sidebar
  ],
  shiny[
    div,
    moduleServer, 
    NS,
    tags
  ]
)

box::use(
  app / logic
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # Tema personalizado
  custom_theme <- bs_theme(
    bootswatch = "cosmo",
    primary = "#f4f4f4",
    font_scale = 1
  )

  # Conteúdo principal com navbar e sidebar
  page_navbar(
    title = "Meu App",
    theme = custom_theme,
    
    # Barra lateral esquerda
    sidebar = sidebar(
      title = "Filtros",
      div("Conteúdo da barra lateral")
    ),
    
    # Painéis de navegação
    nav_panel("Página 1", 
      div("Conteúdo da página 1")
    ),
    nav_panel("Página 2",
      div("Conteúdo da página 2")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Lógica do servidor aqui
  })
}
