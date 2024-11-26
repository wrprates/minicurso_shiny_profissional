box::use(
  bslib[
    bs_theme,
    card,
    card_body,
    layout_column_wrap,
    nav_panel,
    page_navbar,
    sidebar
  ],
  dplyr[
    filter,
  ],
  shiny[
    div,
    moduleServer,
    NS,
    observe,
    observeEvent,
    tags,
    updateSelectInput,
    selectInput,
    sliderInput,
  ]
)

box::use(
  app / view / risk_plot,
  app / view / usage_plot,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # Tema personalizado
  custom_theme <- bs_theme(
    bootswatch = "cosmo",
    # primary = "#f4f4f4", #nolint
    # font_scale = 1 #nolint
  )

  # Conteúdo principal com navbar e sidebar
  page_navbar(
    title = "Meu App",
    theme = custom_theme,
    
    # Barra lateral esquerda
    sidebar = sidebar(
      title = "Filtros",

        # Filtro de Mês
      selectInput(
        ns("month"),
        "Período de Análise",
        choices = c(
          "January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August"
        ),
        selected = c("July", "August"),
        multiple = TRUE
      ),
      
      # Filtro de Faixa Etária
      sliderInput(
        ns("age_range"),
        "Faixa Etária",
        min = 0,
        max = 100,
        value = c(20, 60)
      ),
      
      # Filtro de Ocupação
      selectInput(
        ns("occupation"),
        "Ocupação",
        choices = NULL,
        multiple = TRUE
      ),
      
      # Filtro de Tipo de Crédito
      selectInput(
        ns("loan_type"),
        "Tipo de Crédito",
        choices = NULL,
        multiple = TRUE
      )
    ),
    
    # Painéis de navegação
    nav_panel("Risco de Crédito", 
      layout_column_wrap(
        width = 1/2,
        card(
          card_body(
            risk_plot$ui(ns("risk_plot")),
            title = "Distribuição do Risco de Crédito por Faixa Etária"
          )
        ),
        card(
          card_body(
            usage_plot$ui(ns("usage_plot")),
            title = "Relação Entre Taxa de Utilização e Atraso"
          )
        )
      )
    ),
    nav_panel("Página 2",
      div("Conteúdo da página 2")
    )
  )
}

#' @export
server <- function(id, dados, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectInput(
        session,
        "occupation",
        choices = unique(dados$Occupation) |> sort()
      )
    })

    observe({
      updateSelectInput(
        session,
        "loan_type",
        choices = unique(dados$Type_of_Loan) |> sort()
      )
    })

    # Observa mudanças em qualquer filtro
    observeEvent(list(
      input$month,
      input$age_range,
      input$occupation,
      input$loan_type
    ), {
      dados_filtrados(
        prepare_filter_data(
          dados,
          month = input$month,
          age_range = input$age_range,
          occupation = input$occupation,
          loan_type = input$loan_type
        )
      )
    })

    risk_plot$server("risk_plot", dados_filtrados)
    usage_plot$server("usage_plot", dados_filtrados)
  })
}

#' Prepara dados aplicando filtros selecionados
#' 
#' @param dados DataFrame com dados brutos
#' @param month Vetor de meses selecionados
#' @param age_range Vetor com range de idade (min, max)
#' @param occupation Vetor de ocupações selecionadas
#' @param loan_type Vetor de tipos de empréstimo selecionados
#' @return DataFrame filtrado
#' @export 
prepare_filter_data <- function(
  dados,
  month = NULL,
  age_range = NULL,
  occupation = NULL,
  loan_type = NULL
) {
  dados_filtrados <- dados
  
  # Filtro de mês
  if (!is.null(month) && length(month) > 0) {
    dados_filtrados <- dados_filtrados |>
      filter(Month %in% month)
  }
  
  # # Filtro de faixa etária
  if (!is.null(age_range) && length(age_range) == 2) {
    dados_filtrados <- dados_filtrados |>
      filter(
        Age >= age_range[1],
        Age <= age_range[2]
      )
  }
  
  # Filtro de ocupação
  if (!is.null(occupation) && length(occupation) > 0) {
    dados_filtrados <- dados_filtrados |>
      filter(Occupation %in% occupation)
  }
  
  # Filtro de tipo de empréstimo
  if (!is.null(loan_type) && length(loan_type) > 0) {
    dados_filtrados <- dados_filtrados |>
      filter(Type_of_Loan %in% loan_type)
  }
  
  dados_filtrados
}
