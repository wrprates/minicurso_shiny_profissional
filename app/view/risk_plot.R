box::use(
  ggplot2[
    ggplot,
    aes,
    geom_bar,
    scale_fill_brewer,
    labs,
    theme,
    theme_minimal,
    element_text
  ],
  dplyr[
    mutate,
    filter
  ],
  shiny[
    moduleServer,
    NS,
    plotOutput,
    renderPlot,
    req
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("risk_plot"))
}

#' @export
server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$risk_plot <- renderPlot({
      req(dados_filtrados())
      
      # Usa a função auxiliar para preparar os dados
      dados_plot <- prepare_risk_plot_data(dados_filtrados())
      
      # Cria o gráfico
      ggplot(dados_plot, aes(x = Age_Group, fill = Credit_Score)) +
        geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = "Distribuição do Risco de Crédito por Faixa Etária",
          x = "Faixa Etária",
          y = "Proporção",
          fill = "Score de Crédito"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
    })
    
  })
}

#' Prepara dados para o gráfico de risco
#' 
#' @description 
#' Função auxiliar que limpa e transforma os dados para o gráfico de risco:
#' - Remove idades inválidas
#' - Converte idade para numérico
#' - Cria faixas etárias
#' 
#' @param dados DataFrame com os dados brutos
#' @return DataFrame processado pronto para plotagem
prepare_risk_plot_data <- function(dados) {
  dados |>
    # Remove idades inválidas
    filter(!grepl("_", Age), Age != "-500") |>
    # Converte para numérico e cria faixas
    mutate(
      Age = as.numeric(Age),
      # Cria faixas etárias de 5 em 5 anos
      Age_Group = cut(
        Age,
        breaks = seq(0, 100, by = 5),
        labels = paste(seq(0, 95, by = 5), seq(5, 100, by = 5), sep="-"),
        include.lowest = TRUE
      )
    )
}
