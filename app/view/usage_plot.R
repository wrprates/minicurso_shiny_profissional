box::use(
  echarts4r[
    echarts4rOutput, 
    e_charts, 
    e_scatter, 
    renderEcharts4r,
    e_x_axis,
    e_y_axis,
    e_title
  ],
  dplyr[mutate, filter, group_by, summarise, n],
  shiny[moduleServer, NS, req]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  echarts4rOutput(ns("usage_plot"))
}

#' @export
server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$usage_plot <- renderEcharts4r({
      req(dados_filtrados())
      
      dados_plot <- prepare_usage_plot_data(dados_filtrados())
      
      dados_plot |>
        e_charts(util_group) |>
        e_scatter(avg_delay, size = n) |>
        e_x_axis(
          min = 15, 
          max = 45,
          name = "Taxa de Utilização do Crédito (%)"
        ) |>
        e_y_axis(
          min = 15, 
          max = 25,
          name = "Dias de Atraso"
        ) |>
        e_title("Gráfico de Utilização do Crédito")
    })
    
  })
}

#' Prepara dados para o gráfico de utilização
#' @param dados DataFrame com os dados brutos
#' @return DataFrame processado pronto para plotagem
prepare_usage_plot_data <- function(dados) {
  dados |>
    # Limpa e converte dados
    filter(
      !grepl("_", Credit_Utilization_Ratio),
      !grepl("_", Delay_from_due_date)
    ) |>
    mutate(
      Credit_Utilization_Ratio = as.numeric(Credit_Utilization_Ratio),
      Delay_from_due_date = as.numeric(Delay_from_due_date),
      # Agrupa taxa de utilização em faixas de 5%
      util_group = floor(Credit_Utilization_Ratio / 5) * 5
    ) |>
    # Remove NAs
    filter(
      !is.na(Credit_Utilization_Ratio),
      !is.na(Delay_from_due_date)
    ) |>
    # Agrega por faixa de utilização
    group_by(util_group) |>
    summarise(
      n = n(),  # número de casos
      avg_delay = mean(Delay_from_due_date)  # atraso médio
    )
}
