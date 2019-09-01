library(shiny)
library(tidyverse)
library(folhar2)

media_movel <- function(x, n) {
  c(rep(NA, n - 1), diff(c(0, cumsum(x)), n)) / n
}

buscar <- function(termo, inicio) {
  print(glue::glue("[{Sys.time()}] consulta de '{termo}' chamada."))
  res <- folha_buscar(termo, inicio)
  print(
    glue::glue("[{Sys.time()}] consulta recebida com {nrow(res)} resultados.")
  )
  res
}

limpar_termo <- function(termo) {
  termo %>% 
    str_to_lower() %>% 
    stringi::stri_trans_general("Latin-ASCII")
}

ui <- fluidPage(
  fluidRow(
    column(
      4,
      textInput("termo1", "Termo de busca", "Recessão"),
      textInput("termo2", "Termo de busca", "Recuperação"),
      selectInput("periodo", "Período de análise", selected = 12,
                  list("1 mês" = 12, "3 meses" = 4, "6 meses" = 2 )),
      actionButton("buscar", "Buscar!", icon("search"))
    ),
    column(
      8,
      plotOutput("evolucao")
    )
  )
)

server <- function(input, output, session) {
  termo1 <- reactiveVal("")
  termo2 <- reactiveVal("")
  periodo <- reactiveVal(0)
  resposta1 <- reactiveVal(mtcars)
  resposta2 <- reactiveVal(mtcars)

  observeEvent(input$buscar, {
    inicio <- Sys.Date() %>%
      magrittr::subtract(lubridate::dyears(1)/as.integer(input$periodo) + 7) %>%
      format(format = "%d/%m/%Y")

    if (input$termo1 != termo1() | input$periodo != periodo()) {
      input$termo1 %>% 
        limpar_termo() %>% 
        buscar(inicio) %>% 
        resposta1()
      termo1(input$termo1)
    }

    if (input$termo2 != termo2() | input$periodo != periodo()) {
      input$termo2 %>% 
        limpar_termo() %>% 
        buscar(inicio) %>% 
        resposta2()
      termo2(input$termo2)
    }

    if (input$periodo != periodo()) {
      periodo(input$periodo)
    }
  })

  output$evolucao <- renderPlot({
    validate(
      need(input$buscar, "Aperte o botão para gerar o relatório")
    )

    resposta1() %>%
      bind_rows(resposta2(), .id = "termo") %>%
      mutate(termo = c(termo1(), termo2())[as.numeric(termo)],
             hora = stringr::str_remove(hora, "º"),
             dia = stringr::str_extract(hora, "\\d{1,2}\\.[a-z]{3}\\.\\d{4}"),
             dia = lubridate::dmy(dia, locale = "pt_BR.UTF-8")) %>%
      count(termo, dia) %>%
      complete(dia = full_seq(dia, 1), termo, fill = list(n = 0)) %>%
      group_by(termo) %>%
      mutate(n = media_movel(n, 7)) %>%
      filter(!is.na(n)) %>%
      ggplot(aes(dia, n, col = termo)) +
      geom_line(size = 2) +
      scale_y_continuous("Quantidade", limits = c(0, NA)) +
      ggtitle("Média móvel de 7 dias de notícias da Folha contendo os termos")
  })
}

shinyApp(ui, server)
