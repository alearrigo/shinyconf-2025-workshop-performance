library(shiny)
library(bslib)
library(tidyverse)

survey <- arrow::read_parquet("data/survey.parquet")

ui <- page_sidebar(
  
  sidebar = sidebar(
    selectInput(
      inputId = "region",
      label = "Seleccione region",
      choices = discard(unique(survey$region), is.na)
    ),
    sliderInput(
      inputId = "age",
      label = "Seleccione edad máxima",
      min = 10,
      max = 100,
      value = 100,
      step = 10
    ),
    actionButton(
      inputId = "compute", 
      label = "Calcular"
    )
  ),
  
  useBusyIndicators(),
  
  card(
    max_height = "50%",
    DT::DTOutput("table")
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    
    card(
      plotly::plotlyOutput("histogram")
    ),
    card(
      full_screen = TRUE,
      plotly::plotlyOutput("by_transport")
    ),
    card(
      full_screen = TRUE,
      plotly::plotlyOutput("by_type")
    )
    
  )
  
)

server <- function(input, output, session) {
  filtered <- reactive({
    survey |> 
      filter(region == input$region) |> 
      filter(age <= input$age)
  }) |> 
    bindCache(input$region, input$age) |> 
    bindEvent(input$compute, ignoreNULL = FALSE)
  
  output$table <- DT::renderDT({
    filtered()
  })
  
  output$histogram <- plotly::renderPlotly({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      theme_light()
  })
  
  output$by_transport <- plotly::renderPlotly({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      facet_wrap(~transport) +
      theme_light()
  })
  
  output$by_type <- plotly::renderPlotly({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      facet_wrap(~type) +
      theme_light()
  })
}

shinyApp(ui, server)
