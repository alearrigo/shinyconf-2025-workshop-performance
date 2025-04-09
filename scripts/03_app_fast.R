library(shiny)
library(bslib)
library(tidyverse)
library(future)
library(promises)

plan(multisession)

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
    input_task_button(
      id = "compute", 
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
  filter_task <- ExtendedTask$new(
    memoise::memoise(function(p_survey, p_region, p_age) {
      future_promise({
        p_survey |> 
          dplyr::filter(region == p_region) |> 
          dplyr::filter(age <= p_age)
      })
    }, cache = getShinyOption("cache"))
  ) |> 
    bind_task_button("compute")
  
  observe(filter_task$invoke(survey, input$region, input$age)) |> 
    bindEvent(input$compute, ignoreNULL = FALSE)
  
  filtered <- reactive({
    filter_task$result()
  })
  
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
