library(shiny)
library(bslib)
library(tidyverse)

survey <- arrow::read_parquet("data/survey.parquet") |> 
  slice_sample(n = 5000, by = region)  

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
    tableOutput("table")
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    
    card(
      plotOutput("histogram")
    ),
    card(
      full_screen = TRUE,
      plotOutput("by_transport")
    ),
    card(
      full_screen = TRUE,
      plotOutput("by_type")
    )
    
  )
  
)

server <- function(input, output, session) {
  filtered <- reactive({
    survey |> 
      filter(region == input$region) |> 
      filter(age <= input$age)
  }) |> 
    bindEvent(input$compute, ignoreNULL = FALSE)
  
  output$table <- renderTable({
    filtered()
  })
  
  output$histogram <- renderPlot({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      theme_light()
  })
  
  output$by_transport <- renderPlot({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      facet_wrap(~transport) +
      theme_light()
  })
  
  output$by_type <- renderPlot({
    filtered() |> 
      ggplot(aes(temps_trajet_en_heures)) +
      geom_histogram(bins = 20) +
      facet_wrap(~type) +
      theme_light()
  })
}

shinyApp(ui, server)
