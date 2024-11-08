library(shiny)
library(ggplot2)
library(tidyverse)

source("functions.R")
source("process_data.R")

function(input, output) {
  
  table_damages <- reactive(damages_by_country %>% filter(IMP == input$damage_function))
  output$table_damages <- renderTable({table_damages()})

  table_compensation <- reactive({
    compensation_by_country %>% 
      filter(IMP == input$damage_function,
             resp == input$responsibility)
    })
  output$table_compensation <- renderTable(table_compensation())
  


  # 
  # 
  # output$plot <- renderPlot({
  #   
  #   p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
  #   
  #   if (input$color != 'None')
  #     p <- p + aes_string(color=input$color)
  #   
  #   facets <- paste(input$facet_row, '~', input$facet_col)
  #   if (facets != '. ~ .')
  #     p <- p + facet_grid(facets)
  #   
  #   if (input$jitter)
  #     p <- p + geom_jitter()
  #   if (input$smooth)
  #     p <- p + geom_smooth()
  #   
  #   print(p)
  #   
  # }, height=700)
  
}