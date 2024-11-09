library(shiny)
library(ggplot2)

source("process_data.R")

function(input, output) {
  # Table damages
  table_damages <- reactive(damages_by_country %>% filter(IMP == input$damage_function))
  output$table_damages <- renderTable({table_damages()})
  
  # Table compensation
  table_compensation <- reactive({
    compensation_by_country %>% 
      filter(IMP == input$damage_function,
             resp == input$responsibility)
    })
  output$table_compensation <- renderTable(table_compensation())
  
  # Graph damages
  graph_damages <- reactive(map_damages %>% filter(IMP %in% c(input$damage_function,NA)))
  output$graph_damages <- renderPlot({
    ggplot(data = graph_damages(), aes(fill=damages_percGDP))+
    geom_sf() +
    scale_fill_gradient2(low="#1446A0",mid = "white",high="#DB3069",midpoint=0,na.value = "gray80") +
    theme_void()
  })
  
  # Graph compensation
  graph_compensation <- reactive(map_compensation %>% filter(IMP %in% c(input$damage_function,NA),
                                                        resp == input$responsibility))
  output$graph_compensation <- renderPlot({
    ggplot(data = graph_compensation(), aes(fill=compensation_percGDP))+
      geom_sf() +
      scale_fill_gradient2(low="#1446A0",mid = "white",high="#DB3069",midpoint=0,na.value = "gray80") +
      theme_void()
  })

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