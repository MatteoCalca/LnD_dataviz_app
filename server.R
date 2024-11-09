library(shiny)
library(ggplot2)

source("process_data.R")

function(input, output) {

  # Graph damages
  graph_damages <- reactive(map_damages %>% filter(IMP %in% c(input$damage_function,NA)))
  target_variable <- reactive({case_when(input$damage_unit == "% GDP" ~ "damages_percGDP",
                               input$damage_unit == "Billion USD" ~ "damn")})
  #target_variable_max <- 
  output$graph_damages <- renderPlot({
  p <- 
      ggplot(data = graph_damages(), aes(fill=.data[[target_variable()]]))+
    geom_sf() +
    theme_void() +
    labs(title = paste0("Damages: ",input$damage_function, " (",input$damage_unit,")"),fill="")

    if (input$fixed_fill == T & input$damage_unit == "% GDP") {
      p <- p + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
                                    label = scales::label_percent(),
                                    limits = c(min(map_damages$damages_percGDP,na.rm = T), max(map_damages$damages_percGDP,na.rm = T)))
      
    } else if(input$fixed_fill == F & input$damage_unit == "% GDP") {
      p <- p + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
                                    label = scales::label_percent())
      
    } else if(input$fixed_fill == T & input$damage_unit == "Billion USD") {
      p <- p + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
                                    limits = c(min(map_damages$damn,na.rm = T), max(map_damages$damn,na.rm = T)))
      
    } else if(input$fixed_fill == F & input$damage_unit == "Billion USD") {
      p <- p + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80")
    }
    
    print(p)
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
  
  # # Table damages
  # table_damages <- reactive(damages_by_country %>% filter(IMP == input$damage_function))
  # output$table_damages <- renderTable({table_damages()})
  # 
  # # Table compensation
  # table_compensation <- reactive({
  #   compensation_by_country %>% 
  #     filter(IMP == input$damage_function,
  #            resp == input$responsibility)
  # })
  # output$table_compensation <- renderTable(table_compensation())
  # 
  # # Graph compensation
  # graph_compensation <- reactive(map_compensation %>% filter(IMP %in% c(input$damage_function,NA),
  #                                                            resp == input$responsibility))
  # output$graph_compensation <- renderPlot({
  #   ggplot(data = graph_compensation(), aes(fill=compensation_percGDP))+
  #     geom_sf() +
  #     scale_fill_gradient2(low=color_palette$map[["low"]],
  #                          mid = color_palette$map[["mid"]],
  #                          high=color_palette$map[["high"]],
  #                          midpoint=0,
  #                          na.value = "gray80") +
  #     theme_void()
  # })
  
}