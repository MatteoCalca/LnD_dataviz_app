


function(input, output) {

  # Graph damages
  target_variable_damages <- reactive({case_when(input$damage_unit == "% GDP" ~ "damages_percGDP",
                                                 input$damage_unit == "Billion USD" ~ "damn")})
  graph_damages <- reactive(map_damages %>% filter(IMP %in% c(input$damage_function,NA)))
  output$graph_damages <- renderPlot({
      graph_damages_plot <- ggplot(data = graph_damages(), aes(fill=.data[[target_variable_damages()]]))+
        geom_sf() +
        theme_void() +
        labs(title = paste0("Damages: ",input$damage_function, " (",input$damage_unit,")"),fill="") +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              legend.text = element_text(size = 14))
    
        if (input$fixed_fill == T & input$damage_unit == "% GDP") {
          graph_damages_plot <- graph_damages_plot + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
              label = scales::label_percent(),
              limits = c(min(map_damages$damages_percGDP,na.rm = T), max(map_damages$damages_percGDP,na.rm = T)))
          
        } else if(input$fixed_fill == F & input$damage_unit == "% GDP") {
          graph_damages_plot <- graph_damages_plot + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
              label = scales::label_percent())
          
        } else if(input$fixed_fill == T & input$damage_unit == "Billion USD") {
          graph_damages_plot <- graph_damages_plot + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80",
              limits = c(min(map_damages$damn,na.rm = T), max(map_damages$damn,na.rm = T)))
          
        } else if(input$fixed_fill == F & input$damage_unit == "Billion USD") {
          graph_damages_plot <- graph_damages_plot + scale_fill_gradient2(low=color_palette$map[["low"]], mid = color_palette$map[["mid"]], high=color_palette$map[["high"]], midpoint=0, na.value = "gray80")
        }
        
    print(graph_damages_plot)
  })
  
  # Graph compensation
  target_variable_compensation <- reactive({case_when(input$damage_unit == "% GDP" ~ "compensation_percGDP",
                                                 input$damage_unit == "Billion USD" ~ "comp")})
  binning_index_transform <- reactive((input$binning_index-10)^2+20*input$binning_index-100)
  graph_compensation <- reactive(compensation_by_country %>% 
                                   filter(IMP %in% c(input$damage_function,NA),
                                          resp == input$responsibility) %>%
                                   mutate(to_be_binned = if_else(abs(comp) < binning_index_transform() * sum(abs(comp)),
                                                                 if_else(sign(comp) == 1,"binned_positive","binned_negative"),
                                                                 "not_binned"),
                                          n = case_when(to_be_binned == "not_binned" ~ n,
                                                        to_be_binned == "binned_positive" ~ "Other recipients",
                                                        to_be_binned == "binned_negative" ~ "Other donors")) %>% 
                                   group_by(n) %>% 
                                   summarise(comp = sum(comp), gni = sum(gni,na.rm = T)) %>% 
                                   mutate(compensation_percGDP = (comp/1000) / (gni*100)))
  output$graph_compensation <- renderPlot({
    graph_compensation_plot <- ggplot(data = graph_compensation(),
             aes(x=reorder(n,.data[[target_variable_compensation()]]),
                 y=.data[[target_variable_compensation()]],
                 fill=.data[[target_variable_compensation()]]))+
      geom_col(position = "dodge",color="black",linewidth=0.2,width = 0.5) +
      geom_hline(yintercept = 0) +
      theme_void() +
      theme(axis.text.x = element_text(color = "gray20",angle = 45,hjust = 1,vjust = 1),
            axis.text.y = element_text(color = "gray20", size = 14),
            panel.grid.major.x  = element_line(color = "gray90"),
            legend.title = element_blank(),
            plot.title = element_text(size = 20, hjust = 0.5),
            legend.text = element_text(size = 14),
            plot.margin = unit(c(1,1,1,1),"cm"))+
      labs(title = paste0("Compensation: ",input$damage_function, " (",input$damage_unit,"), ",input$responsibility))+
      scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80")
      
    if (input$fixed_fill == T & input$damage_unit == "% GDP") {
      graph_compensation_plot <- graph_compensation_plot + scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80",
                                                                      label = scales::label_percent(),
                                                                      limits = c(min(compensation_by_country$compensation_percGDP,na.rm = T), max(compensation_by_country$compensation_percGDP,na.rm = T))) +
                                                           scale_y_continuous(
                                                                      label = scales::label_percent(),
                                                                      limits = c(min(compensation_by_country$compensation_percGDP,na.rm = T), max(compensation_by_country$compensation_percGDP,na.rm = T)))
      
    } else if(input$fixed_fill == F & input$damage_unit == "% GDP") {
      graph_compensation_plot <- graph_compensation_plot + scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80",
                                                                      label = scales::label_percent()) +
                                                           scale_y_continuous(
                                                                      label = scales::label_percent())
              
    } else if(input$fixed_fill == T & input$damage_unit == "Billion USD") {
      graph_compensation_plot <- graph_compensation_plot + scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80",
                                                                      limits = c(min(compensation_by_country$comp,na.rm = T), max(compensation_by_country$comp,na.rm = T))) +
                                                           scale_y_continuous(
                                                                      limits = c(min(compensation_by_country$comp,na.rm = T), max(compensation_by_country$comp,na.rm = T)))
      
    } else if(input$fixed_fill == F & input$damage_unit == "Billion USD") {
      graph_compensation_plot <- graph_compensation_plot + scale_fill_gradient2(low=color_palette$bars[["low"]], mid = color_palette$bars[["mid"]], high=color_palette$bars[["high"]], midpoint=0, na.value = "gray80")
    }
  
    print(graph_compensation_plot)
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