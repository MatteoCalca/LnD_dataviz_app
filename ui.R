library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(
  
  titlePanel("L&D fund - Data Explorer"),

  sidebarPanel(
    radioButtons("damage_function",label = "Damage function",choices = unique(damages_by_country$IMP)),
    radioButtons("damage_unit", label = "Unit of damages", choices = c("% GDP","Billion USD")),
    checkboxInput("fixed_fill", label = "Fixed colors across\ndamage function",value = F),
    radioButtons("responsibility",label = "Responsibility principle",choices = unique(compensation_by_country$resp))),
  
  mainPanel(  
    #tabsetPanel(
      # tabPanel("Compensation table",
      #          tableOutput("table_compensation")),
      # tabPanel("Damages table",
      #          tableOutput("table_damages")),
      # tabPanel("Compensation graph",
      #          plotOutput("graph_compensation")),
      #tabPanel("Damages graph",
               plotOutput("graph_damages")
               #)
    #)
  )


    # sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
    #             value=min(1000, nrow(dataset)), step=500, round=0),
    # 
    # selectInput('x', 'X', names(dataset)),
    # selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    # selectInput('color', 'Color', c('None', names(dataset))),
    # 
    # checkboxInput('jitter', 'Jitter'),
    # checkboxInput('smooth', 'Smooth'),
    # 
    # selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    # selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  # ),
  # 
  # mainPanel(
  #   plotOutput('plot')
  # )
)