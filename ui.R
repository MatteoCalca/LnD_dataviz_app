library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(
  
  titlePanel("L&D fund - Data Explorer"),

  sidebarPanel(
    radioButtons("damage_function",label = "Select a damage function",choices = unique(damages_by_country$IMP)),
    radioButtons("responsibility",label = "Select a responsibility principle",choices = unique(compensation_by_country$resp))),  
  
  mainPanel(  
    tabsetPanel(
      tabPanel("Compensation table",
               tableOutput("table_compensation")),
      tabPanel("Damages table",
               tableOutput("table_damages"))
    )
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