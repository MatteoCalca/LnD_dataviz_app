


fluidPage(
  
  titlePanel("L&D fund - Data Explorer"),
  
  tags$style(HTML("
    .js-irs-0 .irs-single, 
    .js-irs-0 .irs-min, 
    .js-irs-0 .irs-max, 
    .js-irs-0 .irs-grid-text {
      display: none;
    }
  ")),
  
  sidebarPanel(
    width=2,
    # Common inputs
    radioButtons("damage_unit", label = "Unit", choices = c("Billion USD","% GDP")),
    checkboxInput("fixed_fill", label = "Fixed legend",value = T),
    tags$hr(),
    # Damages input (valid for both damages and compensation)
    radioButtons("damage_function",label = "Damage function",choices = levels(damages_by_country$IMP)),
    tags$hr(),
    # Compensation input
    radioButtons("responsibility",label = "Responsibility principle",choices = levels(compensation_by_country$resp)),
    sliderInput("binning_index",
                label = "Columns: ⬅ More | Less ➡", 
                min = 0.04 ,max = 0.3, step = 0.02,value = 0.16,ticks = F)
    ),
  
  mainPanel(  
    #tabsetPanel(
      # tabPanel("Compensation table",
      #          tableOutput("table_compensation")),
      # tabPanel("Damages table",
      #          tableOutput("table_damages")),
      # tabPanel("Compensation graph",
      #          plotOutput("graph_compensation")),
      #tabPanel("Damages graph",
               plotOutput("graph_damages"),
               plotOutput(("graph_compensation"))
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