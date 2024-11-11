


page_sidebar(
  
  title = HTML('<p>Data Explorer <br> <span style="font-size: smaller;"> Tavoni, M. et al. <em> Economic quantification of Loss and Damage funding needs</em>. Nat Rev Earth Environ 5, 411–413 (2024). <a href="https://doi.org/10.1038/s43017-024-00565-7">Link</a></span></p>'),
  window_title = "L&D Data Explorer",
  
  # Display customization
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .js-irs-0 .irs-single, 
      .js-irs-0 .irs-min, 
      .js-irs-0 .irs-max, 
      .js-irs-0 .irs-grid-text {
        display: none;
      }
      ")),
    tags$style(HTML("hr.custom-hr {
        margin-top: 1px;
        margin-bottom: 1px;
      }
      ")),
    tags$script(
      HTML('$(document).ready(function() {
                       $(".navbar .container-fluid")
                         .append("<img id = \'myImage\' src=\'CMCC_Logo_small.png\' align=\'right\' height = \'50px\'>"  );
                      });')),
  ),    
  
  # Input
  sidebar = sidebar(
    # Common inputs
    radioButtons("damage_unit", label = "Unit", choices = c("Billion USD","% GDP")),
    checkboxInput("fixed_fill", label = "Fixed legend",value = T),
    tags$hr(class = "custom-hr"),
    # Damages input (valid for both damages and compensation)
    radioButtons("damage_function",label = "Damage function",choices = levels(damages_by_country$IMP)),
    tags$hr(class = "custom-hr"),
    # Compensation input
    radioButtons("responsibility",label = "Responsibility principle",choices = levels(compensation_by_country$resp)),
    sliderInput("binning_index",
                label = "Columns: ⬅More | Less➡", 
                min = 0.04 ,max = 0.2, step = 0.02,value = 0.16,ticks = F)
    ),
  
  # Output
  card(full_screen = T,
       plotOutput("graph_damages")
  ),
  card(
    full_screen = T,
    plotOutput(("graph_compensation"))
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
