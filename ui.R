ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      dateRangeInput('date_range', 'Date Range', start = min(data$date), end = max(data$date),
                     min =  min(data$date), max = max(data$date)),
      selectInput("level", "Level", choices = c("National" = "national", 
                                                "State" = "state"),
                  selected = 'national'),
      conditionalPanel("input.level == 'state'",
                       selectInput("state", "State", choices = state_names)),
      selectizeInput("variables", "Variables", choices = pretty_names_select, multiple = TRUE, selected = 'daily_deaths')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotly::plotlyOutput("plot")
      
    )
  )
)