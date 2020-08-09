server <- function(input, output, session) {
  
  plot_data <- reactive({
    if (input$level == 'national') {
      covid_plot_data(input$level, variables = input$variables, date_start = input$date_range[[1]], date_end = input$date_range[[2]])
    } else if (input$level == 'state') {
      covid_plot_data(input$level, variables = input$variables, date_start = input$date_range[[1]], date_end = input$date_range[[2]], state =  input$state)
    }
  })
  
  output$plot <- plotly::renderPlotly({
    covid_plot(plot_data(), input$variables)
  })
  
}