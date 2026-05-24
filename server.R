library(shiny)
library(dplyr)

server <- function(input, output, session) {
  
  search_results <- eventReactive(input$search, {
    
    find_round_trips_from_kirkwall(
      data = all_timetables_filtered,
      selected_date = input$date,
      selected_destination = input$destination
    )
    
  })
  
  output$results <- renderTable({
    
    results <- search_results()
    
    if (nrow(results) == 0) {
      return(data.frame(
        Message = "No available delivery options found for this date and destination."
      ))
    }
    
    results
  })
}