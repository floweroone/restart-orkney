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
    
    display_results <- results
    
    display_names <- c(
      day = "Day",
      outbound_departure = "Outbound Departure",
      outbound_arrival = "Outbound Arrival",
      return_departure = "Return Departure",
      return_arrival = "Return Arrival"
    )
    
    names(display_results) <- ifelse(
      names(display_results) %in% names(display_names),
      display_names[names(display_results)],
      names(display_results)
    )
    
    display_results
    
  })
  
}