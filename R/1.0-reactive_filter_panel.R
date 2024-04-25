reactive_filtered_data <- function(id, filter, data) {
  moduleServer(id, function(input, output, session) {
    eventReactive(data(), {
      filtered_data <- teal_data_to_filtered_data(data())
      teal.slice::set_filter_state(filtered_data, filter)
      filtered_data
    })
  })
}
