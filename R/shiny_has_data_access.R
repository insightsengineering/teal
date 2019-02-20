#' @noRd

shiny_has_data_access <- function(path) {
  
  has_access <- file.access(path, 4) == 0
  
  if (!has_access) {
    shinyApp(
      ui = function() {
        fixedPage(
          div(
            class="jumbotron",
            tags$h1("No Data Access"),
            tags$p("You do not have read access to the atezo in BCE cdt7722i!",
                   tags$a(href="Please submit ticket at https://biohelp.roche.com/bioforms", "biohelp ticket"),
                   "or contact", 
                   tags$a(href="mailto:adrian.waddell@roche.com", "Adrian Waddell"),
                   "to get further instructions.")          
          )
        )
      }, 
      server = function(input, output) {}
    )
  } else {
    NULL
  }
}
