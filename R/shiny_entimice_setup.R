#' Create page with SAICE setup
#' 
#' TODO
#' 
#' 
#' @param app_url TODO
#' 
#' @importFrom shiny shinyApp
#' @export
#' 
#' @examples 
#' \dontrun{
#' shiny_entimice_setup()
#' }
#' 
shiny_entimice_setup <- function(app_url = NULL) {
  
  if (!dir.exists("~/key_pass_sso")) {
    
    shinyApp(
      ui = function() {
        fixedPage(
          div(
            class="jumbotron",
            tags$h1("Setup User Access for entimICE"),
            tags$p("Please enter your username and password in order to initialize your entimice access.",
                   "If you don't trust this interface follow the instructions on",
                   tags$a(href = "", "SAICE user manual")),
            div(
              textInput("user", "User Name"),
              passwordInput("pass", "Password"),
              actionButton("submit", "submit")
            )
          )
        )
      }, 
      server = function(input, output) {
        
        observeEvent(input$submit, {
          
          
          # set up entimice ...
          
          
          # invalidateLater(...)
          # reload page
        }) 
      }
    )
    
  } else {
    NULL
  }
  
}