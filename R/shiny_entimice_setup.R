#' Create page to collect username/password to pass to SAICE during entimICE connection
#' 
#' Helpful for shiny app on BEE server that requires entimICE data
#' 
#' @param
#' 
#' @importFrom shiny shinyApp
#' @export
#' 
#' @examples 
#' \dontrun{
#' shiny_entimice_setup()
#' }
#' 


shiny_entimice_setup <- function() {
  library(SAICE)
  
  shinyApp(
    ui = function() {
      fixedPage(
        div(
          class="jumbotron",
          tags$h1("Setup User Access for entimICE"),
          tags$p("Please enter your username and password in order to initialize your entimice access.",
                 "If you don't trust this interface follow the instructions on",
                 tags$a(href = "https://github.roche.com/Rpackages/SAICE", "SAICE user manual")),
          div(
            textInput("user", "User Name"),
            passwordInput("pass", "Password"),
            actionButton("submit", "submit")
          )
        )
      )
    }, 
    server = function(input, output, session) {
      
      check_credential <- eventReactive(input$submit, withProgress(message = "Connecting to entimICE", value = 0.2, {
        req(input$user, input$pass)
        get_pass(user_name = input$user, password = input$pass)
        incProgress(0.3)
        start_connection <- initialize_connection(user_name = input$user , entimice_env = "PROD")
        # connection_status <- capture.output(initialize_connection(entimice_env = "PROD"))
        incProgress(0.4)
        check_credentials <- start_connection$exit_status
        return(check_credentials)
      }))
      
      observeEvent(input$submit, {
        
        if(check_credential()==1){
          showNotification("Your UNIX ID or Password is incorrect, please try again", type = "error")
          updateTextInput(session, "user", value = "")
          updateTextInput(session, "pass", value = "")
        }
        else{
          next
        }
      })
    }
  )
  
} 
