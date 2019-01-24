#' Creates page to collect username/password to pass to SAICE during entimICE connection
#' 
#' Helpful for shiny app on BEE server that requires entimICE data. When developing and testing shiny apps locally, please do not run shiny_entimice_setup(). It is best to add when ready to publish.
#' 
#' @author Jennifer Li \email{lij201@gene.com}
#' 
#' @references \url{https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' 
#' @param url string of the url link to the main shiny app. The defaul value is blank, meaning the url will be automatically determined by parsing the directory of main shiny app.
#' 
#' @return The success or failure of entimICE connection, and automatically redirect to the main shiny app page.
#' 
#' @importFrom shiny shinyApp
#' @export
#' 
#' @examples 
#' \dontrun
#' {
#' #install the branch that enables interactive password entering in shiny
#' #devtools::install_github(
#repo = 'Rpackages/SAICE', 
#host = 'https://github.roche.com/api/v3',
#ref = "shiny_passwordInput_SAICE",
#force = TRUE
#)
#' 
#' shiny_entimice_setup()
#' shiny_entimice_setup("shiny.roche.com/drug/ro5541267/go29537/SREP/")
#' # data_path <- "root/clinical_studies/RO5541267/CDT30018/GO29437/data_analysis/Final_OS/qa/outdata_vad"
#' # ASL <- SAICE::get_entimice(file.path(data_path, "aslpd.sas7bdat"))
#' # vads_list = read_entimice(data_path)
#' # list2env(vads_list, envir = environment())
#' # ARS <- ars
#' # ATE <- ate
#' }
#' 

shiny_entimice_setup <- function(url="") {
  start("shiny_entimice_setup")
  if (!dir.exists("~/key_pass_sso")) {
    library(SAICE)
    
    #get the url of current app by looking for patterns in the directory
    wd <- getwd()
    if (grepl('\\/srv\\/shiny-server',wd,perl=T)) {
      link <- gsub('\\/srv\\/shiny\\-server', "shiny.roche.com", wd)
    } else {
      link <- ""
    }
    
    if (url==""){
      url <- link
    }
    
    app <- shinyApp(
      ui = function() {
        fixedPage(
          div(id="login",
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
          ),
          uiOutput("reload")
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
          entim_connection <<- check_credential()
          if(check_credential()==1){
            showNotification("Your UNIX ID or Password is incorrect, please try again", type = "error")
            updateTextInput(session, "user", value = "")
            updateTextInput(session, "pass", value = "")
          }
          else{
            
            removeUI(
              selector = "div#login"
            )
            output$reload<-renderUI({
              tags$p("entimICE connected successfully!",
                     tags$a(href = url, "Click to reload."))
            })
          }
        })
      }
    )
  }  
  return(app)
} 