#' This function will be called when the user invokes
#' the New Project wizard using the project template defined in the template file
#' at:
#'
#' inst/rstudio/templates/project/teal_application.dcf
#'
#' It is exported so that projects can be created outside of the RStudio IDE
#'
#' @export
project_skeleton <- function(path, use_cdisc = TRUE) {

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  contents <- if (use_cdisc) {
    '
  library(teal)
  library(scda)

  adsl <- scda::synthetic_cdisc_data("latest")$adsl
  adtte <- scda::synthetic_cdisc_data("latest")$adtte

  # create data
  data <- cdisc_data(
    cdisc_dataset("ADSL", adsl),
    cdisc_dataset("ADTTE", adtte)
  )'
  } else {
    '
  library(teal)

  # create data
  data <- teal_data(
    dataset("IRIS", iris),
    dataset("MTCARS", mtcars)
  )'
  }

  contents <- paste(contents,
    "# create modules
  modules <- modules(
    example_module()
  )

  # create app
  app <- init(
    data = data,
    modules = modules
  )

  # run
  shinyApp(app$ui, app$server)
  ",
    sep = "\n"
  )

  contents <- styler::style_text(contents)

  # write to index file
  writeLines(contents, con = file.path(path, "app.R"))
}
