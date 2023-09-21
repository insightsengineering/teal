titles <- list(

  ## Blueprint
  blueprint_index =
    "Technical Blueprint",
  blueprint_intro =
    "Introduction",
  blueprint_actors =
    "Actors",
  blueprint_dataflow =
    "Dataflow",
  blueprint_products_map =
    "Products Map",
  blueprint_qenv =
    "`qenv`",
  blueprint_filter_panel =
    "Filter Panel",
  blueprint_ddl =
    "Delayed Data Loading (DDL)",
  blueprint_module_encapsulation =
    "Module Encapsulation"
)

title <- function(id) {
  out <- titles[[id]]

  if (is.null(out)) {
    stop(sprintf("`id` '%s' doesn't exist.", id))
  }

  out
}
