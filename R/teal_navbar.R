#' Navbar in teal
#'
#' Creates a navigation bar with consistent styling
#' - `.teal_navbar` creates a container for modules navigation in `teal`
#' @param nav_items (`tagList`) list of elements (e.g. buttons, dropdowns) which will be placed in
#'  the navbar.
#' @param tab_content (`tagList`) list of panels which will be displayed if respective navigation
#'  button will be clicked.
#'
#'
.teal_navbar <- function(id, nav_items, tab_content) {
  # todo: possible determine active item here?
  #       - we could run through nav_items and mark the first one as active (in page_navbar arg is named `selected`)
  #       - then we can find a tab_content which has the same id
  tags$div(
    class = "teal-modules-wrapper",
    htmltools::htmlDependency(
      name = "module-navigation",
      version = utils::packageVersion("teal"),
      package = "teal",
      src = "module-navigation",
      stylesheet = "module-navigation.css"
    ),
    tags$ul(
      id = id,
      style = "align-items: center; gap: 1em; font-size: large;",
      class = "nav shiny-tab-input", # to mimic nav and mimic tabsetPanel
      `data-tabsetid` = "test",
      nav_items
    ),
    tags$div(class = "tab-content", tab_content)
  )
}

.teal_navbar_menu <- function(..., id = NULL, label = NULL, class = NULL, icon = NULL) {
  tags$div(
    class = "dropdown nav-item-custom",
    .dropdown_button(
      id = id,
      label = label,
      icon = icon
    ),
    tags$div(
      class = "dropdown-menu",
      tags$ul(class = class, !!!list2(...))
    )
  )
}
