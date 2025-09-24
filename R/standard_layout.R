#' @rdname standard_layout
#' @export
standard_layout2 <- function(output,
                             encoding = NULL,
                             forms = NULL,
                             pre_output = NULL,
                             post_output = NULL) {
  checkmate::assert_multi_class(output, c("shiny.tag", "shiny.tag.list", "html"))
  checkmate::assert_multi_class(encoding, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  # if encoding=NULL then forms is placed below output

  tag_output <- tags$div(
    class = "teal standard-layout",
    tags$div(class = "standard-layout-pre-output", pre_output),
    tags$div(class = "standard-layout-output", output),
    tags$div(class = "standard-layout-post-output", post_output)
  )


  output_panel <- tags$div(output, class = "teal standard-layout output-panel")
  out <- if (!is.null(encoding)) {
    tags$div(
      tags$nav(
        class = "navbar navbar-expand-lg navbar-light bg-light",
        tags$div(
          class = "navbar-nav d-flex flex-row flex-nowrap teal standard-layout encoding-panel",
          style = "width: 100%;",
          encoding
        )
      ),
      output_panel
    )
  } else {
    output_panel
  }


  bslib::page_fluid(out, class = "teal standard-layout-wrapper")
}

#' @export
teal_nav_item <- function(label = NULL, ...) {
  checkmate::assert_list(list(...), c("shiny.tag", "shiny.tag.list", "html", "character"))
  checkmate::assert_multi_class(label, c("shiny.tag", "shiny.tag.list", "html", "character"), null.ok = TRUE)
  tags$div(
    class = "nav-item",
    style = "min-width: 150px;",
    label,
    tagList(list(...))
  )
}

#' Dropdown badge
#'
#' Dropdown button in a form of a badge with `bg-primary` as default style
#' Clicking badge shows a dropdown containing any `HTML` element. Folded dropdown
#' doesn't trigger display output which means that items rendered using `render*`
#' will be recomputed only when dropdown is show.
#'
#' @param id (`character(1)`) shiny module's id
#' @param label (`shiny.tag`) Label displayed on a badge.
#' @param ... (`shiny.tag`) Content of a dropdown.
#' @export
badge_dropdown <- function(id, label, content) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$style("
      .badge-dropdown-wrapper:has(.shiny-validation-message),
      .badge-dropdown-wrapper:has(.shiny-output-error),
      .badge-dropdown-wrapper:has(* .shiny-validation-message),
      .badge-dropdown-wrapper:has(* .shiny-output-error) {
        border-color: red !important;
      }
      .badge-dropdown {
        padding: 0.25rem 0.5rem;
        font-size: 0.75rem;
        border-radius: 0.375rem;
        line-height: 1.2;
        min-width: auto;
        width: 130px;
        overflow-x: auto;
        white-space: nowrap;
        position: relative;
        padding-right: .5rem;
      }
      .badge-dropdown-label {
        display: block;
        max-width: calc(100% - .5rem);
        overflow-x: auto;
        white-space: nowrap;
        scrollbar-width: none;
        -ms-overflow-style: none;
        text-align: left;
      }
      .badge-dropdown-label::-webkit-scrollbar {
        display: none;
      }
      .badge-dropdown-icon {
        position: absolute;
        top: 50%;
        right: 0.5rem;
        transform: translateY(-50%);
        background: inherit;
        z-index: 1;
        pointer-events: none;
        opacity: 0;
        transition: opacity 0.2s ease;
      }
      .badge-dropdown:hover .badge-dropdown-icon {
        opacity: 1;
      }
    "),
    htmltools::tags$div(
      class = "badge-dropdown-wrapper",
      htmltools::tags$span(
        id = ns("summary_badge"),
        class = "badge bg-primary rounded-pill badge-dropdown",
        tags$span(class = "badge-dropdown-label", label),
        tags$span(class = "badge-dropdown-icon", bsicons::bs_icon("caret-down-fill")),
        onclick = sprintf(
          "
          var container = document.getElementById('%s');
          var summary = document.getElementById('%s');

          if(container.style.visibility === 'hidden' || container.style.visibility === '') {
            container.style.visibility = 'visible';
            container.style.opacity = '1';
            container.style.pointerEvents = 'auto';
            $(container).trigger('shown');
            Shiny.bindAll(container);


            // Add click outside handler
            setTimeout(function() {
              function handleClickOutside(event) {
                if (!container.contains(event.target) && !summary.contains(event.target)) {
                  container.style.visibility = 'hidden';
                  container.style.opacity = '0';
                  container.style.pointerEvents = 'none';
                  $(container).trigger('hidden');
                  document.removeEventListener('click', handleClickOutside);
                }
              }
              document.addEventListener('click', handleClickOutside);
            }, 10);
          } else {
            container.style.visibility = 'hidden';
            container.style.opacity = '0';
            container.style.pointerEvents = 'none';
            $(container).trigger('hidden');
          }
        ",
          ns("inputs_container"),
          ns("summary_badge")
        )
      ),
      htmltools::tags$div(
        content,
        id = ns("inputs_container"),
        style = "visibility: hidden; opacity: 0; pointer-events: none; position: absolute; background: white; border: 1px solid #ccc; border-radius: 4px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); padding: 10px; z-index: 1000; min-width: 200px; transition: opacity 0.2s ease;",
      )
    )
  )
}

# this one uses bootstrap but doesn't work for some reason
.badge_dropdown <- function(id, label, ...) {
  checkmate::assert_list(list(...), c("shiny.tag", "shiny.tag.list", "html", "character"))
  ns <- shiny::NS(id)

  btn_id <- ns("dropdown_menu")

  htmltools::tagList(
    htmltools::tags$style("
      .badge-dropdown:has(~ div .shiny-validation-message) {
        border-color: red !important;
      }
      .badge-dropdown .btn {
        padding: 0.25rem 0.5rem;
        font-size: 0.75rem;
        border-radius: 0.375rem;
        line-height: 1.2;
        min-width: auto;
      }
      .badge-dropdown .btn::after {
        margin-left: 0.25rem;
        vertical-align: 0.1em;
      }
    "),
    tags$div(
      class = "dropdown badge-dropdown",
      tags$a(
        id = btn_id,
        class = "btn btn-primary btn-sm dropdown-toggle",
        href = "#",
        role = "button",
        `data-bs-toggle` = "dropdown",
        `data-toggle` = "dropdown",
        `aria-haspopup` = TRUE,
        `aria-expanded` = FALSE,
        label
      ),
      tags$div(
        class = "dropdown-menu",
        `aria-labelledby` = btn_id,
        rlang::list2(...)
      )
    )
  )
}
