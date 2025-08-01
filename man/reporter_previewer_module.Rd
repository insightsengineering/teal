% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporter_previewer_module.R
\name{reporter_previewer_module}
\alias{reporter_previewer_module}
\title{Create a \code{teal} module for previewing a report}
\usage{
reporter_previewer_module(label = "Report previewer", server_args = list())
}
\arguments{
\item{label}{(\code{character(1)}) Label shown in the navigation item for the module or module group.
For \code{modules()} defaults to \code{"root"}. See \code{Details}.}

\item{server_args}{(named \code{list}) Arguments will overwrite the default \code{teal.reporter} options
described in the description.}
}
\value{
\code{teal_module} (extended with \code{teal_module_previewer} class) containing the \code{teal.reporter} previewer functionality.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function controls the appearance of the drop-down menu for the reporter.
It is now deprecated in favor of the options:
\itemize{
\item \code{teal.reporter.nav_buttons = c("preview", "load", "download", "reset")} to control which
buttons will be displayed in the drop-down.
\item \code{teal.reporter.rmd_outputs}: passed to \code{\link[teal.reporter:download_report_button]{teal.reporter::download_report_button_srv()}}
\item \code{teal.reporter.rmd_yaml_args}: passed to \code{\link[teal.reporter:download_report_button]{teal.reporter::download_report_button_srv()}}
\item \code{teal.reporter.knitr_global}: passed to \code{\link[teal.reporter:download_report_button]{teal.reporter::download_report_button_srv()}}
}
}
