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

\item{server_args}{(named \code{list})
Arguments passed to \code{\link[teal.reporter:reporter_previewer]{teal.reporter::reporter_previewer_srv()}}.}
}
\value{
\code{teal_module} (extended with \code{teal_module_previewer} class) containing the \code{teal.reporter} previewer functionality.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function wraps \code{\link[teal.reporter:reporter_previewer]{teal.reporter::reporter_previewer_ui()}} and
\code{\link[teal.reporter:reporter_previewer]{teal.reporter::reporter_previewer_srv()}} into a \code{teal_module} to be
used in \code{teal} applications.

If you are creating a \code{teal} application using \code{\link[=init]{init()}} then this
module will be added to your application automatically if any of your \code{teal_modules}
support report generation.
}
