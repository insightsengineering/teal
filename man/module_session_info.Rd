% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_session_info.R
\name{module_session_info}
\alias{module_session_info}
\alias{ui_session_info}
\alias{srv_session_info}
\title{\code{teal} user session info module}
\usage{
ui_session_info(id)

srv_session_info(id)
}
\arguments{
\item{id}{(\code{character(1)}) \code{shiny} module instance id.}
}
\value{
\code{NULL} invisibly
}
\description{
Module to display the user session info popup and to download a lockfile. Module is included
when running \code{\link[=init]{init()}} but skipped when using \code{\link{module_teal}}. Please be aware that session info
contains R session information, so multiple module's calls will share the same information.
}
\examples{
ui <- fluidPage(
  ui_session_info("session_info")
)

server <- function(input, output, session) {
  srv_session_info("session_info")
}

if (interactive()) {
  shinyApp(ui, server)
}

}
\section{Examples in Shinylive}{
\describe{
  \item{example-1}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEracxUurmAJgAUoAczjsB06eYD6AZzg+fTQgvISUiNzA-AKCQiDC+MH4IJIE-RilGS2U1DS0hVBVSXGkiQoKi6SjAkh0Qd0r0339q4NDwhKqYtoSkgF9TQSVpdiFyUXEpbVr6nwALIVYAQXR2c2K0jL6BMF6AXSA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqagSgB0ItMnGYFStAG5wABAB4AtNIBmAVwhjaJdj2kAVLAFUAogIEracxUurmAJgAUoAczjsB06eYD6AZzg+fTQgvISUiNzA-AKCQiDC+MH4IJIE-RilGS2U1DS0hVBVSXGkiQoKi6SjAkh0Qd0r0339q4NDwhKqYtoSkgF9TQSVpdiFyUXEpbVr6nwALIVYAQXR2c2K0jL6BMF6AXSA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

