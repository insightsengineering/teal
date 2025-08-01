% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{modules_slot}
\alias{modules_slot}
\title{Retrieve slot from \code{teal_modules}}
\usage{
modules_slot(modules, slot)
}
\arguments{
\item{modules}{(\code{teal_modules})}

\item{slot}{(\code{character(1)})}
}
\value{
A \code{list} containing the \code{slot} of the modules.
If the modules are nested, the function returns a nested \code{list} of values.
}
\description{
Retrieve slot from \code{teal_modules}
}
\keyword{internal}
