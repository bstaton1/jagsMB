% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-fns.R
\name{model_build}
\alias{model_build}
\title{Combine Components into One Model}
\usage{
model_build(FUN_list)
}
\arguments{
\item{FUN_list}{List object where each element stores a function
(containing a JAGS model component) to be combined with other
model components.}
}
\value{
Function with the function bodies contained in the elements of
\code{FUN_list} combined into the body of one function.
}
\description{
Extracts the body from multiple functions (which store individual
model components), combines them, and returns the combined function.
}
