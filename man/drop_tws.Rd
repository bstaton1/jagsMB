% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/no-export-fns.R
\name{drop_tws}
\alias{drop_tws}
\title{Drop Trailing Whitespace}
\usage{
drop_tws(x)
}
\arguments{
\item{x}{Character string that has trailing white space to be removed}
}
\description{
Shortcut for \code{stringr::str_remove(x, "\\\\s+$")}
}
\note{
For internal use only, users need not concern
themselves with this function
}
