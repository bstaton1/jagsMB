% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-fns.R
\name{model_lines}
\alias{model_lines}
\title{View Lines from a JAGS Model File}
\usage{
model_lines(
  model_file = jagsMB_opts("model_file"),
  lines = NULL,
  show_labels = TRUE,
  keep_first_last = TRUE
)
}
\arguments{
\item{model_file}{Name of file containing JAGS model code;
defaults to \code{jagsMB_opts("model_file")}.}

\item{lines}{Numeric vector specifying the line numbers to display.}

\item{show_labels}{Logical flag indicating whether to display line
numbers with the output (defaults to \code{TRUE}).}

\item{keep_first_last}{Logical flag indicating whether to display
the first and last line in the model code.
These lines purely show \code{"model {"} and \code{"} # END OF MODEL"},
so sometimes it may be desirable to omit them.}
}
\description{
Print specific lines from a JAGS model file
to the console. Enables quick printing
for Rmd explanations or viewing line numbers
referenced in syntax errors return by JAGS.
}
