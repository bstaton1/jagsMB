#' Drop Leading Whitespace
#'
#' Shortcut for `stringr::str_remove(x, "^\\s+")`
#'
#' @param x Character string that has leading white space to be removed
#' @param n number of spaces to remove, default is `NULL` to remove all
#' @note For internal use only, users need not concern
#'   themselves with this function

drop_lws = function(x, n = NULL) {

  # build pattern to remove
  # either a number of spaces to remove or all spaces
  if (is.null(n)) {
    pattern = "^\\s+"
  } else {
    pattern = paste(c("^", rep("\\s", n)), collapse = "")
  }

  # return with spaces removed
  stringr::str_remove(x, pattern)
}

#' Drop Trailing Whitespace
#'
#' Shortcut for `stringr::str_remove(x, "\\s+$")`
#'
#' @param x Character string that has trailing white space to be removed
#' @note For internal use only, users need not concern
#'   themselves with this function

drop_tws = function(x) {
  stringr::str_remove(x, "\\s+$")
}

#' Check `FUN` Argument
#'
#' Standardized check for function
#'
#' @inheritParams model_write
#' @details Simply checks if `is.function(FUN)`, and if `FALSE`,
#'   returns an error
#' @note For internal use only, users need not concern
#'   themselves with this function

check_FUN = function(FUN) {
  if (!is.function(FUN) & !is.null(FUN)) stop("'FUN' must be a function object")
}

#' Check `model_file` Argument
#'
#' Standardized check for JAGS model file names
#'
#' @param model_file Name of file containing JAGS model code
#' @details Currently simply checks if is `NULL`,
#'   but more informative checks will likely be added in future versions
#'
check_model_file = function(model_file = NULL) {

  if (is.null(model_file)) stop ("'model_file' cannot be NULL")

}

